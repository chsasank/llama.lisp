import torch
from inference.config import device
import math
import flashinfer


class PageTable:
    def __init__(self, page_size):
        # dictionary from pid -> pages
        self.page_size = page_size

        # pid -> list of pages
        self.pid_page_table = {}
        # pid -> total memory size
        self.pid_mem_sizes = {}

    def allocate(self, pid, mem_size):
        try:
            current_mem_size = self.pid_mem_sizes[pid]
        except KeyError:
            current_mem_size = 0
            self.pid_mem_sizes[pid] = 0
            self.pid_page_table[pid] = []

        new_mem_size = current_mem_size + mem_size
        self.pid_mem_sizes[pid] = new_mem_size

        num_existing_pages = len(self.pid_page_table[pid])
        num_new_pages = math.ceil(new_mem_size / self.page_size)
        num_pages_to_allocate = num_new_pages - num_existing_pages

        all_occupied_pages = {
            p for pages in self.pid_page_table.values() for p in pages
        }
        if len(all_occupied_pages) == 0:
            # no pages used so far
            self.pid_page_table[pid].extend(range(0, num_pages_to_allocate))
            return
        else:
            first_occupied_page = min(all_occupied_pages)
            # look for space on the left
            if first_occupied_page > 0:
                if first_occupied_page >= num_pages_to_allocate:
                    self.pid_page_table[pid].extend(range(0, num_pages_to_allocate))
                    return
                else:
                    self.pid_page_table[pid].extend(range(0, first_occupied_page))
                    num_pages_to_allocate = num_pages_to_allocate - first_occupied_page

            # look for gaps
            last_occupied_page = max(all_occupied_pages)
            gaps = sorted(
                set(range(first_occupied_page, last_occupied_page + 1))
                - all_occupied_pages
            )
            if len(gaps) >= num_pages_to_allocate:
                self.pid_page_table[pid].extend(gaps[:num_pages_to_allocate])
                return
            else:
                self.pid_page_table[pid].extend(gaps)
                num_pages_to_allocate = num_pages_to_allocate - len(gaps)

            # look for space on the right
            self.pid_page_table[pid].extend(
                range(
                    last_occupied_page + 1,
                    last_occupied_page + 1 + num_pages_to_allocate,
                )
            )
            return

    def free(self, pid):
        del self.pid_page_table[pid]
        del self.pid_mem_sizes[pid]

    def print_state(self):
        print(
            "pid_page_table:", self.pid_page_table, "pid_mem_sizes:", self.pid_mem_sizes
        )

    def convert_to_flashinfer(self):
        sorted_pids = sorted(self.pid_page_table.keys())
        page_indices = (
            torch.Tensor(
                [page for pid in sorted_pids for page in self.pid_page_table[pid]]
            )
            .int()
            .to(device)
        )
        page_indptr = (
            torch.cumsum(
                torch.Tensor(
                    [0] + [len(self.pid_page_table[pid]) for pid in sorted_pids]
                ),
                dim=0,
            )
            .int()
            .to(device)
        )
        last_page_lens = (
            torch.Tensor([self.pid_mem_sizes[pid] for pid in sorted_pids])
            .int()
            .to(device)
            % self.page_size
        )

        return page_indices, page_indptr, last_page_lens

    def convert_to_sdpa_ragged_attn_mask(self, max_pages):
        # attn_mask shape -> (num_pids, max_pages * page_size)
        sorted_pids = sorted(self.pid_page_table.keys())
        attn_mask = torch.zeros(
            (len(sorted_pids), max_pages * self.page_size), dtype=torch.bool
        )
        for bid, pid in enumerate(sorted_pids):
            for page in self.pid_page_table[pid]:
                attn_mask[bid, page * self.page_size : (page + 1) * self.page_size] = 1

            # deal with last page carefully
            last_page = self.pid_page_table[pid][-1]
            last_page_len = self.pid_mem_sizes[pid] % self.page_size
            attn_mask[
                bid,
                last_page * self.page_size : last_page * self.page_size + last_page_len,
            ] = 1
            attn_mask[
                bid,
                last_page * self.page_size
                + last_page_len : (last_page + 1) * self.page_size,
            ] = 0

        return attn_mask.to(device)


class VirtualMemory:
    def __init__(self, max_num_pages, page_size, num_kv_heads, head_dim, num_layers):
        self.paged_model_kv_cache = [
            torch.zeros(
                (max_num_pages, 2, page_size, num_kv_heads, head_dim),
                dtype=torch.float16,
                device=device,
            )
            for _ in range(num_layers)
        ]
        self.page_table = PageTable(page_size=page_size)
        self.num_kv_heads = num_kv_heads
        self.num_qo_heads = num_kv_heads  # no gqa for now
        self.head_dim = head_dim
        self.page_size = page_size
        self.num_layers = num_layers

        workspace_buffer = torch.zeros(
            128 * 1024 * 1024, dtype=torch.uint8, device=device
        )
        self.decode_wrapper = flashinfer.BatchDecodeWithPagedKVCacheWrapper(
            workspace_buffer
        )

    def prefill(self, pid, model_kv_cache):
        n_seq = model_kv_cache[0][0].shape[2]
        assert model_kv_cache[0][0].shape[0] == 1

        self.page_table.allocate(pid=pid, mem_size=n_seq)
        sorted_pids = sorted(self.page_table.pid_mem_sizes.keys())
        bid = sorted_pids.index(pid)
        batch_indices = torch.full((n_seq,), bid, dtype=torch.int32, device=device)
        positions = torch.arange(n_seq, dtype=torch.int32, device=device)
        kv_indices, kv_indptr, kv_last_page_len = (
            self.page_table.convert_to_flashinfer()
        )

        for layer in range(self.num_layers):
            append_key = model_kv_cache[layer][0][0].transpose(0, 1).contiguous().half()
            append_value = (
                model_kv_cache[layer][1][0].transpose(0, 1).contiguous().half()
            )
            flashinfer.append_paged_kv_cache(
                append_key=append_key,
                append_value=append_value,
                batch_indices=batch_indices,
                positions=positions,
                paged_kv_cache=self.paged_model_kv_cache[layer],
                kv_indices=kv_indices,
                kv_indptr=kv_indptr,
                kv_last_page_len=kv_last_page_len,
            )

    def get_decode_closures(self):
        sorted_pids = sorted(self.page_table.pid_mem_sizes.keys())
        for pid in sorted_pids:
            self.page_table.allocate(pid=pid, mem_size=1)

        # we gotta insert at the last position
        positions = [self.page_table.pid_mem_sizes[pid] - 1 for pid in sorted_pids]
        positions = torch.Tensor(positions).int().to(device)

        batch_indices = torch.arange(len(sorted_pids), dtype=torch.int32, device=device)
        kv_indices, kv_indptr, kv_last_page_len = (
            self.page_table.convert_to_flashinfer()
        )
        self.decode_wrapper.plan(
            kv_indptr,
            kv_indices,
            kv_last_page_len,
            self.num_qo_heads,
            self.num_kv_heads,
            self.head_dim,
            self.page_size,
            data_type=torch.float16,
        )

        def _cache_updater(layer_id, append_kv):
            num_batches = append_kv[0].shape[0]
            num_seqs = append_kv[0].shape[2]
            assert num_batches == len(
                self.page_table.pid_mem_sizes
            ), "batch size doesn't match active sequences"
            assert num_seqs == 1, "decode step assumes only 1 token decoded per batch"
            append_key = append_kv[0][:, :, 0].contiguous().half()
            append_value = append_kv[1][:, :, 0].contiguous().half()
            flashinfer.append_paged_kv_cache(
                append_key=append_key,
                append_value=append_value,
                batch_indices=batch_indices,
                positions=positions,
                paged_kv_cache=self.paged_model_kv_cache[layer_id],
                kv_indices=kv_indices,
                kv_indptr=kv_indptr,
                kv_last_page_len=kv_last_page_len,
            )

        def _attn(layer_id, q):
            num_seqs = q.shape[2]
            assert num_seqs == 1, "decode step assumes only 1 token decoded per batch"
            q = q.squeeze(2)
            return self.decode_wrapper.run(
                q, self.paged_model_kv_cache[layer_id]
            ).unsqueeze(2)

        return _cache_updater, _attn
