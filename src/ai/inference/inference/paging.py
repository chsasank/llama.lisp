from typing_extensions import ValuesView
import torch
from inference.config import device
import math
import flashinfer
import time


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
        mem_sizes = torch.Tensor([self.pid_mem_sizes[pid] for pid in sorted_pids]).int()
        last_page_lens = (mem_sizes % self.page_size).to(device)
        last_page_lens = torch.where(
            (mem_sizes > 0).to(device) & (last_page_lens == 0),
            torch.full_like(last_page_lens, self.page_size),
            last_page_lens,
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
            if self.pid_mem_sizes[pid] > 0 and last_page_len == 0:
                last_page_len = self.page_size
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


class VirtualMemoryPaged:
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

    def free(self, pid):
        self.page_table.free(pid)

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


class VirtualMemorySDPA:
    def __init__(self, max_num_pages, page_size, num_kv_heads, head_dim, num_layers):
        self.pid_kv_cache = {}

    def prefill(self, pid, model_kv_cache):
        assert model_kv_cache[0][0].shape[0] == 1
        self.pid_kv_cache[pid] = model_kv_cache

    def get_decode_closures(self):
        sorted_pids = sorted(self.pid_kv_cache.keys())

        def _cache_updater(layer_id, append_kv):

            time_ = time.time()
            for bid, pid in enumerate(sorted_pids):
                # keys
                self.pid_kv_cache[pid][layer_id] = (
                    torch.cat(
                        [
                            self.pid_kv_cache[pid][layer_id][0],
                            append_kv[0][bid].unsqueeze(0),
                        ],
                        dim=2,
                    ),
                    torch.cat(
                        [
                            self.pid_kv_cache[pid][layer_id][1],
                            append_kv[1][bid].unsqueeze(0),
                        ],
                        dim=2,
                    ),
                )
            # print("cache updater time: ", 1000 * (time.time() - time_), "seconds")

        def _attn_(layer_id, q):
            num_seqs = q.shape[2]
            assert num_seqs == 1, "decode step assumes only 1 token decoded per batch"

            sorted_pids = sorted(self.pid_kv_cache.keys())
            attn = []
            keys = []
            values = []

            for bid, pid in enumerate(sorted_pids):
                key = self.pid_kv_cache[pid][layer_id][0]
                value = self.pid_kv_cache[pid][layer_id][1]
                assert key.shape[0] == 1

                bid_attn_output = torch.nn.functional.scaled_dot_product_attention(
                    q[bid].unsqueeze(0), key, value
                )
                attn.append(bid_attn_output)
                keys.append(key[0])
                values.append(value[0])

            attn_output_ = torch.cat(attn, dim=0)

            return attn_output_

        def _attn(layer_id, q):
            time_0 = time.time()
            num_seqs = q.shape[2]
            assert num_seqs == 1, "decode step assumes only 1 token decoded per batch"

            sorted_pids = sorted(self.pid_kv_cache.keys())

            keys_list = [
                self.pid_kv_cache[pid][layer_id][0].squeeze(0) for pid in sorted_pids
            ]
            values_list = [
                self.pid_kv_cache[pid][layer_id][1].squeeze(0) for pid in sorted_pids
            ]
            # each: (num_heads, seq_len_i, head_dim)

            seq_lens = [k.shape[1] for k in keys_list]
            max_len = max(seq_lens)
            batch = len(sorted_pids)
            num_heads, _, head_dim = keys_list[0].shape

            # Pad k/v to (batch, num_heads, max_len, head_dim)
            keys_padded = torch.zeros(
                batch, num_heads, max_len, head_dim, device=q.device, dtype=q.dtype
            )
            values_padded = torch.zeros(
                batch, num_heads, max_len, head_dim, device=q.device, dtype=q.dtype
            )
            for i, (k, v, slen) in enumerate(zip(keys_list, values_list, seq_lens)):
                keys_padded[i, :, :slen, :] = k
                values_padded[i, :, :slen, :] = v

            mask = torch.zeros(batch, 1, 1, max_len, device=q.device, dtype=torch.bool)
            for i, slen in enumerate(seq_lens):
                mask[i, :, :, :slen] = True
            additive_mask = torch.zeros(
                batch, 1, 1, max_len, device=q.device, dtype=q.dtype
            )
            additive_mask.masked_fill_(~mask, float("-inf"))

            time_ = time.time()
            attn_output = torch.nn.functional.scaled_dot_product_attention(
                q, keys_padded, values_padded, attn_mask=additive_mask
            )

            # attn_output_fp16 = torch.nn.functional.scaled_dot_product_attention(
            #    q.half(), keys_padded.half(), values_padded.half(), attn_mask=additive_mask.half()
            # )
            # print("attention time: ", 1000 * (time.time() - time_), "ms")
            # print("total calculation time: ", 1000 * (time.time() - time_0), "ms")

            # attn_output_fp16 = attn_output_fp16 + torch.randn_like(attn_output_fp16) * 1e-1
            # print((attn_output - attn_output_fp16).abs().max())

            return attn_output

        return _cache_updater, _attn

    def free(self, pid):
        del self.pid_kv_cache[pid]


class VirtualMemoryCompare:
    def __init__(self, max_num_pages, page_size, num_kv_heads, head_dim, num_layers):
        self.vm_paged = VirtualMemoryPaged(
            max_num_pages, page_size, num_kv_heads, head_dim, num_layers
        )
        self.vm_sdpa = VirtualMemorySDPA(
            max_num_pages, page_size, num_kv_heads, head_dim, num_layers
        )

    def prefill(self, pid, model_kv_cache):
        self.vm_paged.prefill(pid, model_kv_cache)
        self.vm_sdpa.prefill(pid, model_kv_cache)

    def get_decode_closures(self):
        paged_cache_updater, paged_attn = self.vm_paged.get_decode_closures()
        sdpa_cache_updater, sdpa_attn = self.vm_sdpa.get_decode_closures()

        def _cache_updater(layer_id, append_kv):
            paged_cache_updater(layer_id, append_kv)
            sdpa_cache_updater(layer_id, append_kv)

        def _attn(layer_id, q):
            out_paged = paged_attn(layer_id, q)
            out_sdpa = sdpa_attn(layer_id, q)

            diff = (out_paged - out_sdpa).float()
            max_abs = diff.abs().max()

            if max_abs > 1e-2:
                print(f"max_abs={max_abs:.6g}")
            return out_paged

        return _cache_updater, _attn

    def free(self, pid):
        self.vm_paged.free(pid)
        self.vm_sdpa.free(pid)


def VirtualMemory(
    max_num_pages, page_size, num_kv_heads, head_dim, num_layers, type="sdpa"
):
    if type == "paged":
        return VirtualMemoryPaged(
            max_num_pages, page_size, num_kv_heads, head_dim, num_layers
        )
    elif type == "sdpa":
        return VirtualMemorySDPA(
            max_num_pages, page_size, num_kv_heads, head_dim, num_layers
        )
    elif type == "compare":
        return VirtualMemoryCompare(
            max_num_pages, page_size, num_kv_heads, head_dim, num_layers
        )
