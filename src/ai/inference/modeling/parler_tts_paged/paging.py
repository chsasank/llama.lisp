# PagedAttention for flex_attention: page table, reserve/assign/erase,
# convert_logical_block_mask, get_mask_mod / get_score_mod.
# Adapted from https://github.com/meta-pytorch/attention-gym

from typing import Optional, Union

import torch
from torch.nn.attention.flex_attention import (
    BlockMask,
    _identity,
    noop_mask,
)


def _cdiv(
    x: Union[int, float, torch.Tensor],
    multiple: Union[int, float, torch.Tensor],
) -> Union[int, torch.Tensor]:
    """Ceiling division."""
    if isinstance(x, torch.Tensor) or isinstance(multiple, torch.Tensor):
        return (x + multiple - 1) // multiple
    return (x + multiple - 1) // multiple


class PagedAttention:
    """
    PagedAttention supports flex attention inference with a large batch size.
    KV tensors with varying length are split into fixed-size blocks and cached
    in a compact physical cache via a page table.
    """

    def __init__(
        self,
        n_pages: int,
        page_size: int,
        max_batch_size: int,
        device: Union[str, torch.device] = "cuda",
    ):
        self.n_pages = n_pages
        self.page_size = page_size
        self.max_batch_size = max_batch_size
        self._device = device

        # page_table: [batch_idx, logical_page_idx] -> physical_page_idx
        self.page_table = torch.full(
            (max_batch_size, n_pages),
            -1,
            dtype=torch.int64,
            device=device,
        )
        # capacity[batch_idx] = allocated sequence length for that batch
        self.capacity = torch.zeros(max_batch_size, dtype=torch.int64, device=device)
        # empty_pages: list of physical page indices available for allocation
        self.empty_pages = list(range(n_pages - 1, -1, -1))
        # physical_to_logical: [batch_idx, physical_page_idx] -> logical_page_idx
        self.physical_to_logical = torch.full(
            (max_batch_size, n_pages),
            -1,
            dtype=torch.int64,
            device=device,
        )

    def reserve(
        self,
        batch_idx: Union[int, torch.Tensor],
        seq_len: Union[int, torch.Tensor],
    ) -> None:
        """Ensure capacity for batch_idx is at least seq_len."""
        if isinstance(batch_idx, int):
            batch_idx = torch.tensor(batch_idx, device=self._device)
        if isinstance(seq_len, int):
            seq_len = torch.tensor(seq_len, device=self._device)
        if batch_idx.dim() == 0:
            batch_idx = batch_idx.unsqueeze(0)
        if seq_len.dim() == 0:
            seq_len = seq_len.unsqueeze(0)
        # Support single batch index and single seq_len
        b = batch_idx.item() if batch_idx.numel() == 1 else batch_idx[0].item()
        s = seq_len.item() if seq_len.numel() == 1 else seq_len[0].item()
        if s <= self.capacity[b].item():
            return
        num_pages_to_allocate = _cdiv(s - self.capacity[b].item(), self.page_size)
        num_pages_to_allocate = (
            num_pages_to_allocate
            if isinstance(num_pages_to_allocate, int)
            else num_pages_to_allocate.item()
        )
        assert len(self.empty_pages) >= num_pages_to_allocate, (
            f"requested {num_pages_to_allocate} pages "
            f"but only {len(self.empty_pages)} empty"
        )
        start_page_idx = self.capacity[b].item() // self.page_size
        end_page_idx = start_page_idx + num_pages_to_allocate
        allocated_pages = self.empty_pages[-num_pages_to_allocate:]
        self.empty_pages = self.empty_pages[:-num_pages_to_allocate]
        allocated_pages_t = torch.tensor(
            allocated_pages, device=self._device, dtype=torch.int64
        )
        self.page_table[b, start_page_idx:end_page_idx] = allocated_pages_t
        self.physical_to_logical[b, allocated_pages_t] = torch.arange(
            start_page_idx, end_page_idx, device=self._device, dtype=torch.int64
        )
        self.capacity[b] = self.capacity[b].item() + num_pages_to_allocate * self.page_size

    def erase(self, batch_idx: Union[int, torch.Tensor]) -> None:
        """Free all pages for the given batch index."""
        if isinstance(batch_idx, int):
            b = batch_idx
        else:
            b = batch_idx.item() if batch_idx.numel() == 1 else batch_idx[0].item()
        allocated = self.page_table[b] != -1
        allocated_pages = self.page_table[b][allocated].tolist()
        self.capacity[b] = 0
        self.empty_pages.extend(allocated_pages)
        self.physical_to_logical[b, allocated_pages] = -1
        self.page_table[b] = -1

    def assign(
        self,
        batch_idx: torch.Tensor,
        input_pos: torch.Tensor,
        k_val: torch.Tensor,
        v_val: torch.Tensor,
        k_cache: torch.Tensor,
        v_cache: torch.Tensor,
    ) -> None:
        """Scatter k_val, v_val into physical k_cache, v_cache at (batch_idx, input_pos)."""
        if k_val.requires_grad:
            raise RuntimeError("k_val must not require gradient")
        B, H, S, K_D = k_val.shape
        V_D = v_val.shape[3]
        if B != batch_idx.shape[0]:
            raise RuntimeError(
                f"batch size mismatch: k_val B={B}, batch_idx len={batch_idx.shape[0]}"
            )
        if S != input_pos.shape[1]:
            raise RuntimeError(
                f"sequence length mismatch: k_val S={S}, input_pos S={input_pos.shape[1]}"
            )
        logical_block_idx = input_pos // self.page_size
        logical_block_offset = input_pos % self.page_size
        # page_table[batch_idx] -> (B, n_pages), logical_block_idx -> (B, S)
        physical_block_idx = torch.gather(
            self.page_table[batch_idx], 1, logical_block_idx.to(torch.int64)
        ).to(torch.int32)
        addr = (physical_block_idx * self.page_size + logical_block_offset).view(-1)
        k_val_flat = k_val.permute(1, 0, 2, 3).contiguous().view(1, H, B * S, K_D)
        v_val_flat = v_val.permute(1, 0, 2, 3).contiguous().view(1, H, B * S, V_D)
        k_cache[:, :, addr, :] = k_val_flat
        v_cache[:, :, addr, :] = v_val_flat

    def convert_logical_block_mask(
        self,
        block_mask: BlockMask,
        batch_idx: Optional[torch.Tensor] = None,
    ) -> BlockMask:
        """Convert logical block mask to physical using the page table."""
        B, H, ROWS, MAX_BLOCKS_IN_COL = block_mask.kv_indices.shape
        if block_mask.BLOCK_SIZE[1] != self.page_size:
            raise RuntimeError(
                f"block_mask column block size {block_mask.BLOCK_SIZE[1]} "
                f"must equal page_size {self.page_size}"
            )
        device = block_mask.kv_num_blocks.device
        if batch_idx is None:
            batch_idx = torch.arange(B, device=device)
        page_table = self.page_table[batch_idx]
        new_kv_num_blocks = block_mask.kv_num_blocks.clone()
        new_kv_indices = torch.zeros(
            (B, H, ROWS, self.n_pages), dtype=torch.int32, device=device
        )
        logical_flat = block_mask.kv_indices.view(B, -1).to(torch.int64)
        gathered = torch.gather(page_table, 1, logical_flat)
        new_kv_indices[:, :, :, :MAX_BLOCKS_IN_COL] = gathered.view(
            block_mask.kv_indices.shape
        ).to(torch.int32)
        new_full_kv_num_blocks = None
        new_full_kv_indices = None
        if block_mask.full_kv_num_blocks is not None:
            assert block_mask.full_kv_indices is not None
            new_full_kv_num_blocks = block_mask.full_kv_num_blocks.clone()
            new_full_kv_indices = torch.zeros(
                (B, H, ROWS, self.n_pages), dtype=torch.int32, device=device
            )
            full_flat = block_mask.full_kv_indices.view(B, -1).to(torch.int64)
            full_gathered = torch.gather(page_table, 1, full_flat)
            new_full_kv_indices[:, :, :, :MAX_BLOCKS_IN_COL] = full_gathered.view(
                block_mask.full_kv_indices.shape
            ).to(torch.int32)
        new_mask_mod = self.get_mask_mod(block_mask.mask_mod, batch_idx=batch_idx)
        seq_lengths = (block_mask.seq_lengths[0], self.n_pages * self.page_size)
        return BlockMask.from_kv_blocks(
            new_kv_num_blocks,
            new_kv_indices,
            new_full_kv_num_blocks,
            new_full_kv_indices,
            block_mask.BLOCK_SIZE,
            new_mask_mod,
            seq_lengths=seq_lengths,
        )

    def get_mask_mod(
        self,
        mask_mod: Optional[callable],
        batch_idx: Optional[torch.Tensor] = None,
    ) -> callable:
        """Wrap mask_mod to use physical KV indices via physical_to_logical mapping."""
        if mask_mod is None:
            mask_mod = noop_mask
        physical_to_logical = (
            self.physical_to_logical[batch_idx]
            if batch_idx is not None
            else self.physical_to_logical
        )

        def new_mask_mod(
            b: torch.Tensor,
            h: torch.Tensor,
            q_idx: torch.Tensor,
            physical_kv_idx: torch.Tensor,
        ):
            physical_kv_block = physical_kv_idx // self.page_size
            physical_kv_offset = physical_kv_idx % self.page_size
            logical_block_idx = physical_to_logical[b, physical_kv_block]
            logical_kv_idx = logical_block_idx * self.page_size + physical_kv_offset
            return torch.where(
                logical_block_idx >= 0, mask_mod(b, h, q_idx, logical_kv_idx), False
            )

        return new_mask_mod

    def get_score_mod(
        self,
        score_mod: Optional[callable],
        batch_idx: Optional[torch.Tensor] = None,
    ) -> callable:
        """Wrap score_mod to use physical KV indices via physical_to_logical mapping."""
        if score_mod is None:
            score_mod = _identity
        physical_to_logical = (
            self.physical_to_logical[batch_idx]
            if batch_idx is not None
            else self.physical_to_logical
        )

        def new_score_mod(
            score: torch.Tensor,
            b: torch.Tensor,
            h: torch.Tensor,
            q_idx: torch.Tensor,
            physical_kv_idx: torch.Tensor,
        ):
            physical_kv_block = physical_kv_idx // self.page_size
            physical_kv_offset = physical_kv_idx % self.page_size
            logical_block_idx = physical_to_logical[b, physical_kv_block]
            logical_kv_idx = logical_block_idx * self.page_size + physical_kv_offset
            return torch.where(
                logical_block_idx >= 0,
                score_mod(score, b, h, q_idx, logical_kv_idx),
                float("-inf"),
            )

        return new_score_mod


def materialize_kv_cache(
    paged_attention: PagedAttention,
    k_cache: torch.Tensor,
    v_cache: torch.Tensor,
    batch_idx: torch.Tensor,
    seq_len: int,
    device: torch.device,
) -> tuple:
    """
    Gather from physical k_cache, v_cache into (B, H, seq_len, D) using the page table.
    Returns (k_materialized, v_materialized) for test parity with non-paged cache format.
    """
    B = batch_idx.shape[0]
    H = k_cache.shape[1]
    D_k = k_cache.shape[3]
    D_v = v_cache.shape[3]
    page_size = paged_attention.page_size
    page_table = paged_attention.page_table[batch_idx]
    logical_block_idx = (
        torch.arange(seq_len, device=device, dtype=torch.int64)
        .unsqueeze(0)
        .expand(B, -1)
    )
    physical_block = torch.gather(page_table, 1, logical_block_idx)
    logical_offset = (
        torch.arange(seq_len, device=device, dtype=torch.int64)
        .unsqueeze(0)
        .expand(B, -1)
    )
    physical_offset = logical_offset % page_size
    addr = (physical_block * page_size + physical_offset).view(-1)
    k_flat = k_cache[0, :, addr, :]
    k_materialized = k_flat.view(H, B, seq_len, D_k).permute(1, 0, 2, 3)
    v_flat = v_cache[0, :, addr, :]
    v_materialized = v_flat.view(H, B, seq_len, D_v).permute(1, 0, 2, 3)
    return k_materialized, v_materialized
