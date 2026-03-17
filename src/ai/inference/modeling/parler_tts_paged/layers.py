import torch
import transformers
from transformers.models.musicgen.modeling_musicgen import (
    MusicgenSinusoidalPositionalEmbedding,
)

try:
    from torch.nn.attention.flex_attention import (
        create_block_mask,
        flex_attention,
    )
    FLEX_ATTENTION_AVAILABLE = True
except Exception:
    FLEX_ATTENTION_AVAILABLE = False
    _flex_attention = None

from paging import PagedAttention


class SinusoidalPositionalEmbedding(MusicgenSinusoidalPositionalEmbedding):
    def from_position_ids(self, position_ids):
        bs, seq_len = position_ids.shape
        flat_embs = self.weights.index_select(0, position_ids.view(-1))
        return flat_embs.reshape(bs, seq_len, -1)


class AttentionBlock(torch.nn.Module):
    """Standard attention with SDPA; used for cross-attention (encoder)."""

    def __init__(self, embed_dim, num_heads, bias=False):
        super().__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads

        self.k_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.v_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.q_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.out_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)

    def split_heads(self, x):
        batch_size, seq_len, _ = x.shape
        return x.view(batch_size, seq_len, self.num_heads, -1).transpose(1, 2)

    def merge_heads(self, x):
        batch_size, heads, seq_len, head_dim = x.shape
        return x.transpose(1, 2).reshape(batch_size, seq_len, heads * head_dim)

    def forward(
        self, hidden_states, key_value_states=None, kv_cache=None, attn_mask=None
    ):
        is_cross_attn = key_value_states is not None
        if key_value_states is None:
            key_value_states = hidden_states

        query = self.split_heads(self.q_proj(hidden_states))

        if is_cross_attn:
            if kv_cache is None:
                key = self.split_heads(self.k_proj(key_value_states))
                value = self.split_heads(self.v_proj(key_value_states))
            else:
                key, value = kv_cache
        else:
            key = self.split_heads(self.k_proj(key_value_states))
            value = self.split_heads(self.v_proj(key_value_states))
            if kv_cache is not None:
                past_key, past_value = kv_cache
                key = torch.cat([past_key, key], dim=2)
                value = torch.cat([past_value, value], dim=2)

        attn_output = torch.nn.functional.scaled_dot_product_attention(
            query, key, value, attn_mask
        )
        attn_output = self.merge_heads(attn_output)
        output = self.out_proj(attn_output)
        kv_cache = (key, value)
        return output, kv_cache


def _causal_score_mod(score, b, h, q_idx, kv_idx):
    return torch.where(q_idx >= kv_idx, score, float("-inf"))


def _get_score_mod_with_offset(causal_score_mod, offset: torch.Tensor):
    """Wrap causal score_mod so q_idx is offset by a per-batch tensor."""

    def score_mod(score, b, h, q_idx, kv_idx):
        # offset: (B,); b may be 0-dim tensor
        off = offset[b] if offset.dim() > 0 else offset
        q_off = q_idx + off
        return causal_score_mod(score, b, h, q_off, kv_idx)

    return score_mod


class PagedAttentionBlock(torch.nn.Module):
    """
    Self-attention that can run in paged mode (flex_attention + PagedAttention)
    or fallback mode (SDPA + (key, value) cache). Same q/k/v/out projections
    as AttentionBlock for weight compatibility.
    """

    def __init__(
        self,
        embed_dim,
        num_heads,
        bias=False,
        page_size=16,
        n_pages=256,
        max_batch_size=16,
        device=None,
    ):
        super().__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads
        self.head_dim = embed_dim // num_heads
        self.page_size = page_size
        self.n_pages = n_pages
        self.max_batch_size = max_batch_size
        self._device = device

        self.k_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.v_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.q_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)
        self.out_proj = torch.nn.Linear(embed_dim, embed_dim, bias=bias)

    def split_heads(self, x):
        batch_size, seq_len, _ = x.shape
        return x.view(batch_size, seq_len, self.num_heads, -1).transpose(1, 2)

    def merge_heads(self, x):
        batch_size, heads, seq_len, head_dim = x.shape
        return x.transpose(1, 2).reshape(batch_size, seq_len, heads * head_dim)

    def forward(
        self,
        hidden_states,
        batch_idx=None,
        input_pos=None,
        paged_attention=None,
        k_cache=None,
        v_cache=None,
        causal_offset=None,
        kv_cache=None,
        attn_mask=None,
    ):
        """
        If paged_attention is not None: use flex_attention + assign (paged path).
        Else: use SDPA + kv_cache (fallback, same as AttentionBlock).
        """
        B, S, _ = hidden_states.shape
        device = hidden_states.device
        q = self.split_heads(self.q_proj(hidden_states))
        k = self.split_heads(self.k_proj(hidden_states))
        v = self.split_heads(self.v_proj(hidden_states))

        if paged_attention is not None and FLEX_ATTENTION_AVAILABLE and k_cache is not None and v_cache is not None:
            # Paged path: assign and flex_attention
            paged_attention.assign(batch_idx, input_pos, k, v, k_cache, v_cache)
            q_len = S
            kv_len = (input_pos.max().item() + 1) if input_pos.numel() else 0
            if kv_len <= 0:
                kv_len = q_len
            q_blocks = (q_len + self.page_size - 1) // self.page_size
            kv_blocks = (kv_len + self.page_size - 1) // self.page_size
            Q_LEN = q_blocks * self.page_size
            KV_LEN = kv_blocks * self.page_size
            logical_block_mask = create_block_mask(
                lambda b, h, q_idx, kv_idx: q_idx >= kv_idx,
                B,
                self.num_heads,
                Q_LEN,
                KV_LEN,
                device=device,
                BLOCK_SIZE=self.page_size,
            )
            converted_block_mask = paged_attention.convert_logical_block_mask(
                logical_block_mask, batch_idx
            )
            if causal_offset is not None:
                score_mod_with_offset = _get_score_mod_with_offset(
                    _causal_score_mod, causal_offset
                )
                converted_score_mod = paged_attention.get_score_mod(
                    score_mod_with_offset, batch_idx
                )
            else:
                converted_score_mod = paged_attention.get_score_mod(
                    _causal_score_mod, batch_idx
                )
            if q.shape[2] < Q_LEN:
                q_padded = torch.nn.functional.pad(
                    q, (0, 0, 0, Q_LEN - q.shape[2]), value=0.0
                )
            else:
                q_padded = q
            attn_output = flex_attention(
                q_padded, k_cache, v_cache,
                block_mask=converted_block_mask, score_mod=converted_score_mod
            )
            attn_output = attn_output[:, :, :S, :]
            out = self.merge_heads(attn_output)
            out = self.out_proj(out)
            return out, None
        else:
            # Fallback: SDPA with (key, value) cache
            if kv_cache is not None:
                past_key, past_value = kv_cache
                k = torch.cat([past_key, k], dim=2)
                v = torch.cat([past_value, v], dim=2)
            attn_output = torch.nn.functional.scaled_dot_product_attention(
                q, k, v, attn_mask
            )
            attn_output = self.merge_heads(attn_output)
            output = self.out_proj(attn_output)
            return output, (k, v)


class DecoderLayer(torch.nn.Module):
    def __init__(
        self,
        embed_dim,
        num_heads,
        ffn_dim,
        dropout_p,
        activation_dropout_p,
        activation="gelu",
        page_size=16,
        n_pages=256,
        max_batch_size=16,
        device=None,
    ):
        super().__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads
        self.ffn_dim = ffn_dim

        self.dropout = torch.nn.Dropout(dropout_p)

        self.self_attn_layer_norm = torch.nn.LayerNorm(self.embed_dim)
        self.self_attn = PagedAttentionBlock(
            embed_dim,
            num_heads,
            bias=False,
            page_size=page_size,
            n_pages=n_pages,
            max_batch_size=max_batch_size,
            device=device,
        )
        self.activation_fn = transformers.activations.ACT2FN[activation]

        self.encoder_attn_layer_norm = torch.nn.LayerNorm(self.embed_dim)
        self.encoder_attn = AttentionBlock(self.embed_dim, num_heads, bias=False)

        self.activation_dropout = torch.nn.Dropout(activation_dropout_p)
        self.fc1 = torch.nn.Linear(self.embed_dim, self.ffn_dim, bias=False)
        self.fc2 = torch.nn.Linear(self.ffn_dim, self.embed_dim, bias=False)
        self.final_layer_norm = torch.nn.LayerNorm(self.embed_dim)

    def forward(
        self,
        hidden_states,
        encoder_hidden_states,
        kv_cache=None,
        encoder_kv_cache=None,
        self_attn_mask=None,
        cross_attn_mask=None,
        paged_self_cache=None,
        batch_idx=None,
        input_pos=None,
        causal_offset=None,
    ):
        residual = hidden_states
        hidden_states = self.self_attn_layer_norm(hidden_states)
        if paged_self_cache is not None:
            paged_attention, k_cache, v_cache = (
                paged_self_cache["paged_attention"],
                paged_self_cache["k_cache"],
                paged_self_cache["v_cache"],
            )
            hidden_states, _ = self.self_attn(
                hidden_states,
                batch_idx=batch_idx,
                input_pos=input_pos,
                paged_attention=paged_attention,
                k_cache=k_cache,
                v_cache=v_cache,
                causal_offset=causal_offset,
                attn_mask=self_attn_mask,
            )
            layer_kv_cache = None
        else:
            hidden_states, layer_kv_cache = self.self_attn(
                hidden_states,
                kv_cache=kv_cache,
                attn_mask=self_attn_mask,
            )
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        residual = hidden_states
        hidden_states = self.encoder_attn_layer_norm(hidden_states)
        hidden_states, encoder_kv_cache = self.encoder_attn(
            hidden_states,
            key_value_states=encoder_hidden_states,
            kv_cache=encoder_kv_cache,
            attn_mask=cross_attn_mask,
        )
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        residual = hidden_states
        hidden_states = self.final_layer_norm(hidden_states)
        hidden_states = self.fc1(hidden_states)
        hidden_states = self.activation_fn(hidden_states)
        hidden_states = self.activation_dropout(hidden_states)
        hidden_states = self.fc2(hidden_states)
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        return hidden_states, layer_kv_cache, encoder_kv_cache
