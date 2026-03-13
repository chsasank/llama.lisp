import torch
from transformers.models.musicgen.modeling_musicgen import (
    MusicgenSinusoidalPositionalEmbedding,
)


class SinusoidalPositionalEmbedding(MusicgenSinusoidalPositionalEmbedding):
    def from_position_ids(self, position_ids):
        bs, seq_len = position_ids.shape
        flat_embs = self.weights.index_select(0, position_ids.view(-1))
        return flat_embs.reshape(bs, seq_len, -1)


class AttentionBlock(nn.Module):
    def __init__(self, embed_dim, num_heads, bias=False):
        super().__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads

        self.k_proj = nn.Linear(embed_dim, embed_dim, bias=bias)
        self.v_proj = nn.Linear(embed_dim, embed_dim, bias=bias)
        self.q_proj = nn.Linear(embed_dim, embed_dim, bias=bias)
        self.out_proj = nn.Linear(embed_dim, embed_dim, bias=bias)

    def split_heads(self, x):
        batch_size, seq_len, _ = x.shape
        return x.view(batch_size, seq_len, self.num_heads, -1).transpose(1, 2).contiguous()

    def forward(self, query_states, key_value_states=None, past_key_values=None)
        """If key_value_states is not None, assumed to cross attention"""
        is_cross_attn = key_value_states is not None
        if key_value_states is None:
            key_value_states = query_states

        k = self.split_heads(self.k_proj(key_value_states))
        v = self.split_heads(self.v_proj(key_value_states))
        q = self.split_heads(self.q_proj(query_states))



