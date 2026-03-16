import torch
import transformers
from transformers.models.musicgen.modeling_musicgen import (
    MusicgenSinusoidalPositionalEmbedding,
)


class SinusoidalPositionalEmbedding(MusicgenSinusoidalPositionalEmbedding):
    def from_position_ids(self, position_ids):
        bs, seq_len = position_ids.shape
        flat_embs = self.weights.index_select(0, position_ids.view(-1))
        return flat_embs.reshape(bs, seq_len, -1)


class AttentionBlock(torch.nn.Module):
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

    def forward(self, hidden_states, key_value_states=None, kv_cache=None):
        """If key_value_states is not None, assumed to cross attention"""
        is_cross_attn = key_value_states is not None
        if key_value_states is None:
            # for self attn
            key_value_states = hidden_states

        query = self.split_heads(self.q_proj(hidden_states))

        if is_cross_attn:
            # cross-attention: compute KV once
            if kv_cache is None:
                key = self.split_heads(self.k_proj(key_value_states))
                value = self.split_heads(self.v_proj(key_value_states))
            else:
                key, value = kv_cache
        else:
            # self-attention: append KV every step
            key = self.split_heads(self.k_proj(key_value_states))
            value = self.split_heads(self.v_proj(key_value_states))
            if kv_cache is not None:
                past_key, past_value = kv_cache
                key = torch.cat([past_key, key], dim=2)
                value = torch.cat([past_value, value], dim=2)

        attn_output = torch.nn.functional.scaled_dot_product_attention(
            query,
            key,
            value,
            is_causal=not is_cross_attn,
        )
        attn_output = self.merge_heads(attn_output)
        output = self.out_proj(attn_output)
        kv_cache = (key, value)
        return output, kv_cache


class DecoderLayer(torch.nn.Module):
    def __init__(
        self,
        embed_dim,
        num_heads,
        ffn_dim,
        dropout_p,
        activation_dropout_p,
        activation="gelu",
    ):
        super().__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads
        self.ffn_dim = ffn_dim

        self.dropout = torch.nn.Dropout(dropout_p)

        self.self_attn_layer_norm = torch.nn.LayerNorm(self.embed_dim)
        self.self_attn = AttentionBlock(self.embed_dim, num_heads, bias=False)
        self.activation_fn = transformers.activations.ACT2FN[activation]

        self.encoder_attn_layer_norm = torch.nn.LayerNorm(self.embed_dim)
        self.encoder_attn = AttentionBlock(self.embed_dim, num_heads, bias=False)

        self.activation_dropout = torch.nn.Dropout(activation_dropout_p)
        self.fc1 = torch.nn.Linear(self.embed_dim, self.ffn_dim, bias=False)
        self.fc2 = torch.nn.Linear(self.ffn_dim, self.embed_dim, bias=False)
        self.final_layer_norm = torch.nn.LayerNorm(self.embed_dim)

    def forward(
        self, hidden_states, encoder_hidden_states, kv_cache=None, encoder_kv_cache=None
    ):
        # self attn
        residual = hidden_states
        hidden_states = self.self_attn_layer_norm(hidden_states)
        hidden_states, kv_cache = self.self_attn(hidden_states, kv_cache=kv_cache)
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        # cross attn
        residual = hidden_states
        hidden_states = self.encoder_attn_layer_norm(hidden_states)
        hidden_states, encoder_kv_cache = self.encoder_attn(
            hidden_states,
            key_value_states=encoder_hidden_states,
            kv_cache=encoder_kv_cache,
        )
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        # fully connected
        residual = hidden_states
        hidden_states = self.final_layer_norm(hidden_states)
        hidden_states = self.fc1(hidden_states)
        hidden_states = self.activation_fn(hidden_states)
        hidden_states = self.activation_dropout(hidden_states)
        hidden_states = self.fc2(hidden_states)
        hidden_states = self.dropout(hidden_states)
        hidden_states = residual + hidden_states

        return hidden_states, kv_cache, encoder_kv_cache
