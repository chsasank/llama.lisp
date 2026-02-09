import torch
import transformers


# Same class for encoder and decoder cache mostly
class SimpleKVCacheManager:
    def __init__(
        self, max_size, num_layers, num_heads, hidden_dim, dtype, device
    ):
        self.max_size = max_size
        self.num_layers = num_layers
        self.num_heads = num_heads
        self.hidden_dim = hidden_dim

        self.dtype = dtype
        self.device = device
        self.batch_size = 1
        self.kv = 2

        # kv cache shape for a layer/k
        # bs x num_heads x seq_size x hidden_dim
        # assume bs = 1 and we are doing blocked, so seq_size will by expanded
        # to max_size. So final shape is
        # num_layers x 2 x bs x num_heads x max_size x hidden_dim
        self.kv_cache = torch.zeros(
            [
                self.num_layers,
                self.kv,
                self.batch_size,
                self.num_heads,
                self.max_size
                self.hidden_dim,
            ],
            dtype=dtype,
            device=device,
        )

    def get_past_key_values(self, blocks):
        active_kv_cache = self.kv_cache[
            :,  # num_layers
            :,  # 2 (k/v)
            :,  # batch_size
            :,  # num_heads
            blocks,
        ].view(
            [
                self.num_layers,
                self.kv,
                self.batch_size,
                self.num_heads,
                -1,
                self.hidden_dim,
            ]
        )
        past_key_values = transformers.DynamicCache()
        past_key_values.key_cache = [
            active_kv_cache[layer_idx, 0] for layer_idx in range(self.num_layers)
        ]
        past_key_values.value_cache = [
            active_kv_cache[layer_idx, 0] for layer_idx in range(self.num_layers)
        ]


class Sequence:
    def __init__(self, request_id):
        self.request_id = request_id
        self.block_table = []
        self.num_tokens = 0
