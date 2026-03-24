import torch
import os
import uuid
from inference.modeling import ParlerTTS
from inference.config import device
from inference.paging import VirtualMemory


class TTSRequest:
    def __init__(self, prompt, description):
        self.pid = uuid.uuid4().hex[:6]
        self.prompt = prompt
        self.description = description
        self.decoder_input_ids = []
        self.decoder_position_ids = []

    def __repr__(self):
        return f"""TTSRequest(
        pid={self.pid},
        prompt='{self.prompt}',
        description='{self.description}',
        decoder_input_ids={self.decoder_input_ids},
        decoder_position_ids={self.decoder_position_ids}
    )
    """


class ParlerTTSModelRunner:
    def __init__(self, checkpoint_path):
        self.model = ParlerTTS(checkpoint_path).eval().to(device)
        num_kv_heads = self.model.config["text_encoder"]["num_heads"]
        head_dim = self.model.config["decoder"]["hidden_size"] // num_kv_heads
        num_layers = self.model.config["decoder"]["num_hidden_layers"]
        self.self_attn_vmem = VirtualMemory(
            max_num_pages=1024,
            num_kv_heads=num_kv_heads,
            page_size=16,
            head_dim=head_dim,
            num_layers=num_layers,
        )
        self.cross_attn_vmem = VirtualMemory(
            max_num_pages=1024,
            num_kv_heads=num_kv_heads,
            page_size=16,
            head_dim=head_dim,
            num_layers=num_layers,
        )

        self.num_codebooks = self.model.config["decoder"]["num_codebooks"]
        self.bos_token_id = self.model.config["decoder"]["bos_token_id"]
        self.running_requests = {}

    def prefill(self, request):
        self.running_requests[request.pid] = request

        encoder_hidden_states, prompt_hidden_states = self.model.encode(
            [request.prompt], [request.description]
        )
        decoder_input_ids = torch.full(
            (self.num_codebooks, 1), self.bos_token_id, dtype=torch.int32, device=device
        )
        decoder_position_ids = torch.arange(
            prompt_hidden_states.shape[1] + 1, dtype=torch.int32, device=device
        ).unsqueeze(0)

        request.decoder_input_ids.append(decoder_input_ids)
        request.decoder_position_ids.append(decoder_position_ids)

        logits, model_kv_cache, model_encoder_kv_cache = self.model.prefill(
            decoder_input_ids=decoder_input_ids,
            decoder_position_ids=decoder_position_ids,
            encoder_hidden_states=encoder_hidden_states,
            prompt_hidden_states=prompt_hidden_states,
        )
        self.self_attn_vmem.prefill(pid=request.pid, model_kv_cache=model_kv_cache)
        self.cross_attn_vmem.prefill(
            pid=request.pid, model_kv_cache=model_encoder_kv_cache
        )

        # TODO: dumb sample
        sampled_tokens = logits.argmax(dim=-1)
        next_decoder_input_ids = sampled_tokens[0, :, -1:]
        next_decoder_position_ids = decoder_position_ids[:, -1:] + 1

        request.decoder_input_ids.append(next_decoder_input_ids)
        request.decoder_position_ids.append(next_decoder_position_ids)

    def step(self):
        sorted_pids = sorted(self.running_requests.keys())
        decoder_input_ids = torch.cat(
            [self.running_requests[pid].decoder_input_ids[-1] for pid in sorted_pids],
            dim=0,
        )
        decoder_position_ids = torch.cat(
            [
                self.running_requests[pid].decoder_position_ids[-1]
                for pid in sorted_pids
            ],
            dim=0,
        )

        logits = self.model.decode(
            decoder_input_ids=decoder_input_ids,
            decoder_position_ids=decoder_position_ids,
            model_kv_cache_vmem=self.self_attn_vmem,
            model_encoder_kv_cache_vmem=self.cross_attn_vmem,
        )
        # TODO: dumb sample
        sampled_tokens = logits.argmax(dim=-1)

        next_decoder_input_ids = sampled_tokens[:, :, -1:]
        next_decoder_position_ids = decoder_position_ids[:, -1:] + 1

        for bid, pid in enumerate(sorted_pids):
            self.running_requests[pid].decoder_input_ids.append(
                next_decoder_input_ids[bid]
            )
            self.running_requests[pid].decoder_position_ids.append(
                next_decoder_position_ids[bid].unsqueeze(0)
            )

    def evict(self, request):
        del self.running_requests[request.pid]
        self.self_attn_vmem.page_table.free(request.pid)
        self.cross_attn_vmem.page_table.free(request.pid)
