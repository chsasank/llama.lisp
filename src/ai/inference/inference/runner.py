import torch
import uuid
from inference.modeling import ParlerTTS
from inference.config import device
from inference.paging import VirtualMemory
import math
import transformers


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
        self.topk_processor = transformers.TopKLogitsWarper(top_k=50)
        self.num_codebooks = self.model.config["decoder"]["num_codebooks"]
        self.bos_token_id = self.model.config["decoder"]["bos_token_id"]
        self.eos_token_id = self.model.config["decoder"]["eos_token_id"]
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
        next_decoder_input_ids = self.sample(request, logits, mode="prefill")
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
        next_decoder_position_ids = decoder_position_ids[:, -1:] + 1

        for bid, pid in enumerate(sorted_pids):
            next_decoder_input_ids = self.sample(
                self.running_requests[pid], logits[bid]
            )
            self.running_requests[pid].decoder_input_ids.append(next_decoder_input_ids)
            self.running_requests[pid].decoder_position_ids.append(
                next_decoder_position_ids[bid].unsqueeze(0)
            )

    def sample(self, request, logits, mode="step", sampling="multinomial"):
        if sampling == "argmax":
            if mode == "prefill":
                sampled_tokens = logits.argmax(dim=-1)[0, :, -1:]
            else:
                sampled_tokens = logits.argmax(dim=-1)
        else:
            if mode == "prefill":
                scores = logits[0, :, -1]
                scores = self.topk_processor(
                    torch.cat(request.decoder_input_ids), scores
                )
                sampled_tokens = torch.multinomial(
                    torch.softmax(scores, dim=-1).view(-1, scores.size(-1)), 1
                ).view(scores.size(0), 1)
            else:
                scores = logits[:, 0]
                eos_num = (
                    (request.decoder_input_ids[-1] == self.eos_token_id).sum().item()
                )
                eos_token_mask = torch.ones(
                    self.num_codebooks, dtype=torch.bool, device=device
                )
                eos_token_mask[: eos_num + 1] = False
                scores[eos_token_mask, self.eos_token_id] = -math.inf
                scores = self.topk_processor(
                    torch.cat(request.decoder_input_ids), scores
                )
                sampled_tokens = torch.multinomial(
                    torch.softmax(scores, dim=-1).view(-1, scores.size(-1)), 1
                ).view(scores.size(0), 1)
                if eos_num > 0:
                    sampled_tokens = torch.where(
                        eos_token_mask, sampled_tokens.squeeze(), self.eos_token_id
                    ).unsqueeze(-1)

        mask = torch.arange(self.num_codebooks) < len(request.decoder_input_ids)
        next_decoder_input_ids = torch.where(
            mask.to(device), sampled_tokens.squeeze(), self.bos_token_id
        ).unsqueeze(-1)
        return next_decoder_input_ids

    def check_stopping_criteria(self):
        sorted_pids = sorted(self.running_requests.keys())
        for pid in sorted_pids:
            decoder_input_ids = self.running_requests[pid].decoder_input_ids[-1]
            to_stop = torch.all(decoder_input_ids == self.eos_token_id)
            if to_stop:
                self.evict(self.running_requests[pid])

    def evict(self, request):
        del self.running_requests[request.pid]
        self.self_attn_vmem.page_table.free(request.pid)
        self.cross_attn_vmem.page_table.free(request.pid)
