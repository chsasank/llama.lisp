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
        next_decoder_input_ids = self._sample_prefill(request, logits)
        next_decoder_position_ids = decoder_position_ids[:, -1:] + 1
        request.decoder_input_ids.append(next_decoder_input_ids)
        request.decoder_position_ids.append(next_decoder_position_ids)

    def _sample_prefill(self, request, logits, sampling="multinomial"):
        if sampling == "argmax":
            sampled_tokens = logits.argmax(dim=-1)[0, :, -1:]
        else:
            scores = logits[0, :, -1]
            scores = self.topk_processor(input_ids=None, scores=scores)
            sampled_tokens = torch.multinomial(
                torch.softmax(scores, dim=-1).view(-1, scores.size(-1)), 1
            ).view(scores.size(0), 1)

        mask = torch.arange(self.num_codebooks) < len(request.decoder_input_ids)
        next_decoder_input_ids = torch.where(
            mask.to(device), sampled_tokens.squeeze(), self.bos_token_id
        ).unsqueeze(-1)
        return next_decoder_input_ids

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
        next_decoder_input_ids = self._sample_decode(logits=logits)

        for bid, pid in enumerate(sorted_pids):
            self.running_requests[pid].decoder_input_ids.append(
                next_decoder_input_ids[bid]
            )
            self.running_requests[pid].decoder_position_ids.append(
                next_decoder_position_ids[bid].unsqueeze(0)
            )

    def _sample_decode(self, logits, sampling="multinomial"):
        sorted_pids = sorted(self.running_requests.keys())
        if sampling == "argmax":
            sampled_tokens = logits.argmax(dim=-1)
        else:
            scores = logits[:, :, 0]
            stacked_decoder_input_ids = torch.stack(
                [
                    self.running_requests[pid].decoder_input_ids[-1][:, 0]
                    for pid in sorted_pids
                ],
                dim=0,
            )
            # find number of eos per batch in input ids
            eos_num = (stacked_decoder_input_ids == self.eos_token_id).sum(dim=1)
            # do not allow eos token for eos_num + 1 to rest of codebooks
            eos_token_mask = torch.arange(self.num_codebooks, device=device).unsqueeze(
                0
            ) > eos_num.unsqueeze(1)
            scores[eos_token_mask, self.eos_token_id] = -math.inf

            # get samples from scores now
            scores = self.topk_processor(input_ids=None, scores=scores)
            sampled_tokens = torch.multinomial(
                torch.softmax(scores, dim=-1).view(-1, scores.shape[-1]), num_samples=1
            ).view(scores.shape[:2])

            # set eos token forcibly, but only if eos_num.max() > 0:
            eos_token_mask[eos_num == 0] = True
            sampled_tokens[~eos_token_mask] = self.eos_token_id

        # set bos mask
        current_seq_lens = (
            torch.Tensor(
                [
                    len(self.running_requests[pid].decoder_input_ids)
                    for pid in sorted_pids
                ]
            )
            .int()
            .to(device)
        )
        bos_token_mask = torch.arange(self.num_codebooks, device=device).unsqueeze(
            0
        ) >= current_seq_lens.unsqueeze(1)
        sampled_tokens[bos_token_mask] = self.bos_token_id
        return sampled_tokens.unsqueeze(-1)

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
