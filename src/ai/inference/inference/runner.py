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
        self.decoder_input_ids = None
        self.decoder_position_ids = None




class ParlerTTSModelRunner:
    def __init__(self):
        self.model = (
            ParlerTTS(
                "/home/sasank/code/inference-opt/src/ai/inference/tests/checkpoints"
            )
            .eval()
            .to(device)
        )
        num_kv_heads = self.model.config["text_encoder"]["num_heads"]
        head_dim = self.model.config["decoder"]["hidden_size"] // num_kv_heads
        num_layers = self.model.config["decoder"]["num_hidden_layers"]
        self.vmem = VirtualMemory(
            max_num_pages=1024,
            num_kv_heads=num_kv_heads,
            page_size=16,
            head_dim=head_dim,
            num_layers=num_layers,
        )

        self.num_codebooks = self.model.config['decoder']['num_codebooks']
        self.bos_token_id = self.model.config['decoder']['bos_token_id']
        self.running_requests = {}

    def prefill(self, request):
        encoder_hidden_states, prompt_hidden_states = self.model.encode([request.prompt], [request.description])
        decoder_input_ids = torch.full((self.num_codebooks, 1), self.bos_token_id, dtype=torch.int32, device=device)
        decoder_position_ids = torch.arange(prompt_hidden_states.shape[1] + 1, dtype=torch.int32, device=device).unsqueeze(0)
        logits, model_kv_cache, model_encoder_kv_cache = self.model.prefill(
            decoder_input_ids=decoder_input_ids,
            decoder_position_ids=decoder_position_ids,
            encoder_hidden_states=encoder_hidden_states,
            prompt_hidden_states=prompt_hidden_states,
            cross_attn_mask=None,
        )
        self.vmem.prefill(pid=request.pid, model_kv_cache=model_kv_cache)
        self.running_requests[request.pid] = request


    def sample(self):
        pass
        
