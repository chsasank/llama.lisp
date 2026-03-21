import os
import json

import transformers
import torch
from layers import SinusoidalPositionalEmbedding, DecoderLayer
from paging import VirtualMemory
from config import device


class ParlerTTS(torch.nn.Module):
    def __init__(self, checkpoint_dir):
        super().__init__()
        self.checkpoint_dir = checkpoint_dir
        self.config = json.load(
            open(os.path.join(checkpoint_dir, "indic_parler_tts_config.json"))
        )
        self.description_tokenizer = transformers.AutoTokenizer.from_pretrained(
            "google/flan-t5-large"
        )
        self.prompt_tokenizer = transformers.AutoTokenizer.from_pretrained(
            "ai4bharat/indic-parler-tts"
        )
        self.description_encoder = transformers.T5EncoderModel.from_pretrained(
            os.path.join(checkpoint_dir, "./parler_tts_text_encoder")
        )
        self.embed_prompt = torch.nn.Embedding(
            self.config["vocab_size"], self.config["decoder"]["hidden_size"]
        )
        self.embed_audio = torch.nn.ModuleList(
            [
                torch.nn.Embedding(
                    self.config["decoder"]["vocab_size"] + 1,
                    self.config["decoder"]["hidden_size"],
                )
                for _ in range(self.config["decoder"]["num_codebooks"])
            ]
        )
        self.embed_position = SinusoidalPositionalEmbedding(
            self.config["decoder"]["max_position_embeddings"],
            self.config["decoder"]["hidden_size"],
        )
        self.decoder_layers = torch.nn.ModuleList(
            [
                DecoderLayer(
                    embed_dim=self.config["decoder"]["hidden_size"],
                    num_heads=self.config["text_encoder"]["num_heads"],
                    ffn_dim=self.config["decoder"]["ffn_dim"],
                    dropout_p=self.config["decoder"]["dropout"],
                    activation_dropout_p=self.config["decoder"]["attention_dropout"],
                    activation=self.config["decoder"]["activation_function"],
                )
                for _ in range(self.config["decoder"]["num_hidden_layers"])
            ]
        )
        self.layer_norm = torch.nn.LayerNorm(self.config["decoder"]["hidden_size"])
        self.lm_heads = torch.nn.Linear(
            self.config["decoder"]["hidden_size"],
            (
                self.config["decoder"]["vocab_size"]
                * self.config["decoder"]["num_codebooks"]
            ),
            bias=False,
        )
        self._load_weights()

    def _load_weights(self):
        def _sub_state_dict(prefix, model_weights):
            prefix = prefix + "."
            return {
                k.replace(prefix, ""): v
                for k, v in model_weights.items()
                if prefix in k
            }

        model_weights = torch.load(
            os.path.join(self.checkpoint_dir, "parles_tts_state_dict.pt"),
            map_location=torch.device("cpu"),
            weights_only=True,
        )

        self.embed_prompt.load_state_dict(
            _sub_state_dict("embed_prompts", model_weights)
        )
        self.embed_audio.load_state_dict(
            _sub_state_dict("decoder.model.decoder.embed_tokens", model_weights)
        )
        self.embed_position.weights = model_weights[
            "decoder.model.decoder.embed_positions.weights"
        ]
        self.decoder_layers.load_state_dict(
            _sub_state_dict("decoder.model.decoder.layers", model_weights)
        )
        self.layer_norm.load_state_dict(
            _sub_state_dict("decoder.model.decoder.layer_norm", model_weights)
        )
        self.lm_heads.load_state_dict(
            _sub_state_dict("decoder.lm_heads", model_weights)
        )

    @torch.no_grad
    def encode(
        self,
        prompts,
        descriptions,
    ):
        num_codebooks = self.config["decoder"]["num_codebooks"]
        desc_tokens = model.description_tokenizer(descriptions, return_tensors="pt")
        encoder_hidden_states = model.description_encoder(
            desc_tokens["input_ids"].to(device)
        ).last_hidden_state
        prompt_tokens = model.prompt_tokenizer(prompts, return_tensors="pt").to(device)
        prompt_hidden_states = model.embed_prompt(prompt_tokens["input_ids"])
        return encoder_hidden_states, prompt_hidden_states

    @torch.no_grad
    def prefill(
        self,
        decoder_input_ids,
        decoder_position_ids,
        encoder_hidden_states,
        prompt_hidden_states,
        cross_attn_mask=None,
    ):
        num_decoder_layers = self.config["decoder"]["num_hidden_layers"]
        num_codebooks = self.config["decoder"]["num_codebooks"]
        vocab_size = self.config["decoder"]["vocab_size"]

        decoder_input_ids = decoder_input_ids.reshape(
            -1, num_codebooks, decoder_input_ids.shape[-1]
        )
        inputs_embeds = sum(
            [
                self.embed_audio[codebook](decoder_input_ids[:, codebook])
                for codebook in range(num_codebooks)
            ]
        )
        inputs_embeds = torch.cat([prompt_hidden_states, inputs_embeds], dim=1)

        position_embeds = self.embed_position.from_position_ids(
            decoder_position_ids
        ).to(inputs_embeds.device)
        hidden_states = inputs_embeds + position_embeds

        model_kv_cache = [None for _ in range(num_decoder_layers)]
        model_encoder_kv_cache = [None for _ in range(num_decoder_layers)]

        for layer in range(num_decoder_layers):
            hidden_states, layer_kv_cache, layer_encoder_kv_cache = self.decoder_layers[
                layer
            ](
                hidden_states=hidden_states,
                encoder_hidden_states=encoder_hidden_states,
                encoder_kv_cache=model_encoder_kv_cache[layer],
                cross_attn_mask=cross_attn_mask,
            )
            model_kv_cache[layer] = layer_kv_cache
            model_encoder_kv_cache[layer] = layer_encoder_kv_cache

        hidden_states = self.layer_norm(hidden_states)
        lm_logits = (
            self.lm_heads(hidden_states)
            .view(hidden_states.shape[0], -1, num_codebooks, vocab_size)
            .transpose(1, 2)
        )
        return lm_logits, model_kv_cache, model_encoder_kv_cache


model = ParlerTTS("/home/sasank/code/inference-opt/checkpoints").eval().to(device)


def test_prefill():
    prompts = ["अरे, तुम आज कैसे हो?"]
    descriptions = [
        "Divya's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise."
    ]
    ref = torch.load(
        "/home/sasank/code/inference-opt/checkpoints/values_to_save.pt",
        weights_only=False,
        map_location=device,
    )

    # description -> encoder
    expected_encoder_outputs = ref["encoder_outputs"].last_hidden_state
    desc_tokens = model.description_tokenizer(
        descriptions, device=device, return_tensors="pt"
    )
    encoder_outputs = model.description_encoder(
        desc_tokens["input_ids"].to(device)
    ).last_hidden_state
    assert torch.allclose(expected_encoder_outputs, encoder_outputs, atol=1e-4)

    # prompts -> embeddings
    expected_prompt_hidden_states = ref["prompt_hidden_states"]
    prompt_tokens = model.prompt_tokenizer(prompts, return_tensors="pt").to(device)
    prompt_hidden_states = model.embed_prompt(prompt_tokens["input_ids"])
    assert torch.allclose(
        expected_prompt_hidden_states, prompt_hidden_states, atol=1e-4
    )

    decoder_input_ids = ref["decoder_input_ids"]
    decoder_position_ids = ref["decoder_position_ids"]
    self_attn_mask = ref["decoder_attention_mask"]
    cross_attn_mask = ref["attention_mask"]

    expected_logits = ref["prefill_logits"]
    expected_past_key_values = ref["past_key_values"]

    encoder_hidden_states, prompt_hidden_states = model.encode(prompts, descriptions)
    logits, model_kv_cache, model_encoder_kv_cache = model.prefill(
        decoder_input_ids=decoder_input_ids,
        decoder_position_ids=decoder_position_ids,
        encoder_hidden_states=encoder_hidden_states,
        prompt_hidden_states=prompt_hidden_states,
        cross_attn_mask=cross_attn_mask,
    )
    assert torch.allclose(expected_logits, logits[0], atol=0.05)

    for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
        assert torch.allclose(
            expected_past_key_values[layer_id][0].half(),
            model_kv_cache[layer_id][0],
            atol=5e-2,
        )
        assert torch.allclose(
            expected_past_key_values[layer_id][1].half(),
            model_kv_cache[layer_id][1],
            atol=5e-2,
        )
        assert torch.allclose(
            expected_past_key_values[layer_id][2].half(),
            model_encoder_kv_cache[layer_id][0],
            atol=5e-2,
        )
        assert torch.allclose(
            expected_past_key_values[layer_id][3].half(),
            model_encoder_kv_cache[layer_id][1],
            atol=5e-2,
        )

    num_kv_heads = model.config["text_encoder"]["num_heads"]
    head_dim = model.config["decoder"]["hidden_size"] // num_kv_heads

    vmem = VirtualMemory(
        max_num_pages=1024,
        num_kv_heads=num_kv_heads,
        page_size=16,
        head_dim=head_dim,
        num_layers=model.config["decoder"]["num_hidden_layers"],
    )
    vmem.prefill(pid=0, model_kv_cache=model_kv_cache)

    for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
        # first page first few seqs should match
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 0, :4],
            model_kv_cache[layer_id][0][0, :, :4].transpose(0, 1),
            atol=5e-2,
        )

    # # model step
    # step_ref = ref = torch.load(
    #     "/home/sasank/code/inference-opt/checkpoints/model_step.pt",
    #     weights_only=False,
    #     map_location=torch.device("cpu"),
    # )
    # step_decoder_input_ids = step_ref["decoder_input_ids"]
    # step_decoder_position_ids = step_ref["decoder_position_ids"]
    # step_self_attn_mask = step_ref["decoder_attention_mask"]
    # step_cross_attn_mask = step_ref["attention_mask"]
    # step_logits, step_model_kv_cache, step_model_encoder_kv_cache = model.decode(
    #     encoder_hidden_states=encoder_hidden_states,
    #     decoder_input_ids=step_decoder_input_ids,
    #     decoder_position_ids=step_decoder_position_ids,
    #     model_kv_cache=model_kv_cache,
    #     model_encoder_kv_cache=model_encoder_kv_cache,
    #     self_attn_mask=step_self_attn_mask,
    #     cross_attn_mask=step_cross_attn_mask,
    # )

    # step_expected_logits = step_ref["logits"]
    # assert torch.allclose(step_expected_logits, step_logits[0], atol=1e-4)

    # step_expected_past_key_values = step_ref["past_key_values"]
    # for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
    #     assert torch.allclose(
    #         step_expected_past_key_values[layer_id][0],
    #         step_model_kv_cache[layer_id][0],
    #         atol=1e-4,
    #     )
    #     assert torch.allclose(
    #         step_expected_past_key_values[layer_id][1],
    #         step_model_kv_cache[layer_id][1],
    #         atol=1e-4,
    #     )
    #     assert torch.allclose(
    #         step_expected_past_key_values[layer_id][2],
    #         step_model_encoder_kv_cache[layer_id][0],
    #         atol=1e-4,
    #     )
    #     assert torch.allclose(
    #         step_expected_past_key_values[layer_id][3],
    #         step_model_encoder_kv_cache[layer_id][1],
    #         atol=1e-4,
    #     )


test_prefill()
