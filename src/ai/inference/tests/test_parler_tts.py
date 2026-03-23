import torch
import os
from inference.modeling import ParlerTTS
from inference.config import device
from inference.paging import VirtualMemory
from inference.runner import ParlerTTSModelRunner, TTSRequest

here = os.path.dirname(__file__)


def test_model_run():
    model = ParlerTTS(os.path.join(here, "checkpoints")).eval().to(device)
    prompts = ["अरे, तुम आज कैसे हो?"]
    descriptions = [
        "Divya's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise."
    ]
    ref = torch.load(
        os.path.join(here, "checkpoints/model_prefill_ref.pt"),
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

    max_size = vmem.page_table.pid_mem_sizes[0]
    assert max_size < vmem.page_size
    for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 0, :max_size],
            model_kv_cache[layer_id][0][0, :, :max_size].transpose(0, 1),
            atol=5e-2,
        )
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 1, :max_size],
            model_kv_cache[layer_id][1][0, :, :max_size].transpose(0, 1),
            atol=5e-2,
        )

    # model step
    step_ref = ref = torch.load(
        os.path.join(here, "checkpoints/model_step_ref.pt"),
        weights_only=False,
        map_location=device,
    )
    step_decoder_input_ids = step_ref["decoder_input_ids"]
    step_decoder_position_ids = step_ref["decoder_position_ids"]
    step_cross_attn_mask = step_ref["attention_mask"]
    step_logits = model.decode(
        decoder_input_ids=step_decoder_input_ids,
        decoder_position_ids=step_decoder_position_ids,
        encoder_hidden_states=encoder_hidden_states,
        model_encoder_kv_cache=model_encoder_kv_cache,
        model_kv_cache_vem=vmem,
        cross_attn_mask=step_cross_attn_mask,
    )

    step_expected_logits = step_ref["logits"]
    assert torch.allclose(step_expected_logits, step_logits[0], atol=5e-2)
    step_expected_past_key_values = step_ref["past_key_values"]

    max_size = vmem.page_table.pid_mem_sizes[0]
    assert max_size < vmem.page_size
    for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 0, :max_size],
            step_expected_past_key_values[layer_id][0][0, :, :max_size]
            .transpose(0, 1)
            .half(),
            atol=5e-2,
        )
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 1, :max_size],
            step_expected_past_key_values[layer_id][1][0, :, :max_size]
            .transpose(0, 1)
            .half(),
            atol=5e-2,
        )


def test_runner_obj():
    model_runner = ParlerTTSModelRunner()
    req = TTSRequest(
        prompt="अरे, तुम आज कैसे हो?",
        description="Divya's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise.",
    )
    model_runner.prefill(req)


# test_model_run()
test_runner_obj()
