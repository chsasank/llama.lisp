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
    desc_tokens = model.description_tokenizer(descriptions, return_tensors="pt")
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
    encoder_vmem = VirtualMemory(
        max_num_pages=1024,
        num_kv_heads=num_kv_heads,
        page_size=16,
        head_dim=head_dim,
        num_layers=model.config["decoder"]["num_hidden_layers"],
    )
    vmem.prefill(pid=0, model_kv_cache=model_kv_cache)
    encoder_vmem.prefill(pid=0, model_kv_cache=model_encoder_kv_cache)

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
        model_kv_cache_vmem=vmem,
        model_encoder_kv_cache_vmem=encoder_vmem,
    )

    step_expected_logits = step_ref["logits"]
    # TODO: close enough?
    assert torch.allclose(step_expected_logits, step_logits[0], atol=1)
    step_expected_past_key_values = step_ref["past_key_values"]

    max_size = vmem.page_table.pid_mem_sizes[0]
    assert max_size < vmem.page_size
    for layer_id in range(model.config["decoder"]["num_hidden_layers"]):
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 0, :max_size],
            step_expected_past_key_values[layer_id][0][0, :, :max_size]
            .transpose(0, 1)
            .half(),
            atol=1e-1,
        )
        assert torch.allclose(
            vmem.paged_model_kv_cache[layer_id][0, 1, :max_size],
            step_expected_past_key_values[layer_id][1][0, :, :max_size]
            .transpose(0, 1)
            .half(),
            atol=1e-1,
        )

    print("model ref test passed")


@torch.no_grad()
def test_runner_obj():
    model_runner = ParlerTTSModelRunner(os.path.join(here, "checkpoints"))
    bs = 1
    requests = [
        TTSRequest(
            prompt="अरे, तुम आज कैसे हो? कैसे हो? कैसे हो? कैसे हो?",
            description="Vidya's voice is monotone.",
        )
        for _ in range(bs)
    ]
    for req in requests:
        model_runner.prefill(req)

    import time

    while len(model_runner.running_requests) > 0:
        start = time.time()
        model_runner.step()
        model_runner.check_stopping_criteria()
        print(
            "model runner step",
            len(model_runner.running_requests),
            1000 * (time.time() - start),
        )

    from scipy.io import wavfile

    start = time.time()
    audio_tokens = torch.cat(requests[0].decoder_input_ids, dim=-1)
    num_codebooks = audio_tokens.shape[0]
    audio_tokens_fixed = []
    for codebook in range(num_codebooks):
        audo_code = audio_tokens[codebook, codebook + 1 : -num_codebooks + codebook]
        audio_tokens_fixed.append(audo_code)

    audio_tokens_fixed = torch.stack(audio_tokens_fixed).unsqueeze(0)
    audio_tokens_fixed = torch.cat([audio_tokens_fixed for _idx in range(16)])[
        :, :, :50
    ]
    print(audio_tokens_fixed.shape)
    print(
        "undelay audio",
        1000 * (time.time() - start),
    )

    for _ in range(10):
        start = time.time()
        audio = model_runner.model.dac.decode(audio_codes=audio_tokens_fixed)[0]
        torch.cuda.synchronize()
        print(
            "dac audio",
            1000 * (time.time() - start),
        )
        start = time.time()

    audio_arr = audio[0].detach().cpu().numpy().astype("float")
    wavfile.write("lol.wav", 44100, audio_arr)

    print("save audio", 1000 * (time.time() - start))
    start = time.time()


# test_model_run()
test_runner_obj()
