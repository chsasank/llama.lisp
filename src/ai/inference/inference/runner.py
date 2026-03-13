import torch
from modeling.parler_tts_runner import ParlerTTSModelRunner

runner = ParlerTTSModelRunner()

prompts = ["अरे, तुम आज कैसे हो?", "आपका नाम क्या है?", "मेरा नाम विद्या है!"]

descriptions = [
    "Divya's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise.",
    "male voice's voice is very loud yet slightly slow in delivery, with a very close recording.",
    "Female voice is sweet yet slightly fast in delivery, with a very close recording that almost has no background noise.",
]


y_enc_0 = runner.my_encoder([prompts[0]], [descriptions[0]])
model_inputs_0 = runner.model.prepare_inputs_for_generation(
    y_enc_0["delayed_input_ids"], **y_enc_0["model_kwargs"]
)
model_out_0 = runner.model(**model_inputs_0)

y_enc_1 = runner.my_encoder([prompts[1]], [descriptions[1]])
model_inputs_1 = runner.model.prepare_inputs_for_generation(
    y_enc_1["delayed_input_ids"], **y_enc_1["model_kwargs"]
)
model_out_1 = runner.model(**model_inputs_1)

lhs0 = model_inputs_0["encoder_outputs"].last_hidden_state
lhs1 = model_inputs_1["encoder_outputs"].last_hidden_state
merged_encoder_last_hidden_states = torch.cat([lhs0, lhs1], dim=1)

merged_encoder_outputs = model_inputs_0["encoder_outputs"]
merged_encoder_outputs.last_hidden_state = merged_encoder_last_hidden_states

merged_decoder_input_ids = torch.cat(
    [model_inputs_0["decoder_input_ids"], model_inputs_1["decoder_input_ids"]], dim=1
)

merged_prompt_hidden_states = torch.cat(
    [model_inputs_0["prompt_hidden_states"], model_inputs_1["prompt_hidden_states"]],
    dim=1,
)

# empty for now
merged_past_key_values = y_enc_0["model_kwargs"]["past_key_values"]


p0 = model_inputs_0["prompt_hidden_states"].shape[1]
p1 = model_inputs_1["prompt_hidden_states"].shape[1]
p = merged_prompt_hidden_states.shape[1]

d0 = model_inputs_0["decoder_input_ids"].shape[1]
d1 = model_inputs_1["decoder_input_ids"].shape[1]
d = merged_decoder_input_ids.shape[1]

n_dec = p + d

e0 = model_inputs_0["encoder_outputs"].last_hidden_state.shape[1]
e1 = model_inputs_1["encoder_outputs"].last_hidden_state.shape[1]
n_enc = merged_encoder_last_hidden_states.shape[1]

print(p0, p1, p0, d0, d1, d, n_dec, e0, e1, n_enc)

encoder_attn_mask = torch.zeros((n_dec, n_enc), dtype=torch.bool, device=runner.model.device)

seq0_dec_idxs = list(range(0, p0)) + list(range(p, p+d0))
seq1_dec_idxs = list(range(p0, p0+p1)) + list(range(p+d0, p+d0+d1))

seq0_enc_idxs = list(range(0, e0))
seq1_enc_idxs = list(range(e0, e0+e1))

for i in seq0_dec_idxs:
    for j in seq0_enc_idxs:
        encoder_attn_mask[i, j] = 1

for i in seq1_dec_idxs:
    for j in seq1_enc_idxs:
        encoder_attn_mask[i, j] = 1

from matplotlib import pyplot as plt
plt.imshow(encoder_attn_mask.cpu())
plt.savefig('foo.png')

encoder_attn_mask = encoder_attn_mask.unsqueeze(0).unsqueeze(0)
encoder_attn_mask_float = torch.full(encoder_attn_mask.shape, float("-inf"), dtype=runner.model.dtype, device=runner.model.device)
encoder_attn_mask_float[encoder_attn_mask] = 0
