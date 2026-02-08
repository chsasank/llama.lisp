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


p1 = model_inputs_0["prompt_hidden_states"].shape[1]
p2 = model_inputs_1["prompt_hidden_states"].shape[1]
p = merged_prompt_hidden_states.shape[1]

d1 = model_inputs_0["decoder_input_ids"].shape[1]
d2 = model_inputs_1["decoder_input_ids"].shape[1]
d = merged_decoder_input_ids.shape[1]

n_dec = p + d

e1 = model_inputs_0["encoder_outputs"].last_hidden_state.shape[1]
e2 = model_inputs_1["encoder_outputs"].last_hidden_state.shape[1]
n_enc = merged_encoder_last_hidden_states.shape[1]

print(p1, p2, p1, d1, d2, d, n_dec, e1, e2, n_enc)
