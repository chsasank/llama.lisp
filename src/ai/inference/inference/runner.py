from modeling.parler_tts_runner import ParlerTTSModelRunner

runner = ParlerTTSModelRunner()

prompts = ["अरे, तुम आज कैसे हो?",
            "आपका नाम क्या है?",
            "मेरा नाम विद्या है!"]

descriptions = ["Divya's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise.",
                "male voice's voice is very loud yet slightly slow in delivery, with a very close recording.",
                "Female voice is sweet yet slightly fast in delivery, with a very close recording that almost has no background noise."]



y_enc_0 = runner.my_encoder([prompts[0]], [descriptions[0]])
model_inputs_0 = runner.model.prepare_inputs_for_generation(y_enc_0['delayed_input_ids'], **y_enc_0['model_kwargs']) 
model_out_0 = runner.model(**model_inputs_0)

y_enc_1 = runner.my_encoder([prompts[1]], [descriptions[1]])
model_inputs_1 = runner.model.prepare_inputs_for_generation(y_enc_1['delayed_input_ids'], **y_enc_1['model_kwargs']) 
model_out_1 = runner.model(**model_inputs_1)
