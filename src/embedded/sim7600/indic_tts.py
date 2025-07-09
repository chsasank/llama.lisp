import torch
from parler_tts import ParlerTTSForConditionalGeneration
from transformers import AutoTokenizer
import soundfile as sf
from indicnlp.tokenize.sentence_tokenize import sentence_split as indic_sentence_split

device = "cuda:0" if torch.cuda.is_available() else "cpu"

model = ParlerTTSForConditionalGeneration.from_pretrained("ai4bharat/indic-parler-tts").to(device)
tokenizer = AutoTokenizer.from_pretrained("ai4bharat/indic-parler-tts")
description_tokenizer = AutoTokenizer.from_pretrained(model.config.text_encoder._name_or_path)


prompt = "इंदिरा आवास योजनेसाठी पात्र असण्यासाठी, व्यक्तीला गरीबी लाइनखाली असावे आणि ग्रामसभेने तयार केलेल्या प्रतीक्षारख्यात समाविष्ट असणे आवश्यक आहे"
# description = "Sanjay speaks naturally in a calm, moderate-pitched voice, delivering the news with a neutral tone. The recording is low high quality with some background noise."
description = "Sunita speaks slowly in a calm, moderate-pitched voice, delivering the news with a neutral tone. The recording is very high quality with no background noise."


description_input_ids = description_tokenizer(description, return_tensors="pt").to(device)
prompt_input_ids = tokenizer(prompt, return_tensors="pt").to(device)

generation = model.generate(input_ids=description_input_ids.input_ids, attention_mask=description_input_ids.attention_mask, prompt_input_ids=prompt_input_ids.input_ids, prompt_attention_mask=prompt_input_ids.attention_mask)
audio_arr = generation.cpu().numpy().squeeze()
sf.write("indic_tts_out.wav", audio_arr, model.config.sampling_rate)
