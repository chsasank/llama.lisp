import torch
from parler_tts import ParlerTTSForConditionalGeneration
from transformers import AutoTokenizer
import soundfile as sf
from clean import (
    num_in_sentence_normalization,
    cleaned_prompt_dataset,
)

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")


class TTSModel:
    def __init__(self):
        raise NotImplementedError

    def infer(self, text):
        raise NotImplementedError


class IndicParlerTTSModel(TTSModel):
    def __init__(
        self,
        description="Anu's voice is monotone yet slightly fast in delivery, with a very close recording that almost has no background noise",
    ):
        self.model = ParlerTTSForConditionalGeneration.from_pretrained(
            "ai4bharat/indic-parler-tts"
        ).to(device)
        self.tokenizer = AutoTokenizer.from_pretrained("ai4bharat/indic-parler-tts")
        self.description_tokenizer = AutoTokenizer.from_pretrained(
            self.model.config.text_encoder._name_or_path
        )
        self.description = description

    def infer(self, text, save_path):
        text_cleaned = cleaned_prompt_dataset([text])
        num_normalized_data = num_in_sentence_normalization(text_cleaned)
        description_input_ids = self.description_tokenizer(
            self.description, return_tensors="pt"
        ).to(device)
        prompt_input_ids = self.tokenizer(num_normalized_data, return_tensors="pt").to(
            device
        )
        generation = self.model.generate(
            input_ids=description_input_ids.input_ids,
            attention_mask=description_input_ids.attention_mask,
            prompt_input_ids=prompt_input_ids.input_ids,
            prompt_attention_mask=prompt_input_ids.attention_mask,
        )
        audio_arr = generation.cpu().numpy().squeeze()
        sf.write(save_path, audio_arr, self.model.config.sampling_rate)
        return save_path


class SaraswatiTTSModel(IndicParlerTTSModel):
    def __init__(self):
        self.model = ParlerTTSForConditionalGeneration.from_pretrained(
            "chsasank/saraswati-tts"
        ).to(device)
        self.tokenizer = AutoTokenizer.from_pretrained("ai4bharat/indic-parler-tts")
        self.description_tokenizer = AutoTokenizer.from_pretrained(
            self.model.config.text_encoder._name_or_path
        )
        self.description = "saraswathi speaks kannada in a natural tone"


if __name__ == "__main__":
    import sys
    assert len(sys.argv) == 3
    text = sys.argv[1]
    save_path = sys.argv[2]
    model = SaraswatiTTSModel()
    model.infer(text, save_path)
