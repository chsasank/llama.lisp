from transformers import WhisperProcessor, WhisperForConditionalGeneration, AutoModel
from datasets import Audio, load_dataset
import os
import torchaudio.functional as F
import torchaudio
import torch
import tempfile
import json
import requests
from tqdm import tqdm
from jiwer import wer, cer
import numpy as np

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
sample_dataset_size = 10

#classes for different model

class ASRModel:
    def __init__(self):
        raise NotImplementedError

    def infer(self, mp3_path):
        raise NotImplementedError


class WhsiperASRModel(ASRModel):
    def __init__(self, model_path):
        self.processor = WhisperProcessor.from_pretrained(model_path)
        self.model = WhisperForConditionalGeneration.from_pretrained(
            model_path, torch_dtype=torch.float16
        ).to(device)
        self.target_sample_rate = 16000

    def infer(self, mp3_path):
        wav, sr = torchaudio.load(mp3_path)
        if sr != self.target_sample_rate:
            resampler = torchaudio.transforms.Resample(
                orig_freq=sr, new_freq=self.target_sample_rate
            )
            wav = resampler(wav)

        input_features = self.processor(
            wav.squeeze().numpy(),
            sampling_rate=self.target_sample_rate,
            return_tensors="pt",
        ).input_features
        input_features = input_features.to(device, dtype=torch.float16)
        # generate token ids
        predicted_ids = self.model.generate(
            input_features, task="transcribe", language="kn"
        )
        # decode token ids to text
        transcription_1 = self.processor.batch_decode(
            predicted_ids, skip_special_tokens=True
        )[0]
        return transcription_1


class IndicConformer(ASRModel):
    def __init__(self, model_path, decoder="rnnt"):
        self.model = AutoModel.from_pretrained(model_path, trust_remote_code=True).to(
            device
        )
        self.decoder = decoder

    def infer(self, mp3_path):
        wav, sr = torchaudio.load(mp3_path)
        wav = torch.mean(wav, dim=0, keepdim=True)

        target_sample_rate = 16000
        if sr != target_sample_rate:
            resampler = torchaudio.transforms.Resample(
                orig_freq=sr, new_freq=target_sample_rate
            )
            wav = resampler(wav)

        transcription = self.model(wav, "kn", self.decoder)
        return transcription


class SpeachesASRModel(ASRModel):
    def __init__(self, base_url, model_name):
        self.base_url = base_url
        self.model_name = model_name

    def infer(self, mp3_path):
        files = {"file": open(mp3_path, "rb")}
        resp = requests.post(
            f"{self.base_url}/v1/audio/transcriptions",
            files=files,
            json={"model": self.model_name, "language": "kn"},
        )
        output = resp.json()
        return output["text"]

#classes for different Dataset 
class ASRDataset:
    def __init__(self):
        pass

    def save_samples(self, dir_to_save):
        raise NotImplementedError


class SyntheticYoutubeASRDataset(ASRDataset):
    def __init__(self, dataset_path):
        super().__init__()
        self.hf_dataset = load_dataset(dataset_path, split="test")
        self.sample_rate = 16000
        self.hf_dataset = self.hf_dataset.shuffle(seed=42).select(
            range(0, sample_dataset_size)
        )
        self.hf_dataset = self.hf_dataset.cast_column(
            "audio", Audio(sampling_rate=self.sample_rate)
        )
    def __repr__(self):
        return "SyntheticYoutubeASRDataset"
    
    def save_samples(self, dir_to_save):
        json_data = []
        for row in tqdm(self.hf_dataset, desc="processing data"):
            wav = torch.tensor([row["audio"]["array"]])
            filename = os.path.join(dir_to_save, row["audio"]["path"])
            gt = row["prompt"]
            torchaudio.save(filename, wav, self.sample_rate)
            json_data.append({"filename": filename, "ground_truth": gt})

        with open(os.path.join(dir_to_save, "metadata.json"), "w",encoding="utf-8") as fp:
            json.dump(json_data, fp,ensure_ascii=False)


class KathbathASRDataset(ASRDataset):
    def __init__(self, dataset_path):
        self.hf_dataset = load_dataset(dataset_path, "kannada", split="valid")
        self.sample_rate = 16000
        self.hf_dataset = self.hf_dataset.shuffle(seed=42).select(
            range(0, sample_dataset_size)
        )
        self.hf_dataset = self.hf_dataset.cast_column(
            "audio", Audio(sampling_rate=self.sample_rate)
        )
    
    def __repr__(self):
        return "KathbathASRDataset"

    def save_samples(self, dir_to_save):
        json_data = []
        for row in tqdm(self.hf_dataset, desc="processing data"):
            wav = torch.tensor([row["audio_filepath"]["array"]])
            filename = os.path.join(dir_to_save, row["audio_filepath"]["path"])
            gt = row["text"]
            torchaudio.save(filename, wav, self.sample_rate)
            json_data.append({"filename": filename, "ground_truth": gt})

        with open(os.path.join(dir_to_save, "metadata.json"), "w",encoding="utf-8") as fp:
            json.dump(json_data, fp,ensure_ascii=False)


class GoogleTTSASRDataset(ASRDataset):
    def __init__(self, dataset_path):
        self.hf_dataset = load_dataset(dataset_path, split="test")
        self.sample_rate = 16000
        self.hf_dataset = self.hf_dataset.shuffle(seed=42).select(
            range(0, sample_dataset_size)
        )
        self.hf_dataset = self.hf_dataset.cast_column(
            "audio", Audio(sampling_rate=self.sample_rate)
        )
    def __repr__(self):
        return "GoogleTTSASRDataset"

    def save_samples(self, dir_to_save):
        json_data = []
        for row in tqdm(self.hf_dataset, desc="processing data"):
            wav = torch.tensor([row["audio"]["array"]])
            filename = os.path.join(dir_to_save, row["audio"]["path"])
            gt = row["prompt"]
            torchaudio.save(filename, wav, self.sample_rate)
            json_data.append({"filename": filename, "ground_truth": gt})

        with open(os.path.join(dir_to_save, "metadata.json"), "w",encoding="utf-8") as fp:
            json.dump(json_data, fp, ensure_ascii=False)


class ARTPARK_IISC_VANIDataset(ASRDataset):
    dialect_for_vani = [
        "Karnataka_Bangalore",
        "Karnataka_Belgaum",
        "Karnataka_Bellary",
        "Karnataka_Bidar",
        "Karnataka_Bijapur",
        "Karnataka_Chamrajnagar",
        "Karnataka_DakshinKannada",
        "Karnataka_Dharwad",
        "Karnataka_Gulbarga",
        "Karnataka_Koppal",
        "Karnataka_Mysore",
        "Karnataka_Raichur",
        "Karnataka_Shimoga",
    ]

    def __init__(self, dataset_path, subset):
        super().__init__()
        self.hf_dataset = load_dataset(dataset_path, subset, split="train")
        self.hf_dataset = self.hf_dataset.filter(
            lambda row: row["isTranscriptionAvailable"] == "Yes"
        )
        self.sample_rate = 16000
        self.hf_dataset = self.hf_dataset.shuffle(seed=42).select(range(0, 3))
        self.hf_dataset = self.hf_dataset.cast_column(
            "audio", Audio(sampling_rate=self.sample_rate)
        )
    def __repr__(self):
        return "ARTPARK_IISC_VANIDataset"
    
    def save_samples(self, dir_to_save):
        json_data = []
        for row in tqdm(self.hf_dataset, desc="processing data"):
            wav = torch.tensor([row["audio"]["array"]])
            filename = os.path.join(dir_to_save, row["audio"]["path"])
            gt = row["transcript"]
            torchaudio.save(filename, wav, self.sample_rate)
            json_data.append({"filename": filename, "ground_truth": gt})

        with open(os.path.join(dir_to_save, "metadata.json"), "w", encoding="utf-8") as fp:
            json.dump(json_data, fp, ensure_ascii=False)


def evaluate_model(asr_model, asr_dataset):
    dir_to_save = "/home/phoenix/tmp_whatever/stt/inference"
    asr_dataset.save_samples(dir_to_save)
    
    with open(os.path.join(dir_to_save, "metadata.json")) as f:
        json_data = json.load(f)
    errors = []
    character_errors = []
    json_out=[]
    for row in tqdm(json_data, desc="running model"):
        filename = row["filename"]
        gt = row["ground_truth"]
        pred = asr_model.infer(filename)
        row["prediction"] = pred
        json_out.append(row)
        error = wer(gt, pred)
        character_error = cer(gt, pred)
        # print("gt:", gt)
        # print("pred:", pred)
        # print("wer:", error)
        # print("cer:", character_error )
        # print('-------')
        errors.append(error)
        character_errors.append(character_error)
    with open(os.path.join(dir_to_save, f"{asr_dataset}_prediction.json"), "w",encoding='utf-8') as out:
         json.dump(json_data, out, ensure_ascii=False)

    return np.mean(errors), np.mean(character_errors)


if __name__ == "__main__":
    asr_models = {
        # "finetuned_whispher": WhsiperASRModel(
        #     model_path="adithyal1998Bhat/whisper-kn"
        # ),
        # "indic_conformer": IndicConformer(model_path="ai4bharat/indic-conformer-600m-multilingual"),
        # # "whisper-large-v3": SpeachesASRModel(
        # #     base_url="http://100.64.0.7:49827",
        # #     model_name="Systran/faster-whisper-large-v3",
        # # ),
        # "whisper-medium-vaani-kannada": WhsiperASRModel(
        #     model_path="ARTPARK-IISc/whisper-medium-vaani-kannada"
        # ),
        # "whisper-small-vaani-kannada": WhsiperASRModel(
        #     model_path="ARTPARK-IISc/whisper-small-vaani-kannada"
        # ),
        # "base_whisper_50_epochs": WhsiperASRModel(
        #     model_path="/home/phoenix/tmp_whatever/stt/training/model_output_base_model"
        # ),
        # "base_youtube_50_epochs": WhsiperASRModel(
        #     model_path="/home/phoenix/tmp_whatever/stt/training/model_output1"
        # ),
        # "base_youtube_5_epochs": WhsiperASRModel(
        #     model_path="/home/phoenix/tmp_whatever/stt/training/base_youtube_5_epochs"
        # ),
        # "base_whisper_5_epochs": WhsiperASRModel(
        #     model_path="/home/phoenix/tmp_whatever/stt/training/base_whisper_5_epochs"
        # ),
        # "openai/whisper-medium": WhsiperASRModel(
        #     model_path="openai/whisper-medium"
        # )
        # "base_youtube_20_epochs_lr_1e_7": WhsiperASRModel(
        #      model_path="/home/phoenix/tmp_whatever/stt/training/base_youtube_20_epochs_lr_1e_7"
        #  ),
            "base_youtube_30_epochs_lr_1e_7": WhsiperASRModel(
             model_path="/home/phoenix/tmp_whatever/stt/training/base_youtube_30_epochs_lr_1e_7"
         ),
    }

    asr_datasets = {
        "google_tts": GoogleTTSASRDataset("adithyal1998Bhat/tts_synthetic_kn"),
        "youtube_synthetic": SyntheticYoutubeASRDataset(
            "adithyal1998Bhat/stt_synthetic_kn-IN_kannada"
        ),
        "kathbath": KathbathASRDataset("ai4bharat/Kathbath"),
    }

    #asr_datasets['youtube_synthetic'].save_samples('recycle/youtube')
    for dataset_name, asr_dataset in asr_datasets.items():
        for model_name, asr_model in asr_models.items():
            print("model", model_name, "dataset", dataset_name)
            print('========\n'*3)
            error = evaluate_model(asr_model, asr_dataset)
            print(model_name, dataset_name," wer :", error[0]," cer :" ,error[1])
