
import pkg_resources
import os
pkg_resources.require("transformers==4.48.0")
os.system("pip install -U 'protobuf>=3.4.0'")
import transformers
from huggingface_hub import notebook_login
from datasets import load_dataset, DatasetDict
from transformers import WhisperFeatureExtractor 
from transformers import WhisperTokenizer
from transformers import WhisperForConditionalGeneration
import torch
from dataclasses import dataclass
from typing import Any, Dict, List, Union
import evaluate
from transformers import Seq2SeqTrainingArguments, Seq2SeqTrainer, WhisperTokenizer, WhisperProcessor
from datasets import Audio 
from transformers import pipeline
import gradio as gr

def sample(ds, percentage=1):
    if percentage >= 1:
        return ds

    return ds.select(range(int(percentage * len(ds))))

#os.system("huggingface-cli ....." mind that you should be logged into hugging face either through the script or 
HF_dataset_name = "adithyal1998Bhat/stt_synthetic_kn-IN_kannada"
common_voice = DatasetDict()
common_voice["train"] = sample(load_dataset(HF_dataset_name, split="train"))
common_voice["test"] = sample(load_dataset(HF_dataset_name, split="test"), 0.05)

base_model_name = "openai/whisper-medium"
feature_extractor = WhisperFeatureExtractor.from_pretrained(base_model_name)

#what does the tokenize do here
tokenizer = WhisperTokenizer.from_pretrained(base_model_name, language="Kannada", task="transcribe")
processor = WhisperProcessor.from_pretrained(base_model_name, language="Kannada", task="transcribe")
common_voice = common_voice.cast_column("audio", Audio(sampling_rate=16000))

def prepare_dataset(batch):
    audio = batch["audio"]
    #compute log-mel input features from input  audio array. Why?
    batch["input_features"] = feature_extractor(audio["array"],sampling_rate=audio["sampling_rate"]).input_features[0]

    #encode target text to label ids
    batch["labels"] = tokenizer(batch["prompt"]).input_ids
    return batch 

common_voice = common_voice.map(prepare_dataset, num_proc=2, writer_batch_size=20)


model = WhisperForConditionalGeneration.from_pretrained(base_model_name)


def drop_large_sentences(ds):
    # drop samples with tokens supported by the model
    max_size = model.config.max_target_positions
    idxs_small = [idx for idx, d in enumerate(ds['labels']) if len(d) < max_size]
    print("dropping {} samples with large sentences".format(len(ds) - len(idxs_small)))
    return ds.select(idxs_small)


common_voice['train'] = drop_large_sentences(common_voice['train'])
common_voice['test'] = drop_large_sentences(common_voice['test'])
print(common_voice)


model.generation_config.language = "Kannada"
model.generation_config.task = "transcribe"
model.generation_config.forced_decoder_ids = None

@dataclass
class DataCollatorSpeechSeq2SeqWithPadding:
    processor: Any
    decoder_start_token_id: int

    def __call__(self, features: List[Dict[str, Union[List[int], torch.Tensor]]]) -> Dict[str, torch.Tensor]:
        input_features = [{"input_features": feature["input_features"]} for feature in features]
        batch = self.processor.feature_extractor.pad(input_features, return_tensors="pt")

        label_features = [{"input_ids": feature["labels"]} for feature in features]
        labels_batch = self.processor.tokenizer.pad(label_features, return_tensors="pt")

        labels = labels_batch["input_ids"].masked_fill(labels_batch.attention_mask.ne(1), -100)

        #what is bos?
        if(labels[:,0] == self.decoder_start_token_id).all().cpu().item():
            labels = labels[:, 1:]

        batch["labels"] = labels
        return batch
    
data_collator = DataCollatorSpeechSeq2SeqWithPadding(processor=processor, decoder_start_token_id=model.config.decoder_start_token_id,)
metric = evaluate.load("wer")

def compute_metrics(pred):
    pred_ids = pred.predictions
    label_ids = pred.label_ids

    label_ids[label_ids == -100] = tokenizer.pad_token_id

    #no grouping token when we are computing metrics
    pred_str = tokenizer.batch_decode(pred_ids, skip_special_tokens=True)
    label_str = tokenizer.batch_decode(label_ids, skip_special_tokens=True)

    wer = 100 * metric.compute(predictions=pred_str, references=label_str)
    return {"wer":wer}


training_args = Seq2SeqTrainingArguments(
    output_dir="./whisper-kn",
    per_device_train_batch_size=1,
    gradient_accumulation_steps=16,
    learning_rate=1e-5,
    warmup_steps=500,
    max_steps=10000,
    gradient_checkpointing=True,
    fp16=True,
    eval_strategy="steps",
    per_device_eval_batch_size=1,
    predict_with_generate=True,
    generation_max_length=400,
    save_steps=1000,
    eval_steps=1000,
    logging_steps=25,
    report_to=["tensorboard"],
    load_best_model_at_end=True,
    metric_for_best_model="wer",
    greater_is_better=False,
    push_to_hub=True,
)
trainer = Seq2SeqTrainer(
    args = training_args,
    model=model,
    train_dataset=common_voice["train"],
    eval_dataset=common_voice["test"],
    data_collator=data_collator,
    compute_metrics=compute_metrics,
    tokenizer=processor.feature_extractor,)

 
processor.save_pretrained(training_args.output_dir)
trainer.train(resume_from_checkpoint=True)

kwargs = {
    "dataset_tags": HF_dataset_name,
    "dataset": "kannada voices",  # a 'pretty' name for the training dataset
    "dataset_args": "config: kn, split: test",
    "language": "kn",
    "model_name": "Whisper Small kn - Saraswathi",  # a 'pretty' name for our model
    "finetuned_from": "ope100whisper-small",
    "tasks": "automatic-speech-recognition",
}

trainer.push_to_hub(**kwargs)

