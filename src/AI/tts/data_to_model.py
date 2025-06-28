import json
import glob
import os
import random
import requests
from tqdm.auto import tqdm
from indicnlp.tokenize import sentence_tokenize
from datasets import Dataset, Audio, load_dataset, load_from_disk
from google.cloud import texttospeech
from parler_tts import ParlerTTSForConditionalGeneration
import torch
from transformers import AutoTokenizer
import soundfile as sf


def random_sentences(files_list, sample_size):
    random_files_list = random.sample(files_list, sample_size)
    sentence_list = []
    for filename in random_files_list:
        with open(filename, "r") as file_pointer:
            file_contents = json.load(file_pointer)
            indic_string = file_contents["text"]
            sentence_list = sentence_list + sentence_tokenize.sentence_split(
                indic_string, "ka"
            )
    return sentence_list


def data_set_creation(sentence_list, n_samples):
    data_set_list = []
    random_idxs = random.sample(range(len(sentence_list)), n_samples)
    for sentence_indices in random_idxs:
        sent_length = random.choice([1, 2, 3])
        data_set_list.append(
            " ".join(sentence_list[sentence_indices : sentence_indices + sent_length])
        )
    return data_set_list


def gcp_tts(
    line,
    fname,
    language_code="te-IN",
    voice_name="te-IN-Chirp3-HD-Achernar",
    gender="FEMALE",
):
    os.environ["GOOGLE_APPLICATION_CREDENTIALS"] = (
        "/home/phoenix/learning/AI/tts_google/telugu/tts_gcp.json"
    )
    # Instantiates a client
    client = texttospeech.TextToSpeechClient()
    # Set the text input to be synthesized
    synthesis_input = texttospeech.SynthesisInput(text=line)

    # Build the voice request, select the language code ("kn-IN") and the ssml
    # voice gender ("FEMALE")
    # texttospeech.SsmlVoiceGender.FEMALE
    s_ssml_gender = f"texttospeech.SsmlVoiceGender.{gender}"
    voice = texttospeech.VoiceSelectionParams(
        language_code=language_code,
        name=voice_name,
        ssml_gender=texttospeech.SsmlVoiceGender.FEMALE,
    )

    # Select the type of audio file you want returned
    audio_config = texttospeech.AudioConfig(
        audio_encoding=texttospeech.AudioEncoding.MP3
    )

    # Perform the text-to-speech request on the text input with the selected
    # voice parameters and audio file type
    response = client.synthesize_speech(
        input=synthesis_input, voice=voice, audio_config=audio_config
    )

    # The response's audio_content is binary.
    with open(fname, "wb") as out:
        # Write the response to the output file.
        out.write(response.audio_content)


if __name__ == "__main__":

    data_dir = "extracted_data/"
    files_list = glob.glob(os.path.join(data_dir, "*.json"))
    print("total_files are", len(files_list))
    sample_size = 1000
    n_samples = 1500
    fname_to_save = "data_set_list.json"

    try:
        with open(fname_to_save, "r") as f:
            data_set_list = json.load(f)
    except FileNotFoundError:
        sentence_list = random_sentences(files_list, sample_size)
        data_set_list = data_set_creation(sentence_list, n_samples)

        with open(fname_to_save, "w") as f:
            json.dump(data_set_list, f)

    print(
        "Enter the language . For example for kannada it is kn-IN, for Telugu it is te-IN"
    )
    language_code = input()

    print("enter the name of the language")
    language_name = input()
    print("enter the name of the artist")
    voice_name = input()

    print("enter gender of the speaker")
    gender = input()

    for count, sent in enumerate(tqdm(data_set_list)):
        fname = f"generated_tts/{count}.mp3"

        if not os.path.isfile(fname):
            try:
                gcp_tts(sent, fname)
                # print(f"sentence: {sent}, fname: {fname}")
            except:
                print(f"error with {sent}")

    audio_prompts_dict = json.load(open("./data_set_list.json"))
    audio_files = sorted(glob.glob("generated_tts/*.mp3"))
    file_indices = [int(os.path.basename(x)[: -len(".mp3")]) for x in audio_files]
    audio_prompts = [audio_prompts_dict[x] for x in file_indices]
    speaker = voice_name

    # creating audio dataset and pushing it to huggingh face.
    ds = (
        Dataset.from_dict(
            {
                "audio": audio_files,
                "prompt": audio_prompts,
                "description": [f"{speaker} speaks {language_name} in a natural tone"]
                * len(audio_prompts),
            }
        )
        .cast_column("audio", Audio())
        .train_test_split(seed=0)
    )
    ds.save_to_disk(f"tts_ka_dataset_{language_code}_{language_name}")
    ds.push_to_hub(f"chsasank/tts_synthetic_{language_code}_{language_name}")
    HF_dataset_name = f"chsasank/tts_synthetic_{language_code}_{language_name}"
    print("the training should start")
    print(
        f'accelerate launch ./parler-tts/training/run_parler_tts_training.py     --model_name_or_path "ai4bharat/indic-parler-tts"     --report_to "wandb"     --overwrite_output_dir true     --train_dataset_name  {HF_dataset_name}    --train_metadata_dataset_name "chsasank/tts_synthetic"     --train_dataset_config_name "default"     --train_split_name "train"     --eval_dataset_name "chsasank/tts_synthetic"     --eval_dataset_config_name "default"     --eval_metadata_dataset_name "chsasank/tts_synthetic"     --eval_split_name "test"     --max_eval_samples 8     --per_device_eval_batch_size 8     --target_audio_column_name "audio"     --description_column_name "description"     --prompt_column_name "prompt"     --max_duration_in_seconds 20     --min_duration_in_seconds 2.0     --max_text_length 400     --preprocessing_num_workers 2     --do_train true     --num_train_epochs 2     --gradient_accumulation_steps 18     --gradient_checkpointing true     --per_device_train_batch_size 2     --learning_rate 0.0001     --adam_beta1 0.9     --adam_beta2 0.99     --weight_decay 0.01     --lr_scheduler_type "constant_with_warmup"     --warmup_steps 50     --logging_steps 2     --freeze_text_encoder true     --audio_encoder_per_device_batch_size 5     --dtype "float16"     --seed 456     --output_dir "./output_dir_training/"     --temporary_save_to_disk "./audio_code_tmp/"     --save_to_disk "./tmp_dataset_audio/"     --dataloader_num_workers 2     --do_eval     --pred'
    )
    os.system(
        f"""
    accelerate launch ./parler-tts/training/run_parler_tts_training.py \
    --model_name_or_path "ai4bharat/indic-parler-tts" \
    --report_to "wandb" \
    --overwrite_output_dir true \
    --train_dataset_name {HF_dataset_name} \
    --train_metadata_dataset_name {HF_dataset_name} \
    --train_dataset_config_name "default" \
    --train_split_name "train" \
    --eval_dataset_name {HF_dataset_name} \
    --eval_dataset_config_name "default" \
    --eval_metadata_dataset_name {HF_dataset_name} \
    --eval_split_name "test" \
    --max_eval_samples 8 \
    --per_device_eval_batch_size 8 \
    --target_audio_column_name "audio" \
    --description_column_name "description" \
    --prompt_column_name "prompt" \
    --max_duration_in_seconds 20 \
    --min_duration_in_seconds 2.0 \
    --max_text_length 400 \
    --preprocessing_num_workers 2 \
    --do_train true \
    --num_train_epochs 2 \
    --gradient_accumulation_steps 18 \
    --gradient_checkpointing true \
    --per_device_train_batch_size 2 \
    --learning_rate 0.0001 \
    --adam_beta1 0.9 \
    --adam_beta2 0.99 \
    --weight_decay 0.01 \
    --lr_scheduler_type "constant_with_warmup" \
    --warmup_steps 50 \
    --logging_steps 2 \
    --freeze_text_encoder true \
    --audio_encoder_per_device_batch_size 5 \
    --dtype "float16" \
    --seed 456 \
    --output_dir "./parler-tts/output_dir_training/" \
    --temporary_save_to_disk "./audio_code_tmp/" \
    --save_to_disk "./parler-tts/tmp_dataset_audio/" \
    --dataloader_num_workers 2 \
    --do_eval \
    --predict_with_generate \
    --include_inputs_for_metrics \
    --group_by_length true"""
    )
    device = "cuda:0" if torch.cuda.is_available() else "cpu"
    model = ParlerTTSForConditionalGeneration.from_pretrained(
        "./parler-tts/output_dir_training/"
    ).to(device)
    tokenizer = AutoTokenizer.from_pretrained("ai4bharat/indic-parler-tts")
    description_tokenizer = AutoTokenizer.from_pretrained(
        model.config.text_encoder._name_or_path
    )

    print(f"Enter the input prompt in {language_code}")
    prompt = input()
    description = f"{speaker} speaks {language_name} in a natural tone"
    description_input_ids = description_tokenizer(description, return_tensors="pt").to(
        device
    )
    prompt_input_ids = tokenizer(prompt, return_tensors="pt").to(device)

    generation = model.generate(
        input_ids=description_input_ids.input_ids,
        attention_mask=description_input_ids.attention_mask,
        prompt_input_ids=prompt_input_ids.input_ids,
        prompt_attention_mask=prompt_input_ids.attention_mask,
    )
    audio_arr = generation.cpu().numpy().squeeze()
    sf.write("indic_tts_out.wav", audio_arr, model.config.sampling_rate)
