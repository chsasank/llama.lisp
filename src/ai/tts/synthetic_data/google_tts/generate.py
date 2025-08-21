import json
import glob
import os
import random
import requests
import torchaudio
from tqdm.auto import tqdm
from indicnlp.tokenize import sentence_tokenize
from datasets import Dataset, Audio, load_dataset, load_from_disk
from google.cloud import texttospeech
import torch
from transformers import AutoTokenizer
import soundfile as sf
import math

assert "GOOGLE_APPLICATION_CREDENTIALS" in os.environ


def gcp_tts(
    line,
    fname,
    language_code="te-IN",
    voice_name="te-IN-Chirp3-HD-Achernar",
    gender="FEMALE",
):

    mp3_duration = 0

    # Instantiates a client
    client = texttospeech.TextToSpeechClient()
    # Set the text input to be synthesized
    synthesis_input = texttospeech.SynthesisInput(text=line)
    line_char = len(line)
    # Build the voice request, select the language code ("kn-IN") and the ssml
    # voice gender ("FEMALE")
    # texttospeech.SsmlVoiceGender.FEMALE
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
    metadata = torchaudio.info(fname)
    mp3_duration = math.floor(metadata.num_frames / metadata.sample_rate)

    with open(f'{fname.split(".mp3")[0]}.json', "w", encoding="utf-8") as f:
        json.dump(line, f, ensure_ascii=False)
    return mp3_duration, line_char


if __name__ == "__main__":
    import sys

    data_dir = sys.argv[-1]

    final_data_set_json = os.path.join(
        data_dir, "text_cleaning", "data_set_list_kannada_wiki_final_dataset.json"
    )
    with open(final_data_set_json, "r") as f:
        data_set_list = json.load(f)

    # max data to generate
    max_duration_in_seconds = 23 * 60 * 60

    # Enter the language . For example for kannada it is kn-IN, for Telugu it is te-IN"
    language_code = "kn-IN"

    # enter the name of the language
    language_name = "kannada"

    # enter the name of the artist
    voice_name = "saraswathi"

    # enter gender of the speaker
    gender = "FEMALE"

    total_characters = 0
    total_duration = 0
    for count, sent in enumerate(tqdm(data_set_list)):
        if count % 10 == 0:
            print(f"generated {total_duration} seconds of data with {count} prompts")

        if total_duration > max_duration_in_seconds:
            break
        fname = os.path.join(data_dir, "generated_tts", f"{count}.mp3")

        if not os.path.isfile(fname):
            try:
                mp3_duration, line_char = gcp_tts(sent, fname)
                total_duration = total_duration + mp3_duration
                total_characters = total_characters + line_char
            except Exception:
                print(f"error with {sent}")
        else:
            meta = torchaudio.info(fname)
            total_duration = total_duration + math.floor(
                meta.num_frames / meta.sample_rate
            )
            total_characters = total_characters + len(sent)

    print(f"total audio generated for {total_duration} seconds")
    print(f"total characters of text is {total_characters}")

    audio_prompts_dict = data_set_list
    audio_files = sorted(glob.glob(os.path.join(data_dir, "generated_tts/*.mp3")))
    file_indices = [int(os.path.basename(x)[: -len(".mp3")]) for x in audio_files]
    audio_prompts = [audio_prompts_dict[x] for x in file_indices]
    speaker = voice_name

    # # creating audio dataset and pushing it to huggingh face.
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
    print(ds)
    ds.save_to_disk(
        os.path.join(
            data_dir, "hf_dataset/tts_num_numerized_{language_code}_{language_name}"
        )
    )
    ds.push_to_hub("chsasank/tts_kn_large")

