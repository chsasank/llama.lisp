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
        with open(filename, 'r') as file_pointer:
            file_contents = json.load(file_pointer)   
            indic_string = file_contents['text']
            sentence_list = sentence_list + sentence_tokenize.sentence_split(indic_string, "ka")
    return sentence_list
    
def data_set_creation(sentence_list, n_samples):
    data_set_list = []
    random_idxs = random.sample(range(len(sentence_list)), n_samples)
    for sentence_indices in random_idxs:
        sent_length = random.choice([1,2,3])
        data_set_list.append(' '.join(sentence_list[sentence_indices: sentence_indices + sent_length]))
    return data_set_list

def gcp_tts(line, fname):
    os.environ["GOOGLE_APPLICATION_CREDENTIALS"]="/home/phoenix/learning/AI/tts_google/telugu/tts_gcp.json"
    # Instantiates a client
    client = texttospeech.TextToSpeechClient()
    # Set the text input to be synthesized
    synthesis_input = texttospeech.SynthesisInput(text=line)

    # Build the voice request, select the language code ("kn-IN") and the ssml
    # voice gender ("FEMALE")

    voice = texttospeech.VoiceSelectionParams(
        language_code="te-IN", name="te-IN-Chirp3-HD-Achernar" ,ssml_gender=texttospeech.SsmlVoiceGender.FEMALE
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
    print(files_list)
    sample_size = 1000
    n_samples = 1500
    fname_to_save = 'data_set_list.json'

    try:
        with open(fname_to_save, 'r') as f:
            data_set_list = json.load(f)
    except FileNotFoundError:
        sentence_list = random_sentences(files_list, sample_size)
        data_set_list = data_set_creation(sentence_list, n_samples)

        with open(fname_to_save, 'w') as f:
            json.dump(data_set_list, f)



    for count, sent in enumerate(tqdm(data_set_list)):
        fname = f"generated_tts/{count}.mp3"
        if not os.path.isfile(fname):
            try:
                gcp_tts(sent, fname)
                # print(f"sentence: {sent}, fname: {fname}")
            except:
                print(f"error with {sent}")



    audio_prompts_dict = json.load(open('./data_set_list.json'))

    for count, sent in enumerate(tqdm(data_set_list)):
        fname = f"generated_tts/{count}.mp3"

        if not os.path.isfile(fname):
            try:
                gcp_tts(sent, fname)
                # print(f"sentence: {sent}, fname: {fname}")
            except:
                print(f"error with {sent}")


    audio_prompts_dict = json.load(open('./data_set_list.json'))
    audio_files = sorted(glob.glob('generated_tts/*.mp3'))
    file_indices = [int(os.path.basename(x)[:-len('.mp3')]) for x in audio_files]
    audio_prompts = [audio_prompts_dict[x] for x in file_indices]
    print("Input the name of the speaker")
    speaker = input()

    #creating audio dataset and pushing it to huggingh face. 
    ds = Dataset.from_dict({
        "audio": audio_files,
        "prompt": audio_prompts,
        "description": [f'{speaker} speaks Kannada in a natural tone'] * len(audio_prompts)
    }).cast_column("audio", Audio()).train_test_split(seed=0)
    ds.save_to_disk('tts_ka_dataset')
    ds.push_to_hub('chsasank/tts_synthetic')



    device = "cuda:0" if torch.cuda.is_available() else "cpu"
    model = ParlerTTSForConditionalGeneration.from_pretrained('./parler-tts/output_dir_training/').to(device)
    tokenizer = AutoTokenizer.from_pretrained("ai4bharat/indic-parler-tts")
    description_tokenizer = AutoTokenizer.from_pretrained(model.config.text_encoder._name_or_path)
    prompt = "ಸರಸ್ವತಿ ಹಿಂದೂ ಧರ್ಮದಲ್ಲಿ ಜ್ಞಾನ ಮತ್ತು ಕಲೆಗಳ ದೇವತೆ. ಸರಸ್ವತಿಯನ್ನು ಬ್ರಹ್ಮದೇವರ ಪತ್ನಿಯೆಂದೂ, ಶ್ವೇತವಸ್ತ್ರಗಳನ್ನು ಧರಿಸಿದವಳೆಂದೂ ಪೂಜಿಸುವರು."
    description = f'{speaker} speaks Kannada in a natural tone'
    description_input_ids = description_tokenizer(description, return_tensors="pt").to(device)
    prompt_input_ids = tokenizer(prompt, return_tensors="pt").to(device)
    
    generation = model.generate(input_ids=description_input_ids.input_ids, attention_mask=description_input_ids.attention_mask, prompt_input_ids=prompt_input_ids.input_ids, prompt_attention_mask=prompt_input_ids.attention_mask)
    audio_arr = generation.cpu().numpy().squeeze()
    sf.write("indic_tts_out.wav", audio_arr, model.config.sampling_rate)



