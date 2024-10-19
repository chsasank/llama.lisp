import requests
import time
from openai import OpenAI
from pydub import AudioSegment
import io


# vllm server
client = OpenAI(
    base_url="http://100.64.0.8:58643/v1",
    api_key="V9ykRN7LgozZni8Qf3TguQ"
)

openweb_ui_base = "http://100.64.0.8:45327"

def speech_to_text(dub):
    pcm_data = io.BytesIO()
    dub.export(pcm_data, format="mp3")

    headers = {
        'accept': 'application/json',
        'Authorization': 'Bearer sk-0f47bc66a6fd408bb9808371fd17dc43',
    }
    files = {'file': ('Models_Etc.mp3', pcm_data, 'audio/mpeg'),}
    response_stt = requests.post(f'{openweb_ui_base}/audio/api/v1/transcriptions', headers=headers, files=files)
    response_stt.raise_for_status()
    return response_stt.json()['text']


def text_to_speech(text):
    json_data = {
        "model": "tts-1-hd",
        "input": text,
        'voice': 'fable',
        "response_format": "mp3",
        "speed": 1.0,
    }
    headers = {
        'Authorization': 'Bearer sk-0f47bc66a6fd408bb9808371fd17dc43',
        'accept': 'application/json',
        'content-type': 'application/x-www-form-urlencoded',
    }
    response_from_tts = requests.post(f'{openweb_ui_base}/audio/api/v1/speech', headers=headers, json=json_data, timeout=5)
    dub = AudioSegment.from_file(io.BytesIO(response_from_tts.content), format="mp3")
    return dub


def speech_to_speech_stream(dub):
    prompt = speech_to_text(dub)
    print("prompt:", prompt)
    
    # stream chat completions
    resp = client.chat.completions.create(
        model='neuralmagic/Meta-Llama-3.1-8B-Instruct-FP8',
        messages = [{
            "role": "user",
            "content": prompt
            }],
        stream=True)

    chunk_size = 20
    chunks = []
    now = time.time()
    for idx, resp_chunk in enumerate(resp):
        content = resp_chunk.choices[0].delta.content
        chunks.append(content)
        if (idx % chunk_size == 0) and (idx > 0):
            text = "".join(chunks)
            print("output:", text)
            audio = text_to_speech(text)
            chunks = []
            yield audio
    
    text = "".join(chunks)
    print("output:", text)
    audio = text_to_speech(text)
    yield audio