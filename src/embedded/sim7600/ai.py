import requests
import time
from openai import OpenAI
from pydub import AudioSegment
import io


# vllm server
client = OpenAI(base_url="http://100.64.0.8:58643/v1", api_key="V9ykRN7LgozZni8Qf3TguQ")

openweb_ui_base = "http://100.64.0.8:45327"


def speech_to_text(dub):
    dub = dub.set_frame_rate(16000)  # Set sample rate to 16000 Hz
    dub = dub.set_channels(1)  # Convert to mono
    dub = dub.set_sample_width(2)  # Set to 16-bit PCM (2 bytes per sample)

    pcm_data = io.BytesIO()
    dub.export(pcm_data, format="wav")

    headers = {
        "Content-type ": "multipart/form-data",
    }
    files = {
        "file": pcm_data,
        "response-format": (None, "json"),
    }

    response = requests.post(
        "http://100.64.0.8:12000/inference", headers=headers, files=files
    )

    response.raise_for_status()
    return response.json()["text"]


def text_to_speech(text):
    json_data = {
        "model": "tts-1-hd",
        "input": text,
        "voice": "fable",
        "response_format": "mp3",
        "speed": 1.0,
    }
    headers = {
        "Authorization": "Bearer sk-0f47bc66a6fd408bb9808371fd17dc43",
        "accept": "application/json",
        "content-type": "application/x-www-form-urlencoded",
    }
    response_from_tts = requests.post(
        f"{openweb_ui_base}/audio/api/v1/speech",
        headers=headers,
        json=json_data,
        timeout=5,
    )
    dub = AudioSegment.from_file(io.BytesIO(response_from_tts.content), format="mp3")
    return dub


def chat_stream(messages):
    # stream chat completions
    resp = client.chat.completions.create(
        model="neuralmagic/Meta-Llama-3.1-8B-Instruct-FP8",
        messages=messages,
        stream=True,
    )

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
            yield audio, text

    if chunks:
        text = "".join(chunks)
        print("output:", text)
        audio = text_to_speech(text)
        yield audio, text
