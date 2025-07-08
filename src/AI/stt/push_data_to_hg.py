import os 
import glob
from datasets import Dataset, Audio, load_dataset, load_from_disk
mypath = "/home/phoenix/llama/llama.lisp/src/AI/stt/audio_dataset_torch_audio"
mypath_text = "/home/phoenix/llama/llama.lisp/src/AI/stt/text_dataset_torch_text"
audio_files = sorted(glob.glob(os.path.join(mypath, "*.mp3")))
audio_prompt_files = sorted(glob.glob(os.path.join(mypath_text , "*.txt")))


with open("check_audio_files.txt", "w") as f:
    for order in audio_files:
        f.write((order.split(".mp3")[0]).split("/home/phoenix/llama/llama.lisp/src/AI/stt/audio_dataset_torch_audio/")[1]+ "\n")



with open("audio_prompt_files.txt", "w") as f:
    for order in audio_prompt_files:
        f.write((order.split(".txt")[0]).split("/home/phoenix/llama/llama.lisp/src/AI/stt/text_dataset_torch_text/")[1]+ "\n")
    

audio_prompt = list()
for file_name in audio_prompt_files:
    with open(file_name,"r") as f:
        a = f.read()
        audio_prompt.append(a)
    
ds = (
    Dataset.from_dict(
        {
            "audio": audio_files,
            "prompt": audio_prompt,
            }
        )
        .cast_column("audio", Audio())
        .train_test_split(seed=0)
    )
ds.save_to_disk("stt_ka_dataset_kn-IN_kannada")
ds.push_to_hub("adithyal1998Bhat/stt_synthetic_kn-IN_kannada")
