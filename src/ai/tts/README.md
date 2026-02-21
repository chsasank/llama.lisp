
# How We Fine-Tuned a Kannada TTS Model - Step by Step

> Authors: Sreeram Sridhar, Adithya L Bhat, Sasank Chilamkurthy

We wanted to fine-tune a Kannada TTS model using openly available data and open tools. Here's exactly how we did it from Wikipedia dump to a trained model.

# Synthetic Data Generation

Code to run this in `synthetic_data` folder. We document the process here.

## 1. Get Raw Kannada Text from Wikipedia

We downloaded the Kannada Wiktionary dump.

-   Go to the Wikipedia database download page: https://en.wikipedia.org/wiki/Wikipedia:Database_download
-   Navigate to the dump site:https://dumps.wikimedia.org/
-   For a list of all available dumps: https://dumps.wikimedia.org/backup-index.html
-   For Kannada wikipedia (example date 2025-08-01): https://dumps.wikimedia.org/knwiki/20250801/
-   One of the Kannada .bx2 file is https://dumps.wikimedia.org/knwiki/20250801/knwiki-20250801-pages-articles-multistream.xml.bz2    
-   Download the .bz2 file, extract it to get XML files containing the raw wiki content.
      
    

## 2. Convert XML to Plain Text

We used this GitHub repo to convert Wikipedia XML to plain text: https://github.com/daveshap/PlainTextWikipedia
    
This tool parses XML and removes wiki markup to produce readable plain text in JSON format.

## 3. Clean and Split the Text into Sentences

We used the Indic NLP library to split the large text corpus into clean Kannada sentences. Indic NLP Library (tokenizer): https://github.com/anoopkunchukuttan/indic_nlp_library
    

Steps:

-   Load the text into a Python list.  
      
    
-   Use the Indic NLP sentence tokenizer to break paragraphs into sentences. It handles common full stops, abbreviations like “Dr.”, and other punctuation reasonably well.  
      
    
-   After tokenization, we had a list of sentences like: [sentence_1, sentence_2, ...].
    

## 4. Random Sampling for Fairness

To avoid bias, we sampled sentences randomly from the list using Python:

```
import random
sampled_sentences = random.sample(all_sentences, k=1000)
```
  

We also created grouped variations:

-   1 sentence     
-   2 sentences together  
-   3 sentences together  

This helps the model learn from input sequences of varying lengths.

## 5. Generate Audio with Google TTS

We used Google Cloud’s Text-to-Speech API to generate synthetic audio for each sentence.

-   Google TTS API docs: https://cloud.google.com/text-to-speech/docs/list-voices-and-types
    

We used:

-   Language: Kannada (India)  
-   Voice: kn-IN-Chirp3-HD-Achernar  
-   Type: Premium, Female  
      
Each sentence was converted into an .mp3 audio file. We ensured that filenames matched:

```
001.json ↔ 001.mp3

002.json ↔ 002.mp3

...
```
  

Note: Google TTS supports up to 5000 characters per request. Make sure grouped sentence blocks stay within this limit.

## 6. Create a Hugging Face Dataset

We structured the data in a format compatible with Hugging Face datasets, similar to this example:

-   https://huggingface.co/datasets/chsasank/tts_kn_large
    

Our dataset included:

-   audio: path to .mp3 file      
-   prompt: corresponding text sentence(s)  
-   description: optional metadata  
    
# Model Training


## 1. Fine-Tune the Model

We fine-tuned the [AI4Bharat Indic Parler-TTS model](https://huggingface.co/ai4bharat/indic-parler-tts): https://huggingface.co/ai4bharat/indic-parler-tts
    

We followed the steps from this notebook for single-speaker TTS fine-tuning:

-   https://github.com/ylacombe/scripts_and_notebooks/blob/main/Finetuning_Parler_TTS_v1_on_a_single_speaker_dataset.ipynb
    

Training details:

-   Dataset: our synthetic Kannada dataset  
-   GPU: NVIDIA 4070 Ti Super (16GB VRAM)  
-   Training time: approximately 15–20 minutes (~25 hrs fine-tuning)  

Exact command we used for our final run is below

```
accelerate launch training/run_parler_tts_training.py \
    --model_name_or_path "ai4bharat/indic-parler-tts" \
    --overwrite_output_dir true \
    --train_dataset_name "chsasank/tts_kn_large" \
    --train_metadata_dataset_name "chsasank/tts_kn_large" \
    --train_dataset_config_name "default" \
    --train_split_name "train" \
    --eval_dataset_name "chsasank/tts_kn_large" \
    --eval_dataset_config_name "default" \
    --eval_metadata_dataset_name "chsasank/tts_kn_large" \
    --eval_split_name "test" \
    --per_device_eval_batch_size 1 \
    --per_device_train_batch_size 1 \
    --target_audio_column_name "audio" \
    --description_column_name "description" \
    --prompt_column_name "prompt" \
    --max_train_samples 1900\
    --max_duration_in_seconds 30 \
    --min_duration_in_seconds 2.0 \
    --max_text_length 700 \
    --preprocessing_num_workers 2 \
    --do_train true \
    --num_train_epochs 10 \
    --gradient_accumulation_steps 36 \
    --gradient_checkpointing true \
    --learning_rate 0.0002 \
    --adam_beta1 0.9 \
    --adam_beta2 0.99 \
    --weight_decay 0.01 \
    --lr_scheduler_type "cosine" \
    --warmup_steps 30 \
    --logging_steps 2 \
    --freeze_text_encoder true \
    --audio_encoder_per_device_batch_size 5 \
    --dtype "float16" \
    --seed 456 \
    --output_dir "/tmp/tts_data/training/model_outputs" \
    --temporary_save_to_disk "/tmp/tts_data/training/temp" \
    --save_to_disk "/tmp/tts_data/training/temp_audio" \
    --include_inputs_for_metrics \
    --do_eval \
    --group_by_length true | tee /tmp/tts_data/logs/exp_final_logs.txt
```
NOTE: Directory /tmp/tts_data has to be created by the user. 

We used Hugging Face’s datasets and transformers libraries to load the dataset and train the model.

## 2. Test the Trained Model

After training, we tested the model by giving new Kannada text inputs and generating speech using the trained model. The same GitHub repo provides sample scripts to run inference locally.

Inference code is coming up shortly.

## Final Outcome

We now have a fine-tuned Kannada TTS model capable of generating realistic speech for synthetic or real input. This project shows that:

-   You can start with openly available text.  
-   You don’t need a large team or massive compute resources.
-   Just a few tools, some scripting, and one decent GPU is enough to get started.  
      

If you want to build your own TTS model in your language, the path is open and doable


# Execution
## Preparing the data
Run the data_set.sh as shown below. 

```
bash download_wiki.sh https://dumps.wikimedia.org/knwiki/20250620/knwiki-20250620-pages-articles-multistream.xml.bz2 /tmp/tts_data

```

Here /tmp/tts_data is the directory where the text wikipedia data is downloaded and extracted to. 

Prepare the data

```
python3 /synthetic_data/text_cleaning/clean.py /tmp/tts_data/
```