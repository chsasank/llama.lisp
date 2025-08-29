import datasets
from tqdm import tqdm
from datasets import load_dataset
import json
from number_normalization import number_normalization
import re
import random
import torch
import soundfile as sf
import glob, os
from indicnlp.tokenize import sentence_tokenize

def make_dirs(path):
    if not os.path.exists(path):
        os.makedirs(path)
        print(path, "created")
    

def random_sentences(files_list, sample_size):
    random_files_list = random.sample(files_list, sample_size)
    sentence_list = []
    for filename in tqdm(random_files_list, desc="executing sentence_tokenize"):
        with open(filename, "r") as file_pointer:
            file_contents = json.load(file_pointer)
            indic_string = file_contents["text"]
            sentence_list = sentence_list + sentence_tokenize.sentence_split(
                indic_string, "ka"
            )
    return sentence_list

#the below function data_set_creation is used to create a list of sentences from sentence_list. This is prior to cleaning of the data.
def data_set_creation(sentence_list, n_samples):
    data_set_list = []
    random_idxs = random.sample(range(len(sentence_list)), n_samples)
    for sentence_indices in tqdm(
        random_idxs, desc="creating a mix of 1 senetences "
    ):
        sent_length = random.choice([1])
        data_set_list.append(
            " ".join(sentence_list[sentence_indices : sentence_indices + sent_length])
        )
    return data_set_list


def clean_text(text):
    process_text = text
    char_to_remove = ["=", "*", "[]", "#"]
    char_to_seperate = ["-", "+", "/"]
    char_to_replace = {"%": " ಪರ್ಸಂಟೇಜ್ ", "+": " ಪ್ಲಸ್ ", "-": " ಮೈನಸ್ "}
    regex = r"\d+."
    end_of_line_regex = r"\d+\."

    for char in tqdm(char_to_remove, desc="removing char =, *, []"):
        process_text = process_text.replace(char, "")

    for char in tqdm(char_to_seperate, desc="seperating  - and + some space"):
        process_text = process_text.replace(char, f" {char} ")

    numbers_list = re.findall(regex, process_text)
    if len(numbers_list) > 1:
        for num in numbers_list:
            process_text = process_text.replace(num, num.split(".")[0], 1)

    full_stop_seperation = re.findall(end_of_line_regex, process_text)
    if full_stop_seperation:
        for charecter in tqdm(full_stop_seperation, desc="separating . from number."):
            process_text = process_text.replace(
                charecter, " .".join(charecter.split("."))
            )

    for symbol in tqdm(char_to_replace.keys(), desc="replacing from symbols to words"):
        process_text = process_text.replace(
            symbol, char_to_replace[symbol]
        )
    return process_text


def cleaned_prompt_dataset(dataset):
    clean_data = []
    count = 0
    for text in tqdm(dataset, desc="cleaning _prompt _dataset"):
        if "http" in text:
            count = count + 1
        else:
            clean_data.append(clean_text(text))
    return clean_data


def num_normalization(number):
    number = number.replace(",", "")
    if "." in number:
        normalized_num = " point ".join(
            [
                num
                for num in [
                    number_normalization(number_token, len(number_token))
                    for number_token in number.split(".")
                ]
                if num is not None
            ]
        )
    else:
        normalized_num = number_normalization(number, len(number))

    return normalized_num


def num_in_sentence_normalization(clean_data):
    num_normalized_data = []
    re_numbers = r"\d[\d.,]*\d*"
    for sentence in clean_data:
        numbers_list = re.findall(re_numbers, sentence)
        if len(numbers_list) > 0:
            for num in numbers_list:
                try:
                    replace_text = num_normalization(num)
                    sentence = sentence.replace(num, replace_text, 1)
                except:
                    pass
        num_normalized_data.append(sentence.lstrip())
    return num_normalized_data

#The below function takes random.sample from num_normalized_data
def dataset_creation(num_normalized_data, n_samples):
    dataset_list = []
    random.seed(a=1000)
    if n_samples > len(num_normalized_data):
        n_samples = len(num_normalized_data)

    random_idx = random.sample(range(len(num_normalized_data)), n_samples)
    for sentence_idx in random_idx:
        length_of_sentence = random.choice([1])
        dataset_list.append(
            " ".join(
                num_normalized_data[sentence_idx : sentence_idx + length_of_sentence],
            ).lstrip()
        )

    return dataset_list


if __name__ == "__main__":
    import sys

    data_dir = sys.argv[-1]
    files_list = glob.glob(os.path.join(data_dir, "wikipedia_dump/extracted_data/*.json"))

    print("total_files are", len(files_list))
    sample_size = 30000
    n_sample_sentences = 30000

    # sample the wikipedia data
    data_set_file = os.path.join(
        data_dir, "text_cleaning", "data_set_list_kannada_wiki.json"
    )

    try:
        with open(data_set_file, "r") as f:
            data_set_list = json.load(f)
    except FileNotFoundError:
        make_dirs(os.path.join(data_dir, "text_cleaning"))
        sentence_list = random_sentences(files_list, sample_size)
        data_set_list = data_set_creation(sentence_list, n_sample_sentences)
        with open(data_set_file, "w", encoding="utf-8") as f:
            json.dump(data_set_list, f, ensure_ascii=False)

    # clean the wikipedia text
    clean_dataset_json = os.path.join(
        data_dir, "text_cleaning", "data_set_list_kannada_wiki_clean.json"
    )
    try:
        with open(clean_dataset_json, "r") as f:
            cleaned_data = json.load(f)
    except FileNotFoundError:
        cleaned_data = cleaned_prompt_dataset(data_set_list)
        with open(clean_dataset_json, "w", encoding="utf-8") as f:
            json.dump(cleaned_data, f, ensure_ascii=False)

    # normalize all the numbers
    num_normal_dataset_json = os.path.join(
        data_dir, "text_cleaning", "data_set_list_kannada_wiki_num_normalized.json"
    )
    num_normalized_data = num_in_sentence_normalization(cleaned_data)
    with open(
        num_normal_dataset_json,
        "w",
        encoding="utf-8",
    ) as f:
        json.dump(num_normalized_data, f, ensure_ascii=False)

    # create final dataset to use
    final_data_set_json = os.path.join(
        data_dir, "text_cleaning", "data_set_list_kannada_wiki_final_dataset.json"
    )
    with open(
        final_data_set_json,
        "w",
        encoding="utf-8",
    ) as f:
        final_dataset = dataset_creation(num_normalized_data, n_sample_sentences)
        json.dump(final_dataset, f, ensure_ascii=False)
