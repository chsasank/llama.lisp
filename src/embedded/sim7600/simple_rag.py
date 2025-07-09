import requests
import numpy as np
from indicnlp.tokenize.sentence_tokenize import sentence_split as indic_sentence_split
from tqdm import tqdm
import pickle
import bm25s
import Stemmer
from sklearn.metrics.pairwise import cosine_similarity

ollama_base_url = "http://100.64.0.3:58943/"
ollama_token = "J_xpZYOejbRJlFikQVLKHQ"
lang = "mr"
model_name = "gemma2:9b-instruct-q8_0"


def get_embed_text(text_list):
    r = requests.post(
        ollama_base_url + "api/embed",
        headers={"Authorization": f"Bearer {ollama_token}"},
        json={"model": "nomic-embed-text", "input": text_list},
    )
    emb = np.array(r.json()["embeddings"])
    return emb


def parse_knowledge():
    with open("knowledge.txt") as f:
        text = f.read()

    sentences = [x for x in indic_sentence_split(text, lang)]
    num_words_per_sent = np.mean([len(x.split()) for x in sentences])
    chunk_size = 30

    num_sentences_to_merge = round(chunk_size / num_words_per_sent)

    merged_sentences = [
        "\n".join(sentences[idx : idx + num_sentences_to_merge])
        for idx in range(0, len(sentences), num_sentences_to_merge)
    ]
    return merged_sentences


def llm_generate(prompt):
    r = requests.post(
        ollama_base_url + "api/chat",
        headers={"Authorization": f"Bearer {ollama_token}"},
        json={
            "model": model_name,
            "stream": False,
            "messages": [
                {
                    "role": "user",
                    "content": prompt,
                }
            ],
        },
    )
    out = r.json()
    return out["message"]["content"]


def translate_mr2en(marathi_text):
    return llm_generate(
        f"Your job is to translate the below from Marathi to English. Keep it accurate to the source text. Provide only the translation. Do not format to markdown. \n\n {marathi_text}"
    )


def translate_en2mr(english_text):
    return llm_generate(
        f"Your job is to translate the below from English to Marathi. Keep it accurate to the source text. Provide only the translation. Do not format to markdown. \n\n {english_text}"
    )


def get_knowledge_base():
    db_name = "knowledge_base.pkl"

    try:
        with open(db_name, "rb") as f:
            return pickle.load(f)
    except FileNotFoundError:
        our_db = []
        knowledge = parse_knowledge()

        for mr_sent in tqdm(knowledge):
            en_sent = translate_mr2en(mr_sent)
            emb = get_embed_text(en_sent)

            our_db.append([mr_sent, en_sent, emb])

        with open(db_name, "wb") as f:
            pickle.dump(our_db, f)

        return our_db


kb = get_knowledge_base()
corpus = [x[1] for x in kb]
stemmer = Stemmer.Stemmer("english")
corpus_tokens = bm25s.tokenize(corpus, stopwords="en", stemmer=stemmer)
retriever = bm25s.BM25()
retriever.index(corpus_tokens)

def top_k_bm25(en_query, k=20):
    query_tokens = bm25s.tokenize(en_query, stemmer=stemmer)
    results, scores = retriever.retrieve(query_tokens, k=k)
    return list(results[0])


def top_k_embeddings(en_query, bm25_results, k=3):
    query_embeddings = get_embed_text(en_query)
    bm25_embeddings = np.array([kb[x][2][0] for x in bm25_results])
    scores = cosine_similarity(query_embeddings, bm25_embeddings)[0]
    top_k = list(scores.argsort()[::-1][:k])
    top_k_og = [bm25_results[x] for x in top_k]
    return top_k_og


mr_query = "तुम्ही मला घर बांधण्यास कशी मदत करू शकता?"
en_query = translate_mr2en(mr_query)

bm25_results = top_k_bm25(en_query)
emb_results = top_k_embeddings(en_query, bm25_results)


prompt_template = """
You are a helpful assistant working for Nashik Zilla Parishad Government. You are addressing citizen queries. Please provide a detailed answer based solely on the provided sources. When referencing information from a source, cite the appropriate source(s) using their corresponding numbers. Every answer should include at least one source citation. Only cite a source when you are explicitly referencing it. After referencing a source, summarize the relevant part of the source in a line. If none of the sources are helpful, you should indicate that. 
Source 1:
The sky is red in the evening and blue in the morning.
Source 2:
Water is wet when the sky is red.
Query: When is water wet?
Answer: Water will be wet when the sky is red [2], which occurs in the evening [1]. Sky and water is known to be red in evening.
Now it's your turn. Below are several numbered sources of information:
------
{context_str}
------
Query: {query_str}
Answer: 
"""

context_str = ""
window = 2
for idx, doc_idx in enumerate(emb_results):
    doc = '\n'.join([kb[x][1] for x in range(doc_idx - window, doc_idx + window + 1) if x > 0 and x < len(kb)])
    context_str = context_str + f"Source {idx + 1}:\n" + doc

prompt = prompt_template.format(context_str=context_str, query_str=en_query)
out = llm_generate(prompt)
print(mr_query)
print('---')
print(en_query)
print('---')
print(prompt)
print('---')
print('====')
print(out)
print('---')
print(translate_en2mr(out))