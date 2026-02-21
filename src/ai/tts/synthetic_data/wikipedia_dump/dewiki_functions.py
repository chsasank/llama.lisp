from tqdm import tqdm
from threading import Thread
import json
import re
from html2text import html2text as htt
import wikitextparser as wtp
import glob
import os


def dewiki(text):
    text = wtp.parse(text).plain_text()  # wiki to plaintext
    text = htt(text)  # remove any HTML
    text = text.replace("\\n", " ")  # replace newlines
    text = re.sub("\s+", " ", text)  # replace excess whitespace
    text = re.sub("=+", "=", text)  ##
    return text


def analyze_chunk(text):
    try:
        if '<redirect title="' in text:  # this is not the main article
            return None
        if "(disambiguation)" in text:  # this is not an article
            return None
        else:
            title = text.split("<title>")[1].split("</title>")[0]
            title = htt(title)
            if (
                ":" in title
            ):  # most articles with : in them are not articles we care about
                return None
        serial = text.split("<id>")[1].split("</id>")[0]
        content = text.split("</text")[0].split("<text")[1].split(">", maxsplit=1)[1]
        content = dewiki(content)
        return {"title": title.strip(), "text": content.strip(), "id": serial.strip()}
    except Exception as oops:
        print(oops)
        return None


def save_article(article, savedir):
    doc = analyze_chunk(article)
    if doc:
        filename = doc["id"] + ".json"
        with open(savedir + filename, "w", encoding="utf-8") as outfile:
            json.dump(doc, outfile, sort_keys=True, indent=1, ensure_ascii=False)


def process_file_text(filename, savedir):
    article = ""
    with open(filename, "r", encoding="utf-8") as infile:
        for line in tqdm(infile):
            if "<page>" in line:
                article = ""
            elif "</page>" in line:  # end of article
                Thread(target=save_article, args=(article, savedir)).start()
            else:
                article += line


if __name__ == "__main__":
    import sys

    data_dir = sys.argv[-1]
    filename = glob.glob(os.path.join(data_dir, "*.xml"))
    process_file_text(filename[0], os.path.join(data_dir, "extracted_data/"))
