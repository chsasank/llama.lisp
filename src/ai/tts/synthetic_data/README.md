1. Download wikipedia dump

```
cd wikipedia_dump
bash download_wiki.sh https://dumps.wikimedia.org/knwiki/20250620/knwiki-20250620-pages-articles-multistream.xml.bz2 /tmp/tts_data/
```

2. Clean the downladed data

```
cd ../text_cleaning
python clean.py /tmp/tts_data/
```

3. Generate Google TTS

```
cd ../google_tts
python generate.py /tmp/tts_data/
```