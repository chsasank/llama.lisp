echo "type in the url to .bz2 file"
read url
echo "$url"
wget -O download.xml.bz2 "$url"
filename="download.xml.bz2"
echo "The file to extracted is $filename"
bzip2 -d "$filename"
pip install html2text wikitextparser
mkdir extracted_data

creating the below directory for voices to be stored
mkdir generated_tts
python dewiki_functions.py
python data_to_model.py
