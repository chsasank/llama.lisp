# Download wikipedia text dump and extract the data
# example: bash download_wiki.sh https://dumps.wikimedia.org/knwiki/20250620/knwiki-20250620-pages-articles-multistream.xml.bz2 /tmp/tts_data/
set -e

url=$1
data_dir=$2/wikipedia_dump
mkdir -p "$data_dir"
if [ -z "$2" ]
  then
    echo "Incorrect arguments supplied"
    exit 1
fi


echo "downloading $url data into $data_dir"
filename="$data_dir/download.xml.bz2"
wget -O "$filename" "$url"

echo "The file to be extracted is $filename"
bzip2 -d "$filename"

python3 -m pip install html2text wikitextparser

mkdir -p $data_dir/extracted_data
echo "the directory for extracted text to be stored is $data_dir/extracted_data"

python3 dewiki_functions.py $data_dir
