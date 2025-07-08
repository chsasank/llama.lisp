import json
file_list = {
    "url_kannada_movies_list.txt": "movie",
    "url_news_playlist.txt": "news",
    "url_serial_playlist.txt": "serial"
}

data = []
for file_name, data_type in file_list.items():
    with open(file_name, "r") as f: 
        lines = f.read().strip().split('\n')
        data.extend([{'video': line, 'type': data_type} for line in lines])

with open("kannada_asr_data.json", 'w') as f:
    json.dump(data, f)
