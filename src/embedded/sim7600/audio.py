import serial
import time
from pydub import AudioSegment
from pydub.silence import detect_leading_silence
import io

#     audio = AudioSegment.from_file(mp3_fname, format="mp3").low_pass_filter(1000)
#     raw_audio.export(mp3_fname, format="mp3")

def dub2pcm(audio):
    audio = audio.set_frame_rate(8000)  # Set to 8000 Hz sample rate
    audio = audio.set_channels(1)       # Set to mono
    audio = audio.set_sample_width(2)  # Set to 16-bit samples

    pcm_data = io.BytesIO()
    audio.export(pcm_data, format="raw")
    return pcm_data


def pcm2dub(pcm_data):
    pcm_data.seek(0)
    raw_audio = AudioSegment(
        data=pcm_data.read(),
        frame_rate=8000,
        channels=1,
        sample_width=2)
    return raw_audio


class USBAudioHandle():
    def __init__(self):
        self.ser = None
        self.stream_out = None
        self.stream_in = None

    def __enter__(self):
        self.ser = serial.Serial("/dev/ttyUSB4", 115200)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        print("cleaning")
        self.ser.close()

        try:
            self.stream_out.stop_stream()
            self.stream_out.close()
            self.stream_in.stop_stream()
            self.stream_in.close()
        except:
            pass


    def send(self, audio, verify_callback):
        self.ser.reset_input_buffer()
        self.ser.reset_output_buffer()

        pcm_data = dub2pcm(audio)
        pcm_data.seek(0)

       # Set parameters for audio data handling
        chunk_size = 1024  # Bytes per chunk (frames_per_buffer * sample_width)
        sample_rate = 8000  # Sample rate in Hz (samples per second)
        frame_duration = chunk_size / (sample_rate * 2)  # 2 bytes per sample (16-bit PCM)

        # Loop through PCM data, read in chunks, and send to serial port
        while True:
            chunk = pcm_data.read(chunk_size)
            if not chunk:
                break  # End of audio data

            # Write PCM data to the serial port
            start = time.time()
            self.ser.write(chunk)
            self.ser.flush()
            verify_callback()
            elapsed = time.time() - start
            remaining = frame_duration - elapsed

            # Sleep to synchronize the data rate (simulate real-time audio processing)
            time.sleep(remaining)

        print("Finished sending audio")



    def receive(self, num_secs):
        self.ser.reset_input_buffer()
        self.ser.reset_output_buffer()

        pcm_data = io.BytesIO()

        now = time.time()   
        while time.time() < now + num_secs:
            if self.ser.in_waiting:
                chunk = self.ser.read(self.ser.in_waiting)
                pcm_data.write(chunk)
                time.sleep(0.1)

        return pcm2dub(pcm_data)
        

    def is_silent(self, dub):
        return detect_leading_silence(dub) / len(dub) > 0.9
