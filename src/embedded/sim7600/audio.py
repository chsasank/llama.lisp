import serial
import time
from pydub import AudioSegment
from pydub.silence import detect_leading_silence
import io
import pyaudio
import time

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
        self.pyaudio = pyaudio.PyAudio()
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

        self.pyaudio.terminate()

    def send(self, audio):
        self.ser.reset_input_buffer()
        self.ser.reset_output_buffer()

        pcm_data = dub2pcm(audio)
        pcm_data.seek(0)

        # Callback function for PyAudio
        def callback(in_data, frame_count, time_info, status):
            chunk = pcm_data.read(frame_count * 2)  # 2 bytes per frame (16-bit)
            if not chunk:
                return (None, pyaudio.paComplete)

            self.ser.write(chunk)  # Write PCM data to serial port
            self.ser.flush()
            return (chunk, pyaudio.paContinue)

        # Open an output stream to play the audio (through the serial interface)
        self.stream_out = self.pyaudio.open(
            format=pyaudio.paInt16,  # 16-bit PCM
            channels=1,  # Mono
            rate=8000,  # Sample rate (8000 Hz)
            output=True,  # Output stream
            frames_per_buffer=1024,  # Buffer size
            stream_callback=callback)  # Use callback

        # Start the stream
        self.stream_out.start_stream()
        return self.stream_out

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
