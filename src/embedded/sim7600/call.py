import serial
import time
from audio import USBAudioHandle
from ai import chat_stream, text_to_speech, speech_to_text


class ATError(Exception):
    pass


class ATHandle():
    def __init__(self):
        self.ser = None

    def __enter__(self):
        self.ser = serial.Serial("/dev/ttyUSB2", 115200)
        return self
    
    def __exit__(self, exc_type, exc_value, traceback):
        print("cleaning up")

        try:
            at.send("AT+CHUP", expected_out="OK")
            at.send(f"AT+CPCMREG=0", expected_out="OK")
        except:
            pass

        self.ser.close()

    def send(self, command, expected_out, wait=1, num_max_tries=5):
        rec_buff = b""
        num_tries = 0
        while (num_tries < num_max_tries) and (expected_out not in rec_buff.decode()):
            print(f"[Command]\n{command}")
            self.ser.write((command + "\r\n").encode())
            num_tries += 1
            time.sleep(wait)
            rec_buff = self.ser.read(self.ser.inWaiting())
            print(f"[Output]\n{rec_buff.decode().strip()}")

        if expected_out not in rec_buff.decode():
            print(command + " ERROR")
            print(command + " back:\t" + rec_buff.decode())
            raise ATError(f"{command} failed")

        print(f"[success]\n")

    def wait_for_begin(self, wait=1, num_max_tries=20):
        expected_out = "VOICE CALL: BEGIN"
        rec_buff = self.ser.read(self.ser.inWaiting())
        num_tries = 0
        while (num_tries < num_max_tries) and (expected_out not in rec_buff.decode()):
            num_tries += 1
            time.sleep(wait)
            rec_buff = self.ser.read(self.ser.inWaiting())
            print(f"[Output]\n{rec_buff.decode().strip()}")

    def assert_call_on(self):
        rec_buff = self.ser.read(self.ser.inWaiting())
        if "VOICE CALL: END" in rec_buff.decode():
            raise ATError("Call ended")


phone_number = "7019295600"
now = time.time()

def log(msg):
    print(f'[{round(time.time() - now, 2)}] {msg}')

with ATHandle() as at:
    with USBAudioHandle() as audio:
        at.send(f"ATD{phone_number};", expected_out="OK", wait=0.1)
        at.wait_for_begin()
        at.send(f"AT+CPCMREG=1", expected_out="OK", wait=2)
        
        log("ready")
        # say hi
        system_msg = "Hi, how are you! Welcome to Meta Hackathon. You are speaking to an AI bot. How can I help you?"
        hi_dub = text_to_speech(system_msg)
        stream = audio.send(hi_dub, verify_callback=at.assert_call_on)
        log(f"sent {len(hi_dub)/1000} audio")

        messages = [
            {"role": "system", 
            "content": "You are a helpful assistant speaking to a user on a phone call. Keep your response crisp and short and say 'over' after your response."},
            {"role": "assistant", "content": system_msg}
        ]

        for num_iter in range(10):
            #######
            # recieve audio
            num_sec_listen = 2
            rec = audio.receive(num_sec_listen)
            log(f"recieved audio with {rec.dBFS}")
            while True:
                at.assert_call_on()
                rec_chunk = audio.receive(num_sec_listen)
                log(f"recieved audio chunk again with {rec_chunk.dBFS}")
                if audio.is_silent(rec_chunk):
                    log("this is silent chunk") 
                    break
                else:
                    rec = rec + rec_chunk
            log(f"recieved total {len(rec)/1000}s audio with {rec.dBFS}")
            prompt = speech_to_text(rec)

            messages.append({"role": "user", "content": prompt})

            #######
            # send audio
            txt_output = ""
            log(f"messages: {messages}")
            for to_send, txt_out in chat_stream(messages):
                stream = audio.send(to_send, verify_callback=at.assert_call_on)
                txt_output = txt_output + txt_out
                log(f"sent {len(to_send)/1000} audio")

            messages.append({"role": "assistant", "content": txt_output})

        at.send("AT+CHUP", expected_out="OK")
        at.send(f"AT+CPCMREG=0", expected_out="OK")
