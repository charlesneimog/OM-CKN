import dawdreamer as daw
import numpy as np
from scipy.io import wavfile
import librosa

SAMPLE_RATE = 44100
BUFFER_SIZE = 128
DURATION = 15
MEU_AUDIO = "C:/USERS/NEIMOG/ONEDRIVE_USP.BR/DOCUMENTS/OpenMusic/OUT-FILES/om-ckn/Fl-mul-G#5-G4+-D6-100-cents.wav-v-stereo.wav"

def load_audio_file(file_path, duration=None):
  sig, rate = librosa.load(file_path, duration=duration, mono=False, sr=SAMPLE_RATE)
  assert(rate == SAMPLE_RATE)
  return sig

engine = daw.RenderEngine(SAMPLE_RATE, BUFFER_SIZE)
meu_plugin = engine.make_plugin_processor("plugin", "C:/Users/neimog/OneDrive_usp.br/Documents/Plugins/VST3/dearVR MICRO.vst3")

meu_plugin.set_parameter(0, 45) # override a specific parameter.


tempo = librosa.get_duration(filename=MEU_AUDIO)
multi =  load_audio_file(MEU_AUDIO , duration=tempo)


graph = [
  (meu_plugin, [])]

engine.load_graph(graph)
engine.render(DURATION)
audio = engine.get_audio()  

wavfile.write('my_song.wav', SAMPLE_RATE, audio.transpose()) # don't forget to transpose!
