import dawdreamer as daw
from scipy.io import wavfile

SAMPLE_RATE = 44100
BUFFER_SIZE = 128 
PLUGIN = 'C:/Users/neimog/OneDrive_usp.br/Documents/Plugins/VST3/dearVR pro.vst3' 
MY_SOUND = "C:/USERS/NEIMOG/ONEDRIVE_USP.BR/DOCUMENTS/OpenMusic/OUT-FILES/om-ckn/Ob+S-ord-C4-mf-N-N.wav-v-stereo.wav"


engine = daw.RenderEngine(SAMPLE_RATE, BUFFER_SIZE)
dearVR = engine.make_plugin_processor('Espacializacao', PLUGIN)
dearVR.set_parameter(0, 75)
dearVR.set_parameter(1, 20)

graph = [
  (dearVR, [])  
]

engine.load_graph(graph)  
engine.render(10)
audio = engine.get_audio()  
wavfile.write('C:/USERS/NEIMOG/ONEDRIVE_USP.BR/DOCUMENTS/OpenMusic/OUT-FILES/NEIMOG.wav', SAMPLE_RATE, audio.transpose()) 

