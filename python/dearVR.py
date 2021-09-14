import soundfile as sf
from pedalboard import load_plugin


dearVR = load_plugin("python/dearVR MICRO.vst3")
print(dearVR.parameters.keys())
dearVR.hrtf = 'dearVR'
setattr(dearVR, 'azimuth_ￂﾰ', '-90.0ￂﾰ') ### wait for the developer fix one bug

audio, sample_rate = sf.read('C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/untitled.wav')

final_audio = dearVR.process(audio, sample_rate)

sf.write('C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/OM - Workspace/out-files/dearVR-1.wav', final_audio, sample_rate)
