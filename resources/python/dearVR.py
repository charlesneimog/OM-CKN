import soundfile as sf
from pedalboard import load_plugin

plugin = load_plugin("C:/Users/neimog/OneDrive_usp.br/Documents/REAPER Media/Plugins/VST3/dearVR MICRO.vst3")
all_parameters = list(plugin.parameters.keys())
setattr(plugin, all_parameters[0], 300.0) 
audio, sample_rate = sf.read("C:/USERS/NEIMOG/ONEDRIVE_USP.BR/DOCUMENTS/OpenMusic/OUT-FILES/sox-stereo.wav")
final_audio = plugin.process(audio, sample_rate)
sf.write("C:/USERS/NEIMOG/ONEDRIVE_USP.BR/DOCUMENTS/OpenMusic/OUT-FILES/DEAR-90.wav", final_audio, sample_rate)