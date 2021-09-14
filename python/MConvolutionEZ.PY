import soundfile as sf
from pedalboard import load_plugin


MConvolutionEZ = load_plugin("C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/Plugins/MConvolutionEZ.dll")
print(MConvolutionEZ.parameters.keys())


#audio, sample_rate = sf.read('C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/untitled.wav')

#final_audio = MConvolutionEZ.process(audio, sample_rate)

#sf.write('C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/OM - Workspace/out-files/dearVR-1.wav', final_audio, sample_rate)
