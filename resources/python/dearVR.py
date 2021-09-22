import soundfile as sf
from pedalboard import load_plugin

plugin = load_plugin("C:\\USERS\\NEIMOG\\ONEDRIVE - DESIGN.UFJF.BR\\DOCUMENTOS\\REAPER MEDIA\\PLUGINS\\ATMOS 2 VST WIN x64\\Atmos 2.vst3")

midi_messages = ((0.0, (60, 127)), (1.0, (62, 127)), (3.0, (64, 127)))
output = plugin.process(sample_rate=44100, midi_messages=midi_messages)
sf.write('C:/Users/neimog/OneDrive/Documentos/OM - Workspace/out-files/MIDI-TEST.wav', output, sample_rate)