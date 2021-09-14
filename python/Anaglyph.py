import soundfile as sf
from pedalboard import load_plugin


Anaglyph = load_plugin("C:/Users/neimog/OneDrive - design.ufjf.br/Documentos/REAPER Media/Plugins/anaglyph-win-v0.9.4/Anaglyph.vst3")
print(Anaglyph.parameters.keys())

setattr(Anaglyph, 'azimuth_ￂﾰ', '90º') ### wait for the developer fix one bug