import vamp
import librosa
from om_ckn import to_om

# from om_ckn import to_om

data, rate = librosa.load(r'C:\Users\neimog\OneDrive_usp.br\Documents\OpenMusic\in-files\Fl-ord-A4-mf.aif')
output = vamp.collect(data, rate, "bbc-vamp-plugins:bbc-speechmusic-segmenter")
print(output)




