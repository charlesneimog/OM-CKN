import vamp
import librosa
from om_py import to_om

data, rate = librosa.load(my_sound)

output = vamp.collect(data, rate, vamp_key)

to_om(output)