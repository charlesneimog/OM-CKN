import sys
import matplotlib.pyplot as plt
import simpl


audio = simpl.read_wav(r'C:\Users\neimog\OneDrive_usp.br\Documents\OpenMusic\out-files\4.wav')

pd = simpl.LorisPeakDetection()
pd.max_peaks = 30
frames = pd.find_peaks(audio)
pt = simpl.MQPartialTracking()
pt.max_partials = 30
frames = pt.find_partials(frames)
simpl.plot_partials(frames, show_peaks=False)
plt.show()