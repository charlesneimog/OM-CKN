import matplotlib.pyplot as plt

plt.rcParams['agg.path.chunksize'] = 10000
plt.figure(figsize=(20, 10), dpi=100)
plt.plot(x_axis, y_axis, lw=thickness, color=color)
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.show()