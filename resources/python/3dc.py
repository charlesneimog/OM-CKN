from mpl_toolkits import mplot3d
import numpy as np
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 10000
plt.figure(figsize=(5, 5), dpi=100)
ax = plt.axes(projection='3d')
ax.xaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.yaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.zaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.xaxis._axinfo['grid']['color'] =  (1,0,1,0)
ax.yaxis._axinfo['grid']['color'] =  (1,0,1,0)
ax.zaxis._axinfo['grid']['color'] =  (1,0,0,0)
plt.subplots_adjust(left=0.0, right=1, top=1, bottom=0.0)
ax.plot3D(xline, yline, zline,  lw=thickness, color=color)
plt.show()