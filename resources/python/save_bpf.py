import matplotlib.pyplot as plt
from om_py import to_om
plt.rcParams['agg.path.chunksize'] = 1000

if blackback:
    blackback.style.use('dark_background')
    
plt.figure(figsize=(20, 10), dpi=dpi)
plt.plot(x_axis, y_axis, lw=thickness, color=color)
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.rcParams['agg.path.chunksize'] = 10000
plt.savefig(outfile)
plt.show()
to_om(outfile)



