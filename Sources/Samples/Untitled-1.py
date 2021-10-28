
import numpy as np
import matplotlib.pyplot as plt
from celluloid import Camera

def to_complex(mag, theta):
    return mag * np.exp(theta*1j)

plt.figure(figsize = (15, 5))
res = 1000
wave1 =  np.sin(np.linspace(np.pi/2, np.pi*440 + np.pi/2, res)) # 10 hz
wave2 =  np.sin(np.linspace(np.pi/2, np.pi*550 + np.pi/2, res)) # 20 hz
wave3 =  np.sin(np.linspace(np.pi/2, np.pi*660 + np.pi/2, res)) # 30 hz
plt.plot(np.linspace(0, 1, res), wave1, label = "10 Hz")
plt.plot(np.linspace(0, 1, res), wave2, label = "20 Hz" )
plt.plot(np.linspace(0, 1, res), (wave1+wave2), label = "Wave 1 + Wave 2" )
plt.xlim(0, 1)
plt.legend()

fig = plt.figure(figsize = (15, 5))
plots = []
f = 0.5

waves = [wave1, wave2, wave3]

for i in range(3):
    plots.append(fig.add_subplot(130+i+1,projection='polar')) # creating axis in subplot
    
    plots[i].plot(2*np.pi*np.linspace(0, f, res), waves[i], c = "C" + str(i)) #
    plots[i].set_yticklabels([]) #getting rid of tick labels 
    plots[i].set_xticklabels([])

fig = plt.figure(figsize = (15, 7))
camera = Camera(fig)
plots = []
for i in range(3):
    plots.append(fig.add_subplot(130+i+1,projection='polar'))
    
frequencies, step = np.linspace(9, 10, 200, retstep = True) #the frequency I start at to the frequency I finish at

print("STEP: ", step)
for f in frequencies:
    for i in range(3):

        plots[i].plot(2*np.pi*np.linspace(0, f, res), waves[i], c = "C" + str(i))
        plots[i].set_yticklabels([])
        plots[i].set_xticklabels([])
        
        a = np.mean(to_complex(abs(waves[i]), 2*np.pi*np.linspace(0, f, res))) #compute centroid
        plots[i].scatter(np.angle(a), abs(a), c= "grey", s = 70) #plot centroid
            
    camera.snap()
    plt.show()