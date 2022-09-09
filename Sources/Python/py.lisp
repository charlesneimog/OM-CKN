(in-package :om)

;; ============== VISUAL THINGS =================

(defun bpf-python-fun (X Y Z color)
      "Build a BPF using Python."
(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 10000
X = ~d
Y = ~d
plt.figure(figsize=(20, 10), dpi=100)
plt.plot(X, Y, lw=~d, color='~d')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.show()
" x y z color))
      (save-python-code (om::save-as-text python-code (om::tmpfile "bpf.py" :subdirs "om-py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code))))
      (where-i-am-running 
                     #+macosx (om::string+ om-py::*activate-virtual-enviroment* " && python3 ")
                     #+windows (om::string+ om-py::*activate-virtual-enviroment* " && python ")
                     #+linux (om::string+ om-py::*activate-virtual-enviroment* " && python3.8 ")))
      (oa::om-command-line (om::string+ where-i-am-running prepare-cmd-code) t)))


;; ============
(defun save-bpf-python-fun (X Y thickness color outfile blackback dpi)
      "Save a BPF using Python."

(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 1000
~d.style.use('dark_background')
X = ~d
Y = ~d
plt.figure(figsize=(20, 10), dpi=~d)
plt.plot(X, Y, lw=~d, color='~d')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.rcParams['agg.path.chunksize'] = 10000
plt.savefig(~d)
plt.show()
print('Documento Salvo em ~d')
" blackback x y dpi thickness  color outfile outfile))
      (save-python-code (om::save-as-text python-code (om::outfile "save-bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ============
(defun bpf-python-om (X Y Z)
(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
X = ~d
Y = ~d
plt.figure(figsize=(14, 7))
plt.plot(X, Y, lw=~d, color='black')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.show()
" x y z))
      (run (om-py::run-py (make-value (quote om-py::om2py) (list (list :py-om python-code))))))
      (flat run 1)))

;; ============
(defun 3dc-python-fun (X Y Z A color)
(let* (
      (python-code (format nil
                    "
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
zline = ~d
xline = ~d
yline = ~d
plt.subplots_adjust(left=0.0, right=1, top=1, bottom=0.0)
ax.plot3D(xline, yline, zline,  lw=~d, color='~d')
plt.show()
" x y z a color))
      (run (om-py::run-py (make-value (quote om-py::om2py) (list (list :py-om python-code))))))
      (flat run 1)))


