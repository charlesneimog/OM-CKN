(in-package :om)

(require-library "om-py")

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
      (save-python-code (om::save-as-text python-code (om::outfile "bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

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
      (save-python-code (ckn-save-as-text python-code (om::outfile "bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

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
      (save-python-code (om::save-as-text python-code (om::outfile "3dc.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ================================================
;;;; Working with Vamp Plugins        =============
;; ================================================

(defmethod! vamp-list-plugins nil
:icon 'py-f
:doc "
This object will output a list of all the available Vamp plugins.
"

(let* (
      (python-code (format nil
                    "
from om_py import to_om
import vamp
to_om(vamp.list_plugins())"
))
      (run (om-py::run-py (make-value (quote py) (list (list :py-om python-code))))))
      (flat run 1)))

;; ================================================

(defmethod! vamp-process ((sound string) (vamp_key string))
(let* (
      (python-code (format nil
                    "
import vamp
import librosa
from om_py import to_om
data, rate = librosa.load(r'~d')
output = vamp.collect(data, rate, '~d')
to_om(output)
" sound vamp_key))
      (run (om::run-py (make-value (quote py) (list (list :py-om python-code))))))
      run))
      
;; ================================================
(defmethod! vamp-process ((sound pathname) (vamp_key string))
:icon 'py-f
:doc "
This object will process some audio using Vamp plugins.
"
(vamp-process (namestring sound) vamp_key))


;; ================================================
(defmethod! vamp-process ((sound sound) (vamp_key string))
:icon 'py-f
:doc "
This object will process some audio using Vamp plugins.
"
(let* (
(sound-path (if (null (file-pathname sound))
                      (save-temp-sounds sound)
                      (file-pathname sound))))
(vamp-process (namestring sound-path) vamp_key)))


;; ================================================

(defmethod! vamp-filter-by-prefix ((vamp_plugins list) (vamp_prefix string))
:icon 'py-f
:doc "
Filters plugins using the prefix."

(let* (
      (filter-function (lambda (x) (if (equal vamp_prefix (first (string-to-list x ":"))) x nil))))
      (remove nil (mapcar filter-function vamp_plugins))))
