
;; ================================================
;;;; Working with Vamp Plugins        =============
;; ================================================

(defmethod! vamp-list-plugins nil
:icon 'py-f
:doc "
This object will output a list of all the available Vamp plugins. You need to install 'vamphost' using py-ext-add-module from om-py.
"

(let* (
      (python-code (format nil
                    "
from om_py import to_om
import vamp
to_om(vamp.list_plugins())"
))
      (run (om-py::run-py (make-value (quote om-py::python) (list (list :code python-code))))))
     (flat run)))

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
      (run (om-py::run-py (make-value (quote om-py::python) (list (list :code python-code))))))
      run))
      
;; ================================================
(defmethod! vamp-process ((sound pathname) (vamp_key string))
:icon 'py-f
:doc "
This object will process some audio using Vamp plugins.

OBS.: You need to install 'vamphost' using py-ext-add-module from om-py.

"
(vamp-process (namestring sound) vamp_key))


;; ================================================
(defmethod! vamp-process ((sound sound) (vamp_key string))
:icon 'py-f
:doc "
This object will process some audio using Vamp plugins.

OBS.: You need to install 'vamphost' using py-ext-add-module from om-py.
"
(let* (
(sound-path (if (null (file-pathname sound))
                      (car (save-temp-sounds sound))
                      (file-pathname sound))))
(vamp-process (namestring sound-path) vamp_key)))


;; ================================================

(defmethod! vamp-filter-by-prefix ((vamp_plugins list) (vamp_prefix string))
:icon 'py-f
:doc "
Filters plugins using the prefix.

"

(let* (
      (filter-function (lambda (x) (if (equal vamp_prefix (first (string-to-list x ":"))) x nil))))
      (remove nil (mapcar filter-function vamp_plugins))))