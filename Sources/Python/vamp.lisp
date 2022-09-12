(in-package :om)

;; ================================================
;;;; Working with Vamp Plugins =============
;; ================================================

(defmethod! vamp-list-plugins nil
:icon 'vamp
:doc "
This object will output a list of all the available Vamp plugins. You need to install 'vamphost' using py-ext-add-module from om-py.
"
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "vamp-list-plugins.py"))
      (Python-Output (om-py::run-py-script PythonScript nil nil)))
      ; if necessary, you can do some processing of the output here, in this case we just return it
      Python-Output
      ))

;; ================================================

(defmethod! vamp-get-values ((results list))
:icon 'vamp
:doc "
Most plugins result in some list like ('vector' (Number (DATA))). This function filters and only uses DATA.
"
(second (second (car results))))

;; ================================================

(defmethod! vamp-parameters ((vamp-key string))
:icon 'vamp
:doc "
This object will output a list of parameters of some Vamp plugin.
"
; print two newlines to make the output more readable 

(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "vamp-get-parameters.py"))
      (Python-Output (om-py::run-py-script PythonScript (list "vamp_key") (list vamp-key))))
      ; if necessary, you can do some processing of the output here, in this case we just return it
      (print (format nil "~%~%"))
      (mapcar (lambda (x) (om::om-print x vamp-key)) Python-Output)
      "Check the console for the output"
      ))


;; ================================================

(defmethod! vamp-process ((sound string) (vamp_key string))
:icon 'vamp
:doc "
This object will output a list of all the available Vamp plugins. You need to install 'vamphost' using py-ext-add-module from om-py.
"
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "vamp-process.py"))
      (Python-Output (om-py::run-py-script PythonScript '("my_sound" "vamp_key") (list sound vamp_key))))
      Python-Output
      ))

;; ================================================
(defmethod! vamp-process ((sound pathname) (vamp_key string))
:icon 'vamp
:doc "
This object will process some audio using Vamp plugins.

OBS.: You need to install 'vamphost' (just work with Python 3.9) using py-ext-add-module from om-py.

"
(vamp-process (namestring sound) vamp_key))


;; ================================================
(defmethod! vamp-process ((sound sound) (vamp_key string))
:icon 'vamp
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
:icon 'vamp
:doc "
Filters plugins using the prefix.

"

(let* (
      (filter-function (lambda (x) (if (equal vamp_prefix (first (string-to-list x ":"))) x nil))))
      (remove nil (mapcar filter-function vamp_plugins))))