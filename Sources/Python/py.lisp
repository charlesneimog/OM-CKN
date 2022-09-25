(in-package :om)

;; ============== VISUAL THINGS =================

(defun bpf-python-fun (x y thickness color)
      "Build a BPF using Python."
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "bpf.py")))
      (om-py::run-py-script PythonScript (list "x_axis" "y_axis" "thickness" "color") (list X Y thickness color)))
      ; if necessary, you can do some processing of the output here, in this case we just return it
      "Done!"
      )

;; ============
(defun save-bpf-python-fun (x-axis y-axis thickness color outfile_in_python black-backgroud dpi)
      "Build a BPF using Python."
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "save_bpf.py")))
      (print (om-py::run-py-script PythonScript (list "x_axis" "y_axis" "thickness" "color" "outfile" "blackback" "dpi") (list x-axis y-axis thickness color outfile_in_python black-backgroud dpi)))
      ; if necessary, you can do some processing of the output here, in this case we just return it
      ))

;; ============
(defun 3dc-python-fun (X Y Z thickness color)
      "Build a 3DC using Python."
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "3dc.py")))
      (om-py::run-py-script PythonScript (list "xline" "yline" "zline" "thickness" "color") (list X Y Z thickness color)))
      ; if necessary, you can do some processing of the output here, in this case we just return it
      "Done!"
      )

;; ============
(defmethod! show-image-python ((image-path string))
:icon 'py-f
:doc "This will show some image using Python."
(let* (
      (PythonScript (om-py::find-library-PyScripts "OM-CKN" "show_image.py")))
      (om-py::run-py-script PythonScript (list "image_path") (list image-path) :thread t)
      ; if necessary, you can do some processing of the output here, in this case we just return it
      "Done!"
      ))

(defmethod! show-image-python ((image-path pathname))
(show-image-python (namestring image-path)))



