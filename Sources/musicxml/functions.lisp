
(in-package :om)

;; ===========================================================================
(defun txt-vel2int-vel (txt-vel)

  (cond  
        ((equal txt-vel "pppppp") 5)
        ((equal txt-vel "ppppp") 14)
        ((equal txt-vel "pppp") 23)
        ((equal txt-vel "ppp") 32)
        ((equal txt-vel "pp") 41)
        ((equal txt-vel "p") 50)
        ((equal txt-vel "mp") 59)
        ((equal txt-vel "mf") 68)
        ((equal txt-vel "f") 79)
        ((equal txt-vel "ff") 86)
        ((equal txt-vel "fff") 95)
        ((equal txt-vel "ffff") 104)
        ((equal txt-vel "fffff") 113)
        ((equal txt-vel "ffffff") 122)))

;; =============================================================================

(defmethod! musicxml2om (&optional path)
:initvals '(NIL)
:indoc ' ("Translate a MusicXML file to an OM object.")
:icon :import-xml
:doc 
"
Read the data from musicxml2om function in Python.
"

(let* (
    (file (or path (om-choose-file-dialog)))      
    (python_code (format nil "
from om_py import musicxml2om

musicxml2om(r'~d')

" (namestring file)))
    (run-code (om-py::run-py (om::make-value 'om-py::to-om (list (list :py-inside-om python_code))) nil))
    (ritmos (first run-code))
    (alturas (second run-code))
    (velocity (third run-code))

    (voices
        (loop :for alturas-por-voz :in alturas 
        :for ritmos-por-voz :in ritmos
        :for velocity-por-voz :in velocity
        :collect (let* (
                        (format-alturas (om::n->mc (remove nil (om::flat alturas-por-voz)) 4))
                        (format-velocity (mapcar (lambda (x) (txt-vel2int-vel x)) (remove nil (om::flat velocity-por-voz))))
                        (format-ritmos (om::mktree (mapcar (lambda (x) (read-from-string x)) (remove nil (om::flat ritmos-por-voz))) '(4 4))))      
                        (make-value 'om::voice (list (list :tree format-ritmos) (list :lmidic format-alturas) (list :lvel format-velocity)))))))
    
    (make-value 'om::poly (list (list :obj-list voices)))))




        
   