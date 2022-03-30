
(in-package :om)


;; ===========================================================================

(om::defclass! ckn-note ()
    (
     (midicents :initform nil :initarg :midicents :accessor midicents)
     (cents :initform nil :initarg :cents :accessor cents)
     (technique :initform nil :initarg :technique :accessor technique)
     (dynamic_value :initform nil :initarg :dynamic_value :accessor dynamic_value)
     (articulations :initform nil :initarg :articulations :accessor articulations)
     (expressions :initform nil :initarg :expressions :accessor expressions)
     (notehead :initform nil :initarg :notehead :accessor notehead)
     (instrument :initform nil :initarg :instrument :accessor instrument)
     (variables :initform nil :initarg :variables :accessor variables)))


;; ===========================================================================

(defun ckn-smulf-to-char (x)
  (cond 
	((equal x "normal")   nil)
        ((equal x "x") 57513)
	((equal x "arrow down")   57588)
	((equal x "arrow up")   57584)
	((equal x "back slashed")   57551)
	((equal x "circle dot")   57573)
	((equal x "circle-x")   57523)
	((equal x "diamond")   57564)
	((equal x "square")   57529)))


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

;; ===========================================================================
(defun symbol2om-velocity (x)

(loop :for symbol :in (om::list! x)
      :collect (cond 
                    ((equal symbol "ppp") 19)
                    ((equal symbol "pp") 39)
                    ((equal symbol "p") 54)
                    ((equal symbol "mp") 59)
                    ((equal symbol "mf") 84)
                    ((equal symbol "f") 99)
                    ((equal symbol "ff") 112)
                    ((equal symbol "fff") 126))))
                    

;; =============================================================================

;;; Edit a valid LAMBDA EXPRESSION
;;; e.g. (lambda (arg1 arg2 ...) ( ... ))
(defun notation->orchidea-instruments (x)
(let* (
       (instrument (instrument x))
       (technique (technique x))
       (articulations (articulations x))
       (notehead (notehead x))
       (key (om::x-append instrument technique articulations notehead)))

(print key)
(cond 
    ((equal key '("Flauta" "ord." "normal")) 8)
    ((equal key '("Flauta" "key click" "x")) 7)
    ((and (equal (first key) "Flauta") (equal (third key) "arrow down")) (om::om-print 1025 "Arrow Down"))


    ((equal key '("Clarinete em Sib" "ord." "x")) 24)
    ((equal key '("Clarinete em Sib" "ord." "normal")) 25)
    ((equal key '("Violino (solo)" "ord." "normal")) 111)
    ((equal key '("Violino (solo)" "bartók" "normal")) 112)
    ((equal key '("Violino (solo)" "pizz." "normal")) 113)
  
    ((equal key '("Viola (solo)" "ord." "normal")) 129)
    ((equal key '("Viola (solo)" "pizz." "normal")) 131)
    ((equal key '("Violoncelo (solo)" "ord." "normal")) 146)
    ((equal key '("Violoncelo (solo)" "pizz." "normal")) 148)
    ((equal key '("Contrabaixo (solo)" "ord." "normal")) 163)
    ((equal key '("Contrabaixo (solo)" "pizz." "normal")) 165)
    ((equal key '("Violão [notação]" "bartók" "normal")) 177)
    ((equal key '("Violão [notação]" "ord." "normal")) 175)
    ((equal key '("Violão [notação]" "ord." "diamond")) 174))))





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
    (file (or path (om-api:om-choose-file-dialog :types '("Musicxml File" "*.musicxml;*.xml"))))      
    (python_code (format nil "
from om_py.musicxml2om import musicxml2om
musicxml2om(r'~d')

" (namestring file)))
    (run-code (om-py::run-py (om::make-value 'om-py::python (list (list :code python_code)))))
    (ritmos (first run-code))
    (alturas (second run-code))
    (velocity (third run-code))
    (voices
        (loop :for alturas-por-voz :in alturas 
        :for ritmos-por-voz :in ritmos
        :for velocity-por-voz :in velocity
        :collect (let* (
                        (format-alturas (remove nil (om::flat alturas-por-voz)))
                        (alturas->midicents (if (null format-alturas)
                                                nil 
                                              (om::n->mc format-alturas 4)))
                        (format-velocity (mapcar (lambda (x) (txt-vel2int-vel x)) (remove nil (om::flat velocity-por-voz))))
                        (format-ritmos (om::mktree (mapcar (lambda (x) (read-from-string x)) (remove nil (om::flat ritmos-por-voz))) '(4 4))))      
                        (make-value 'om::voice (list (list :tree format-ritmos) (list :lmidic alturas->midicents) (list :lvel format-velocity)))))))
    
    (make-value 'om::poly (list (list :obj-list voices)))))




        
  