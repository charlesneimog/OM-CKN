
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

(defun notation->ckn-instruments (x)

(let* (
        (instrument (instrument x))
        (technique (technique x))
        (articulations (articulations x))
        (notehead (notehead x))
        (key (om::x-append instrument technique articulations notehead))
        (instrument_channel 
          (cond 
              ((equal key '("Flute" "ord." "normal")) 8)
              ((equal key '("Flute" "key click" "x")) 7)
              ((and (equal (first key) "Flute") (equal (third key) "arrow down")) 1025)
              ((equal key '("Flute" "multi." "normal")) 1014)
              ((equal key '("Flute" "ord." "staccato" "normal")) 1024)
              ((equal key '("Flute" "ord." "tremolo" "normal")) 1009)


              ((equal key '("Clarinet in Bb" "ord." "x")) 24)
              ((equal key '("Clarinet in Bb" "ord." "normal")) 25)
              ((equal key '("Violin" "ord." "normal")) 111)
              ((equal key '("Violin" "bartók" "normal")) 112)
              ((equal key '("Violin" "pizz." "normal")) 113)
            
              ((equal key '("Viola" "ord." "normal")) 129)
              ((equal key '("Viola" "pizz." "normal")) 131)
              ((equal key '("Violoncello" "ord." "normal")) 146)
              ((equal key '("Violoncello" "pizz." "normal")) 148)
              ((equal key '("Contrabass" "ord." "normal")) 163)
              ((equal key '("Contrabass" "pizz." "normal")) 165)
              ((equal key '("Ac. Guitar" "bartók" "normal")) 177)
              ((equal key '("Ac. Guitar" "ord." "normal")) 175)
              ((equal key '("Ac. Guitar" "ord." "diamond")) 174)
              ((equal (first key) "Sounds") (mapcar (eval 'f-ckn-sounds) (list (second key)))))))


        (print (equal (first key) "Sounds"))
        (print (x-append "Keys: " key "Instrument Result" instrument_channel))
        
instrument_channel))
      

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




        
  