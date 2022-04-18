
(in-package :om)

;; GENERAL FUNCTIONS 

;; ################################################################################
(defun music21.measure-beat-unit (object)
(/ (fdenominator (car (tree object))) (find-beat-symbol (fdenominator (car (tree object))))))


;; ################################################################################
(defun music21.create-measure-class (tree)

(format nil
"
###########################
## Create New Measure #####
###########################

measure = stream.Measure()
measure.append(meter.TimeSignature('~d/~d'))" (car (car tree)) (second (car tree))))

;; ################################################################################
(defun music21.char2headnote-name (headnote)

(if (null headnote)
    "normal"
    (case (head-char headnote)
          (57566 "diamond")
          (57513 "x")
          (57511 "x")
          (57540 "triangle")
          (t "normal"))))

;; ################################################################################
;; ################################################################################
;; ################################################################################

(defun music21.get-measure-class (measure_inside ties)
 " Apply correct function to make python code."

(set 'measure_ties_values ties)

(let* (

        
        (lambda_function (lambda (class)

                                (case (type-of class)
                                      ('r-rest (music21.mkrest class))
                                      ('group (music21.mkgroup class))
                                      ('chord (music21.mkchord class))
                                      ('continuation-chord (music21.mkchord class)))))

        (apply_mapcar (mapcar lambda_function measure_inside)))
        
apply_mapcar))


;; ################################################################################

(defun music21.mkrest (rest)

  (let* (
          (rest_duration (float (* (symbolic-dur rest) 4)))
          (make_python_code (format nil "

## REST ##
r = note.Rest()
r.duration.quarterLength = ~d
measure.append(r) " rest_duration)))
(set 'measure_ties_values (cdr (eval 'measure_ties_values)))
 make_python_code))

;; ################################################################################

(defun music21.mkchord (chord)

  (let* (
          (chord_duration (float (* (om::symbolic-dur chord) 4)))
          (chords_notehead (let* (  
                                  (chord_extras (extras chord))
                                  (just_notes_heads (loop :for extras in chord_extras
                                                        :collect (if (equal 'head-extra (type-of extras))
                                                                      extras))))
                                  (music21.char2headnote-name (car (remove nil just_notes_heads))))) ;; It is not possible use more 
                                                                                                     ;; than one head-extra in a chords.                                     
          (python-ties  
                        (let*   (
                                (tie (car (eval 'measure_ties_values))))
                                (set 'measure_ties_values (cdr (eval 'measure_ties_values)))
                                (if      
                                        (equal tie "None")
                                        "#This chord has no ties"
                                        (format nil "c.tie = tie.Tie(~d) " tie))))
                
          (notes (if 
                        (equal 'chord (type-of chord))
                        (om::set 'om-ckn-lmidic-value (mc->n (om::lmidic chord) 4))
                        (eval 'om-ckn-lmidic-value)))
          
          (py-note-list (let* (
                        (conteudo (loop :for note :in notes :collect (om-py::concatString (om::x-append "'" note "'" '(", "))))))
                        (om-py::concatString (om::x-append "[" conteudo "]"))))
        
          
          (make_python_code (format nil "

### CHORD ###

c = chord.Chord(~d)
c.notehead = '~d'
~d 
c.duration.quarterLength = ~d
measure.append(c)
" py-note-list chords_notehead python-ties  chord_duration)))
 
 make_python_code))

;; ################################################################################

(defun music21.mkgroup (group)

(let*   (
        (make_python_code 
                (loop   :for element :in (inside group)
                        :collect (case (type-of element)
                                ('r-rest (music21.mkrest-in-group element))
                                ('group (music21.mkgroup element)) ;; Should be music21.mkgroup-in-group 
                                ('chord (music21.mkchord-in-group element))
                                ('continuation-chord (music21.mkchord-in-group element))))))
        (om-py::concatString make_python_code)))


;; ################################################################################

(defun music21.mkchord-in-group (chord)

  (let* (
          (chord_duration (* (om::symbolic-dur chord) 4))
          (chords_notehead (let* (  
                                  (chord_extras (extras chord))
                                  (just_notes_heads (loop :for extras in chord_extras
                                                        :collect (if (equal 'head-extra (type-of extras))
                                                                      extras))))
                                  (music21.char2headnote-name (car (remove nil just_notes_heads))))) ;; It is not possible use more 
                                                                                                     ;; than one head-extra in a chords.                                     
          (python-ties  
                        (let*   (
                                (tie (car (eval 'measure_ties_values))))
                                (set 'measure_ties_values (cdr (eval 'measure_ties_values)))
                                (if      
                                        (equal tie "None")
                                        "#This chord has no ties"
                                        (format nil "c.tie = tie.Tie(~d) " tie))))
          
          (notes (if 
                        (equal 'chord (type-of chord))
                        (om::set 'om-ckn-lmidic-value (mc->n (om::lmidic chord) 4))
                        (eval 'om-ckn-lmidic-value)))
          
          (py-note-list (let* (
                        (conteudo (loop :for note :in notes :collect (om-py::concatString (om::x-append "'" note "'" '(", "))))))
                        (om-py::concatString (om::x-append "[" conteudo "]"))))
        
          
          (make_python_code (format nil "

### CHORD ###

c = chord.Chord(~d)
c.notehead = '~d'
~d
c.duration = duration.Duration(~d)
measure.append(c)
" py-note-list chords_notehead python-ties chord_duration)))

 make_python_code))

 ;; ################################################################################

(defun music21.mkrest-in-group (rest)

  (let* (
          (rest_duration (* (symbolic-dur rest) 4))
          (make_python_code (format nil "

## REST ##
r = note.Rest()
r.duration = duration.Duration(~d)
measure.append(r) " rest_duration)))
(set 'measure_ties_values (cdr (eval 'measure_ties_values)))
make_python_code))