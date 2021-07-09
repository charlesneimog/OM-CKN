(in-package :om)

;; Caminho para Mr Swatson  ========================


(defvar *MrsWatson-PATH* "path to MrsWatson")

(pushr 'om-ckn *external-prefs*)

(defmethod get-external-name ((module (eql 'om-ckn))) "path to MrsWatson")

(defmethod get-external-module-path ((module (eql 'om-ckn)) modulepref) (get-pref modulepref :om-ckn))

(defmethod set-external-module-path ((module (eql 'om-ckn)) modulepref path) 
  (set-pref modulepref :om-ckn path))

(defmethod save-external-prefs ((module (eql 'om-ckn))) 
  `(:om-ckn ,(om-save-pathname *MrsWatson-PATH*)))


(defmethod put-external-preferences ((module (eql 'om-ckn)) moduleprefs)
    (when (get-pref moduleprefs :om-ckn)
      (setf *MrsWatson-PATH* (find-true-external (get-pref moduleprefs :om-ckn))))
    t)

;; Caminho para Sox  ========================

(defvar *SOX-PATH* "path to OM-SOX")

(pushr 'om-ckn *external-prefs*)

(defmethod get-external-name ((module (eql 'om-ckn))) "OM-CKN Sox path")

(defmethod get-external-module-path ((module (eql 'om-ckn)) modulepref) (get-pref modulepref :om-ckn))

(defmethod set-external-module-path ((module (eql 'om-ckn)) modulepref path) 
  (set-pref modulepref :om-ckn path))

(defmethod save-external-prefs ((module (eql 'om-ckn))) 
  `(:om-ckn ,(om-save-pathname *SOX-PATH*)))


(defmethod put-external-preferences ((module (eql 'om-ckn)) moduleprefs)
    (when (get-pref moduleprefs :om-ckn)
      (setf *SOX-PATH* (find-true-external (get-pref moduleprefs :om-ckn))))
    t)


;; =================================================================================================
(defun string-to-list (string &optional (separator " "))
  (when string
    (multiple-value-bind (token rest)
        (string-until-char string separator)
      (cons token (string-to-list rest separator)))))
;; =================================================================================================

;;(defun sox-pitchshift (sample cents-tranposition)

(defun name-of-the-sound (p)
  (let ((path (and p (pathname p))))
  (when (pathnamep path)
    (string+ (pathname-name path) 
             (if (and (pathname-type path) (stringp (pathname-type path)))
                 (string+ "." (pathname-type path)) 
               "")))))

;  ========================

(defun ckn-string-name (list-name)

(let*  (
    (action1 (string+ (first list-name) (second list-name)))
    (action2 (if 
                (>  (length (x-append action1 list-name)) 2)
                (x-append action1 (last-n list-name (- (length list-name) 2)))
                action1)))
    
    (if (< (length action2) 2)
            (first action2)
            (setf list-name (ckn-string-name action2)))))

;  ========================

(defun ckn-transpose-a-sound (instrumentos desvio) 

(let* (
(action1 (string-to-list (name-of-the-sound instrumentos) "-"))
(action2 (1- (length action1)))
(action3 (loop :for x :in action1 :collect (string+ x "-")))
(action4 (ckn-string-name (first-n action3 action2)))
(action5 (string+ action4 (format nil "~d-cents" desvio) ".aif"))
(action6 (outfile action5))
(action7 (namestring action6))
(action8 (om-cmd-line 
          (string+ (list->string-fun (list (namestring *SOX-PATH*)))
          " "
          (list->string-fun (list instrumentos))
          " "
          (list->string-fun (list action7))
          (format nil " pitch ~d" desvio)))))

action6))

;; Fazer um código mais bonito

;; ===============================================================================

(defun ckn-transpose  (nome cents)

(let* (
(action1 (namestring nome)))
 (progn
  (loop with ckn-loop 
        := nil 
        :while (setf ckn-loop (equal (probe-file (ckn-transpose-a-sound action1 cents)) nil))
        :do (let* () ckn-loop))
  (probe-file (ckn-transpose-a-sound action1 cents)))))

;; ===============================================================================

(defun true-durations (ckn)

 (let* ((newchrdseq (if (typep ckn 'note) 
                           (om::Objfromobjs (om::Objfromobjs ckn (make-instance 'chord)) (make-instance 'chord-seq))
                           (om::Objfromobjs ckn (make-instance 'chord-seq))))

         (newcs (normalize-chord-seq newchrdseq))
         (onsets (om::Lonset newcs))
         (dur (om::Ldur newcs))
         (newonsets (if (= 2 (length onsets)) (om::x->dx  onsets) (butlast (om::x->dx onsets))))
         (newdurs (mapcar 'first dur))
         (resultat1 
          (om::x-append 
          (flat
           (list (mapcar #'(lambda (x y) (if (= 0 (- x y)) x 
                                             (list x (- x y))))
                         newdurs newonsets)
                 (last newdurs)))
          (last-elem newdurs)))
         (resultat2 (butlast
                     (if (= 0 (first onsets)) resultat1 (cons (om::om* -1 (first onsets)) resultat1)))))
    
   (let ((result (remove nil (mapcar #'(lambda (x) (if (not (or (= x 1) (= x -1))) x ))
          resultat2))))
         (if (= 2 (length onsets)) (list (car result) (second result)) result))
   ))

;====================================================================================

(defun normalize-chord-seq (chrdseq)
  (let* ((xdx (om::x->dx (om::lonset chrdseq)))
         (filt-durs1 (mapcar 'list-min (om::ldur chrdseq)))
         (lst-durs (mapcar 'list xdx filt-durs1))
         (filt-durs2 (mapcar 'list-min lst-durs))
         (newdurs (loop 
                   :for pt :in (om::lmidic chrdseq)
                   :for drs :in filt-durs2
                   collect (repeat-n drs (length pt)))))
    (make-instance 'chord-seq 
                   :lmidic (om::lmidic chrdseq)
                   :lonset (om::lonset chrdseq)
                   :ldur newdurs)))

;====================================================================================

(defun build-sound-sequence-fun (sounds fade-bewteen-sound)

(let*  (
    (action1 (sound-seq (first sounds) (second sounds) fade-bewteen-sound))
    (action2 (if 
                (>  (length (x-append action1 sounds)) 2)
                (x-append action1 (last-n sounds (- (length sounds) 2)))
                action1)))
    
    (if (< (length action2) 2)
            (first action2)
            (setf sounds (build-sound-sequence-fun action2 fade-bewteen-sound)))))

;====================================================================================

(defun build-sound-mix-fun (sounds)

(let*  (
    (action1 (sound-mix (first sounds) (second sounds)))
    (action2 (if 
                (>  (length (x-append action1 sounds)) 2)
                (x-append action1 (last-n sounds (- (length sounds) 2)))
                action1)))
    
    (if (< (length action2) 2)
            (first action2)
            (setf sounds (build-sound-mix-fun action2)))))

;====================================================================================

(defun ckn-mc->n (note)
(case note
(3600 "C2") (3700 "C#2") (3800 "D2") (3900 "D#2") (4000 "E2") (4100 "F2") (4200 "F#2") (4300 "G2") (4400 "G#2") (4500 "A2") (4600 "A#2") (4700 "B2")
(4800 "C3") (4900 "C#3") (5000 "D3") (5100 "D#3") (5200 "E3") (5300 "F3") (5400 "F#3") (5500 "G3") (5600 "G#3") (5700 "A3") (5800 "A#3") (5900 "B3")
(6000 "C4") (6100 "C#4") (6200 "D4") (6300 "D#4") (6400 "E4") (6500 "F4") (6600 "F#4") (6700 "G4") (6800 "G#4") (6900 "A4") (7000 "A#4") (7100 "B4")
(7200 "C5") (7300 "C#5") (7400 "D5") (7500 "D#5") (7600 "E5") (7700 "F5") (7800 "F#5") (7900 "G5") (8000 "G#5") (8100 "A5") (8200 "A#5") (8300 "B5")
     
(nil nil)))
       
;====================================================================================


(defun build-seq-of-sounds (sounds sounds-length &optional result)

(let*  (


    (action1 (first-n sounds sounds-length))
    (action2  (if (plusp (om- (length sounds) sounds-length))
                  (last-n sounds (om::om- (length sounds) sounds-length))
                action1)))

(if 
    (plusp (om- (length sounds) sounds-length))
    (setf sounds (build-seq-of-sounds action2 sounds-length (push action1 result)))

  (x-append (cdr (reverse (x-append result (list action1)))) (list (flat
                                                              (let* (
                                                                   (action1 (last (x-append result (list action1))))
                                                                   (action2 (equal (length (flat action1)) 1)))
                                                               (if action2 (x-append action1 (sound-silence 0.0001)) action1))))))))

;====================================================================================
(defun sound-seq-list-multi-threading (sounds)

(let* (
        (first-action1 
          (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
        (second-action1 (ckn-make-mail-box first-action1))
        (action1 
            (loop 
                :for sound-loop :in sounds
                :for names-loop :in first-action1
                :for mail-box-loop :in second-action1 
                :do 
                        (mp:process-run-function names-loop () 
                                (lambda (x w) (mp:mailbox-send w 
                                                                (sound-seq-list x 0.001)))
                                sound-loop mail-box-loop)))



;; ======================================================

(action2 
  (loop with mailbox-empty = nil :while 
          (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1)))
            :do (let*
                    ((box-remove (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1))))
            mailbox-empty)))

;; ======================================================

(action3 (mapcar (lambda (x) (mp:mailbox-peek x)) second-action1))

(action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))

(sound-seq-list action4 0.001)))


;; ============

(defun voice->samples-sound-fun (voice1 pan temp-files)  nil)



;====================================================================================

(defun voice->samples-sound-om6-fun (voice1 pan temp-files) 
 
(let* (

(action1
    (loop :for ckn-LOOP1 :in (choose-to-rest voice1)
        :for ckn-LOOP2 :in (om6-true-durations voice1)
        :collect
        (let*
            (
              (midis-no-om6 (make-instance 'chord-seq :lmidic (chords voice1)))
              (box-choose1 (choose (lmidic midis-no-om6) ckn-LOOP1)) ;; fazer isso virar um chord-seq
                (box-choose2 (choose (lchan midis-no-om6) ckn-LOOP1))
                (box-choose3 (choose (lvel midis-no-om6) ckn-LOOP1))
                (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) '(-50 50)  (choose pan ckn-LOOP1)))
                (box-first1 (first box-choose3)))

(if (plusp ckn-LOOP2) ;;silencio ou nÃ£o 

;; NOTA 
(sound-fade 

        (sound-stereo-pan (sound-mono-to-stereo 
            (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFÃ”NICO

            ;;;;; MONOFONICO

;;;; COLOCAR MEIO PARA APAGAR ARQUIVOS TEMPORÃRIOS

(sound-vol 
    (sound-cut 
            (samples-menores 
                (om-abs (ms->sec ckn-LOOP2))

                (objfromobjs (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2)))
                                (ircam-instruments
                                    (first (approx-m box-choose1 2))
                                    (first box-choose2)
                                    box-first1)
                                (ckn-sound-transpose 
                                    (ircam-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1)
                                (first (om- box-choose1 (approx-m box-choose1 2))))) (make-instance 'sound nil)))
        0.0 
        (om-abs (ms->sec ckn-LOOP2)))
    
    (om-scale box-first1 0.001 0.999 1 110)) ;;;;; FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

;;;;; Com acorde

(sound-mix-list 
            (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3))))
            (first box-choose4)
            (second box-choose4)) 0.01 0.01)

;;silencio 

            (sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))

;;; ================= Apagar temp files

(if temp-files 
        (ckn-clear-temp-files))

;;; ================= Finalizar
                                            

action1))

;;; ================================================================================

(defun samples-menores (ckn-time ckn-sound)

(if (om::om> ckn-time (sound-dur ckn-sound))
    (sound-seq ckn-sound (sound-silence (om+ (om- ckn-time (sound-dur ckn-sound)) 0.03)) 0.001)
    ckn-sound))

;;; ================================================================================

(defun acordes-de-samples (ckn-time midic channel velocity)

    (loop :for ckn-LOOP1 :in midic
        :for ckn-LOOP2 :in channel
        :for ckn-LOOP3 :in velocity
        :collect (let* (
                    (action1 
                        (objfromobjs 
                          (if 
                                (equal 0 (om- ckn-LOOP1 (approx-m ckn-LOOP1 2)))
                                (ircam-instruments 
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                (ckn-sound-transpose
                                            (ircam-instruments
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                            (om- ckn-LOOP1 (approx-m ckn-LOOP1 2))))
                            (make-instance 'sound))))

                                              
                        (sound-vol (sound-cut (samples-menores ckn-time action1) 0.0 ckn-time) (om-scale ckn-LOOP3 0.001 1 1 127)))))

;;; ================================================================================

(lambda (sounds)

(let* (
        (first-action1 (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
        (second-action1 (ckn-make-mail-box first-action1))
        (action1 
            (loop 
                :for sound-loop :in sounds
                :for names-loop :in first-action1
                :for mail-box-loop :in second-action1 
                :do 
                        (mp:process-run-function names-loop () 
                                (lambda (x w) (mp:mailbox-send w 
                                                                (sound-seq-list x 0.001)))
                                (print sound-loop) mail-box-loop)))



;; ======================================================

(action2 
  (loop with mailbox-empty = nil :while 
          (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1)))
            :do (let*
        ((box-remove (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1))))
            mailbox-empty)))

;; ======================================================

(action3 (mapcar (lambda (x) (mp:mailbox-peek x)) second-action1))

(action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))

(sound-seq-list action4 0.001)))




;;;;; Falta sincronizar os sons


;; ======================================================
;; (sound-seq-list-multi-threading-until-finish (build-seq-of-sounds sounds list-per-threading)


(defun sound-seq-list-multi-threading-until-finish (sounds)

(let* (
        (first-action1 
          (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
        (second-action1 (ckn-make-mail-box first-action1))
        (action1 
            (loop 
                :for sound-loop :in sounds
                :for names-loop :in first-action1
                :for mail-box-loop :in second-action1 
                :do 
                        (mp:process-run-function names-loop () 
                                (lambda (x w) (mp:mailbox-send w 
                                                                (sound-seq-list x 0.001)))
                                sound-loop mail-box-loop)))

;; ===============

(action2 
  (loop with mailbox-empty = nil :while 
          (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1)))
            :do (let*
                    ((box-remove (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1))))
            mailbox-empty)))

;; ==============

(action3 (mapcar (lambda (x) (mp:mailbox-peek x)) second-action1)))

(loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))

;;; ================================================================================

(defun recursive-sound-seq (sounds list-per-threading)
(sound-seq-list-multi-threading-until-finish (build-seq-of-sounds sounds list-per-threading)))



;; ===================================================================================
#| 
(let* () 
      (eval (flat (get-slot-val 
                     (let
                         ((tb
                           (make-value-from-model 'textbuffer 
                                (probe-file (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN")))) nil)))
                       (setf (reader tb) :lines-cols) tb) "CONTENTS")))

(if *first-time-load*
    (let* () 
      (save-as-text '(((defvar *first-time-load* nil))) (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN"))))
      (hqn-web:browse "https://www.charlesneimog.com/"))))



  |#


;; (sound-seq-list-multi-threading-until-finish (build-seq-of-sounds sounds list-per-threading)

;; ====================================================== THIS IS A AUTO-PROMOTION ================================= 

(defun sound-seq-list-multi-threading-until-finish (sounds)

(let* (
        (first-action1 
          (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
        (second-action1 (ckn-make-mail-box first-action1))
        (action1 
            (loop 
                :for sound-loop :in sounds
                :for names-loop :in first-action1
                :for mail-box-loop :in second-action1 
                :do 
                        (mp:process-run-function names-loop () 
                                (lambda (x w) (mp:mailbox-send w 
                                                                (sound-seq-list x 0.001)))
                                sound-loop mail-box-loop)))

;; ===============

(action2 
  (loop with mailbox-empty = nil :while 
          (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1)))
            :do (let*
                    ((box-remove (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) second-action1))))
            mailbox-empty)))

;; ==============

(action3 (mapcar (lambda (x) (mp:mailbox-peek x)) second-action1)))
(gc-all)
(loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))

;;; ================================================================================

(defun recursive-sound-seq (sounds list-per-threading)
(sound-seq-list-multi-threading-until-finish (build-seq-of-sounds sounds list-per-threading)))

;;; ================================================================================

(defun save-temp-sounds (sounds) 

(let* (
    (first-action1 
        (mapcar 
            (lambda (x) (string+ "Sound-" x))
                (mapcar (lambda (x) (format nil "~6,'0D" x)) (om::arithm-ser 1 (length sounds) 1)))))

(loop :for loop-sound :in sounds
      :for loop-names :in first-action1
      :collect (om:save-sound loop-sound (merge-pathnames "om-ckn/" (outfile (string+ loop-names ".wav")))))))

;;; ================================================================================

(defun sox-sequence (sounds sequence-name)

(let* (
(action1 (save-temp-sounds sounds))
(action2 (om::save-sound (first sounds) (merge-pathnames "om-ckn/" (outfile (string+ "Sound-001" ".wav")))))
(action3 (x-append action2 (first-n action1 (1- (length sounds)))))
(action4 (om-cmd-line 
          (string+ 
           (list->string-fun (list (namestring *SOX-PATH*)))
           " "
           "--combine sequence " 
           (list->string-fun (list (namestring (merge-pathnames "om-ckn/" (outfile "*.wav")))))
           " "
           (list->string-fun (list (namestring (outfile sequence-name)))))))
(action5 (loop-until-probe-file (outfile sequence-name))))
(sleep 5)
(ckn-clear-temp-files)
action5))


;;; ================================================================================

(compile 'voice->samples-sound-fun)
(compile 'samples-menores)
(compile 'acordes-de-samples)
(compile 'build-sound-mix-fun)
(compile 'build-sound-sequence-fun)
(compile 'normalize-chord-seq)
(compile 'true-durations)
(compile 'ckn-transpose-a-sound) 
(compile 'name-of-the-sound)
(compile 'ckn-string-name)
(compile 'build-seq-of-sounds)


