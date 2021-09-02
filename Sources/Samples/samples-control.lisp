(in-package :om)

;; Caminho para MrsWatson  ========================

(add-preference :externals :MrsWatson-exe "MrsWatson Path" 
                :file nil)

;; Caminho para Plugins DLL  ========================

(add-preference :externals :plugins "Plugins DLL" 
  :path nil)

;; Caminho para FPX Presets ========================

(add-preference :externals :fxp-presets  "FXP Presets" 
  :path nil)

;; Caminho para Sox  ========================

(add-preference :externals :sox-exe "Sox Path" 
                :file nil)

;; Orchidea Instruments  ========================

(add-preference :externals :OrchideaSOL "SOL Samples Library" 
                :path nil)

;  ========================

(defmethod! define-fxp-presets ((fxp-presets string))
:initvals '(nil)
:indoc '("Define the fxp-presets to use in your sound.") 
:icon '17359
:doc "It defines the fxp-presets to use in your sound with the object ckn-VST2. You need be careful because the binaries that I am using not accept very long paths. Then prefer smaller paths."

(let* (
(action1 (probe-file (merge-pathnames fxp-presets (namestring (get-pref-value :externals :fxp-presets))))))
(if (equal nil action1) (let* ((action1 (om-print "FXP Presets nao encontrado" "Abortando"))) (om-abort)) action1)))

;  ========================

(defmethod! define-VST2-plugin ((plugin-name string))
:initvals '(nil)
:indoc '("Define the fxp-presets to use in your sound.") 
:icon '17359
:doc "It defines the fxp-presets to use in your sound with the object ckn-VST2."

(let* (
(action1 (probe-file (merge-pathnames plugin-name (namestring (get-pref-value :externals :plugins))))))
(if (equal nil action1) (let* ((action1 (om-print "Plugin nao encontrado" "Abortando"))) (om-abort)) action1)))

;  ========================

(defmethod! ckn-VST2 ((sound sound) (sound-out string) (plugin-path string) (fxp-path string))
:initvals '(nil nil nil nil)
:indoc '("Use VST2 plugins in your sounds") 
:icon '17359
:doc "It allows to use VST2 plugins in your sounds."

(om-cmd-line 
 (string+ 
  (list->string-fun (list (namestring (get-pref-value :externals :MrsWatson-exe))))
    " --input " (list->string-fun (list (namestring (file-pathname sound))))
    " --output " (list->string-fun (list (namestring (outfile sound-out))))
    " --plugin " (list->string-fun (list (namestring (define-VST2-plugin plugin-path)))) "," 
                 (list->string-fun (list (namestring (define-fxp-presets fxp-path))))))

(ckn-clear-temp-files)

(make-value-from-model 'sound (outfile sound-out) nil))

;  ========================

(defmethod! ckn-VST2 ((sound sound) (sound-out pathname) (plugin-path string) (fxp-path string))
:initvals '(nil nil nil nil)
:indoc '("Use VST2 plugins in your sounds") 
:icon '17359
:doc "It allows to use VST2 plugins in your sounds."

(om-cmd-line 
 (string+ 
  (list->string-fun (list (namestring (get-pref-value :externals :MrsWatson-exe))))
    " --input " (list->string-fun (list (namestring (file-pathname sound))))
    " --output " (list->string-fun (list (namestring sound-out)))
    " --plugin " (list->string-fun (list (namestring (define-VST2-plugin plugin-path)))) "," 
                 (list->string-fun (list (namestring (define-fxp-presets fxp-path))))))

(ckn-clear-temp-files)

(make-value-from-model 'sound sound-out nil))


;  ========================

(defmethod! ckn-VST2 ((sound pathname) (sound-out string) (plugin-path string) (fxp-path string))
:initvals '(nil)
:indoc '("Use VST2 plugins in your sounds") 
:icon '17359
:doc "It allows to use VST2 plugins in your sounds."

(let* (
(action1 
(om-cmd-line 
 (print (string+ 
  (list->string-fun (list (namestring (get-pref-value :externals :MrsWatson-exe))))
    " --input " (list->string-fun (list (namestring sound)))
    " --output " (list->string-fun (list (namestring (outfile sound-out))))
    " --plugin " (list->string-fun (list (namestring (define-VST2-plugin plugin-path)))) "," 
                 (list->string-fun (list (namestring (define-fxp-presets fxp-path)))))))))

(make-value-from-model 'sound (outfile sound-out) nil)))

;  ========================

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
(action6 (merge-pathnames (string+ "om-ckn/" action5) (outfile "")))
(action7 (namestring action6)))
(om-cmd-line (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
          " "
          (list->string-fun (list instrumentos))
          " "
          (list->string-fun (list action7))
          (format nil " pitch ~d" desvio)))
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

(defun om6-true-durations (ckn)

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
         (if (= 2 (length onsets)) (list (car result) (second result)) result))))

;====================================================================================

(defun build-sound-sequence-fun (sounds fade-bewteen-sound)

(let*  (
    (action1 (om::sound-seq (first sounds) (second sounds) fade-bewteen-sound))
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
(8400 "C6") (8500 "C#6") (8600 "D6") (8700 "D#6") (8800 "E6") (8900 "F6") (9000 "F#6") (9100 "G6") (9200 "G#6") (9300 "A6") (9400 "A#6") (9500 "B6")    
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
    (action1 
          (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
    (action2 (ckn-make-mail-box action1))
    (action3 (let* ()
                    (loop 
                            :for sound-loop :in sounds
                            :for names-loop :in action1
                            :for mail-box-loop :in action2 
                        :do 
                        (mp:process-run-function names-loop () (lambda (x w) (mp:mailbox-send w (sound-seq-list x 0.001))) sound-loop mail-box-loop)) 
                    (loop-until-finish-process action2) ;; espera todos os processos terminarem
                    (mapcar (lambda (x) (mp:mailbox-peek x)) action2))) ;; coleta os dados
    (action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))
    (gc-all)
    (sound-seq-list action4 0.001)))

;====================================================================================

(defun voice->samples-sound-fun (voice1 pan temp-files) 
 
(let* (
(action1
    (loop   :for ckn-LOOP1 :in (choose-to-rest voice1)
            :for ckn-LOOP2 :in (om6-true-durations voice1)
            :collect
                (let*
                    ((box-choose1 (choose (lmidic voice1) ckn-LOOP1))
                    (box-choose2 (choose (lchan voice1) ckn-LOOP1))
                    (box-choose3 (choose (lvel voice1) ckn-LOOP1))
                    (box-first1 (first box-choose3))
                    (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) '(-50 50)  (choose pan ckn-LOOP1))))

(if (plusp ckn-LOOP2) ;;silencio ou nao 
;; SE FOR NOTA 
    (om::sound-fade 
        (om::sound-stereo-pan (sound-mono-to-stereo 
                                (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFONICO
            ;;;;; MONOFONICO
            (sound-vol 
                (sound-cut 
                    (samples-menores (om-abs (ms->sec ckn-LOOP2)) (make-value-from-model 'sound 
                                (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2))) 
                                    (ircam-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1)
                                    (ckn-sound-transpose 
                                        (ircam-instruments
                                            (first (approx-m box-choose1 2))
                                            (first box-choose2)
                                            box-first1)
                                    (first (om- box-choose1 (approx-m box-choose1 2))))) nil)) 0.0 (om-abs (ms->sec ckn-LOOP2)))
                (om-scale box-first1 0.001 0.999 1 110))  ;;;;; FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

            ;;;;; POLIFONICO

            (sound-mix-list 
                        (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3))))
                        (first box-choose4)
                        (second box-choose4)) 0.01 0.01)
;;silencio 
            (sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))
(if temp-files (ckn-clear-temp-files)) ;;; ================= Apagar temp files
action1))

;;; ===========================================================

(defun voice->samples-sound-om6-fun (voice1 pan temp-files) 
 
(let* (

(action1
    (loop :for ckn-LOOP1 :in (choose-to-rest voice1)
        :for ckn-LOOP2 :in (om6-true-durations voice1)
        :collect
        (let*
            ((box-choose1 (choose (lmidic (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1)) ;; fazer isso virar um chord-seq
                (box-choose2 (choose (lchan (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1))
                (box-choose3 (choose (lvel (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1))
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
                    (make-instance 'sound nil
                            (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2))) 
                                (ircam-instruments
                                    (first (approx-m box-choose1 2))
                                    (first box-choose2)
                                    box-first1)
                                (ckn-sound-transpose 
                                    (ircam-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1)
                                (first (om- box-choose1 (approx-m box-choose1 2))))) nil))
        0.0 
        (om-abs (ms->sec ckn-LOOP2)))
    
    (om-scale box-first1 0.001 0.999 1 110))  ;;;;; FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

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

;;; ============================ COM ITD ===========================================

(defun voice->samples-sound-ITD-fun (voice1 pan temp-files) 
 
(let* (

    (action1
            (loop   :for ckn-LOOP1 :in (choose-to-rest voice1)
                    :for ckn-LOOP2 :in (om6-true-durations voice1)
                    :collect
        (let*
            ((box-choose1 (choose (lmidic voice1) ckn-LOOP1))
                (box-choose2 (choose (lchan voice1) ckn-LOOP1))
                (box-choose3 (choose (lvel voice1) ckn-LOOP1))
                (box-first1 (first box-choose3))
                (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) 15  (choose pan ckn-LOOP1))))

(if (plusp ckn-LOOP2) ;;silencio ou NAO 

;; NOTA 
(sound-fade 
        (ITD-Sound  (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFONICO
                            (sound-vol 
                                    (sound-cut (samples-menores (om-abs (ms->sec ckn-LOOP2)) 
                                        (make-value-from-model 'sound
                                                (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2)))
                                                    (ircam-instruments (first (approx-m box-choose1 2)) (first box-choose2) box-first1)
                                                    (ckn-sound-transpose 
                                                                (ircam-instruments 
                                                                        (first (approx-m box-choose1 2)) 
                                                                        (first box-choose2) box-first1)
                                        (first (om- box-choose1 (approx-m box-choose1 2))))) nil))
                                    0.0 
                                    (om-abs (ms->sec ckn-LOOP2)))
        
                            (om-scale box-first1 0.001 0.999 1 110))

                        ;;;;; Com acorde
                (sound-mix-list 
                        (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3)))
                            box-choose4) 0.001 0.001)
;;silencio 
(sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))
;;; ================= Apagar temp files

(if temp-files (ckn-clear-temp-files))

;;; ================= Finalizar                        
action1))

;;; ================================================================================

(defun samples-menores (ckn-time ckn-sound)

(if (om::om> ckn-time (om::sound-dur ckn-sound)) 
    (om::sound-seq ckn-sound (sound-silence (om::om+ (om::om- ckn-time (sound-dur ckn-sound)) 0.03)) 0.01)
    ckn-sound))

;;; ================================================================================

(defun acordes-de-samples (ckn-time midic channel velocity)

    (loop :for ckn-LOOP1 :in midic
        :for ckn-LOOP2 :in channel
        :for ckn-LOOP3 :in velocity
        :collect (let* (
                    (action1 
                        (make-value-from-model 'sound
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
                            nil)))          
                        (sound-vol (sound-cut (samples-menores ckn-time action1) 0.0 ckn-time) (om-scale ckn-LOOP3 0.001 1 1 127)))))



;; ====================================================== THIS IS A AUTO-PROMOTION ================================= 

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

;;; ================================================================================

(defun save-temp-sounds (sounds) 
    (let* (
            (first-action1 
                (mapcar 
                    (lambda (x) (string+ "Sound-" x))
                        (mapcar (lambda (x) (format nil "~6,'0D" x)) (om::arithm-ser 1 (length sounds) 1)))))
      
            (loop :for loop-sound :in sounds
                :for loop-names :in first-action1
                :collect (ckn-gc-all (om:save-sound loop-sound (merge-pathnames "om-ckn/" (outfile (string+ loop-names ".wav"))))))))


;;; ================================================================================

(defun save-temp-sounds-index (sounds index) 
    (let* (
            (first-action1 
                (mapcar (lambda (x) (string+ "Sound-" x)) (mapcar (lambda (x) (format nil "~6,'0D" x)) (list index)))))
      
            (loop :for loop-sound :in (list sounds)
                :for loop-names :in first-action1
                :collect (om:save-sound loop-sound (print (merge-pathnames "om-ckn/" (outfile (string+ loop-names ".wav"))))))))

;;; ================================================================================

(defun sox-sequence (sounds sequence-name)

(let* (
(action1 (save-temp-sounds sounds))
(action2 (om::save-sound (first sounds) (merge-pathnames "om-ckn/" (outfile (string+ "Sound-001" ".wav")))))
(action3 (x-append action2 (first-n action1 (1- (length sounds)))))
(action4 (om-cmd-line 
          (string+ 
           (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
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

(defun sound-vol-sox-fun (sounds volume)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (outfile (string+ (first (om::string-to-list (get-filename sound-in-path) ".")) "-vol-correction" ".wav"))))))
  (action-sound-vol (format nil " -v ~d " volume))
  (line-command 
    (string+ sox-path " " action-sound-vol " " (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out)))
  (the-command (om::om-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
    (car sound-in-out)))

;;; ================================================================================
(compile 'voice->samples-sound-fun)
(compile 'samples-menores)
(compile 'acordes-de-samples)
(compile 'build-sound-mix-fun)
(compile 'build-sound-sequence-fun)
(compile 'normalize-chord-seq)
(compile 'ckn-transpose-a-sound) 
(compile 'name-of-the-sound)
(compile 'ckn-string-name)
(compile 'build-seq-of-sounds)
(compile 'sound-seq-list-multi-threading)
(compile 'voice->samples-sound-ITD-fun)

