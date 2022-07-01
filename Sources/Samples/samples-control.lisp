(in-package :om)

(setf *om-ckn-temp-sound-sox* 0)

(defun make-number-for-temp-sound ()
    (let* (
        (sound_number (+ *om-ckn-temp-sound-sox* 1)))
        (setf *om-ckn-temp-sound-sox* sound_number)
        (format nil "~8,'0D" *om-ckn-temp-sound-sox*)))


(defun reset-om-ckn-temp-sound-sox ()
(setf *om-ckn-temp-sound-sox* 0)
t)



;  ======================== SOX controls ================================

(defun ckn-transpose-a-sound (instrumentos desvio) 

(let* (
(action1 (string-to-list (get-filename instrumentos) "-"))
(action2 (1- (length (om::list! action1))))
(action3 (loop :for x :in action1 :collect (om::string+ x "-")))
(action4 (ckn-string-name (list! (first-n action3 action2))))
(action5 (string+ action4 (format nil "~d-cents" desvio) ".wav"))
(action6 (tmpfile action5 :subdirs "om-ckn"))
(probe (probe-file action6))
(action7 (namestring action6)))
(if (not probe)
    (ckn-cmd-line (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
          " "
          (list->string-fun (list instrumentos))
          " "
          (list->string-fun (list action7))
          (format nil " pitch ~d" desvio)))
    action6)
(loop-until-probe-file action6)))

;; Fazer um código mais bonito

;; ===============================================================================

(defun ckn-transpose (nome cents)

(let* (
(pathname (namestring nome)))
 (progn
  (loop :with ckn-loop 
        := nil 
        :while (setf ckn-loop (equal (probe-file (ckn-transpose-a-sound pathname cents)) nil))
        :do ckn-loop))
  (probe-file (ckn-transpose-a-sound pathname cents))))

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
    (action3 (progn
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
(defun sound-mix-multi (sounds)
(let* (
    (action1 
          (mapcar (lambda (x) (string+ "Sound-mix-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
    (action2 (ckn-make-mail-box action1))
    (action3 (progn
                    (loop 
                            :for sound-loop :in sounds
                            :for names-loop :in action1
                            :for mail-box-loop :in action2 
                        :do 
                        (mp:process-run-function names-loop () (lambda (x w) (mp:mailbox-send w (build-sound-mix-fun x))) sound-loop mail-box-loop)) 
                    (loop-until-finish-process action2) ;; espera todos os processos terminarem
                    (mapcar (lambda (x) (mp:mailbox-peek x)) action2))) ;; coleta os dados
    (action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))
    (gc-all)
    (build-sound-mix-fun action4)))

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
                                    (print (FULL-SOL-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1))
                                    (sound-transpose-sox 
                                        (FULL-SOL-instruments
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
(if temp-files (clear-subdir-temp-files "om-ckn")) ;;; ================= Apagar temp files
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
                                (FULL-SOL-instruments
                                    (first (approx-m box-choose1 2))
                                    (first box-choose2)
                                    box-first1)
                                (sound-transpose-sox 
                                    (FULL-SOL-instruments
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
        (clear-subdir-temp-files "om-ckn"))

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
                                                    (FULL-SOL-instruments (first (approx-m box-choose1 2)) (first box-choose2) box-first1)
                                                    (sound-transpose-sox 
                                                                (FULL-SOL-instruments 
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

(if temp-files (clear-subdir-temp-files "om-ckn"))

;;; ================= Finalizar                        
action1))

;;; ================================================================================

(defun samples-menores (ckn-time ckn-sound)

(if (om::om> ckn-time (om::sound-dur (make-value-from-model 'sound ckn-sound nil))) 
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
                                
                                    (FULL-SOL-instruments 
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                    (sound-transpose-sox
                                            (FULL-SOL-instruments
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                            (om- ckn-LOOP1 (approx-m ckn-LOOP1 2))))
                            nil)))          
                        (sound-vol (sound-cut (samples-menores ckn-time action1) 0.0 ckn-time) (om-scale ckn-LOOP3 0.001 1 1 127)))))



;; ====================================================== THIS IS A AUTO-PROMOTION ================================= 

(progn 
      (eval (flat (get-slot-val 
                     (let
                         ((tb
                           (make-value-from-model 'textbuffer 
                                (probe-file (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN")))) nil)))
                       (setf (reader tb) :lines-cols) tb) "CONTENTS")))

    (if *first-time-load*
        (progn 
            (save-as-text '(((defvar *first-time-load* nil))) (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN"))))
            (hqn-web:browse "https://www.charlesneimog.com/")
           )))

;;; ================================================================================

(defun save-temp-sounds (sounds &optional if-needed) 
    (let* (
            (first-action1 
                (mapcar 
                    (lambda (x) (string+ "Sound-" if-needed x))
                        (mapcar (lambda (x) (format nil "~6,'0D" x)) (om::arithm-ser 1 (length (om::list! sounds)) 1)))))
      
            (loop :for loop-sound :in (om::list! sounds)
                :for loop-names :in first-action1
                :collect (ckn-gc-all (om:save-sound loop-sound (merge-pathnames "om-ckn/" (tmpfile (string+ loop-names ".wav"))))))))


;;; ================================================================================

(defun save-temp-sounds-index (sounds index) 
    (let* (
            (first-action1 
                (mapcar (lambda (x) (string+ "Sound-" x)) (mapcar (lambda (x) (format nil "~6,'0D" x)) (list index)))))
      
            (loop :for loop-sound :in (list sounds)
                :for loop-names :in first-action1
                :collect (om:save-sound loop-sound (merge-pathnames "om-ckn/" (tmpfile (string+ loop-names ".wav")))))))

;;; ================================================================================

(defun sox-sequence (sounds sequence-name)

(let* (
(action1 (save-temp-sounds sounds))
(action2 (om::save-sound (first sounds) (merge-pathnames "om-ckn/" (tmpfile (string+ (make-number-for-temp-sound) ".wav")))))
(action3 (x-append action2 (first-n action1 (1- (length sounds)))))
(action4 (ckn-cmd-line 
          (string+ 
           (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
           " "
           "--combine sequence " 
           (list->string-fun (list (namestring (merge-pathnames "om-ckn/" (tmpfile "*.wav")))))
           " "
           (list->string-fun (list (namestring (tmpfile sequence-name)))))))
(action5 (loop-until-probe-file (tmpfile sequence-name))))

(clear-subdir-temp-files "om-ckn")
action5))

;;; ================================================================================


(defun sound-vol-sox-fun (sounds volume)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (tmpfile (string+ (first (om::string-to-list (get-filename sound-in-path) ".")) "-" (make-number-for-temp-sound) "-vol-correction" ".wav"))))))
  (action-sound-vol (format nil " -v ~d " volume))
  (line-command 
    (string+ sox-path " " action-sound-vol " " (format nil " -b ~d " (get-pref-value :audio :resolution)) (list->string-fun (list (namestring sound-in-path)))  " " (list->string-fun sound-in-out)))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
  ;(ckn-clear-the-file sounds)
    (car sound-in-out)))

;;; ================================================================================

(defun sound-mix-sox-fun (sounds name)
    (let* (
            (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
            (sound-in-path (names-to-mix sounds))
            (sound-in-out 
                (list (namestring (merge-pathnames "om-ckn/" 
                    (tmpfile (string+ name "-" (make-number-for-temp-sound) "-mix-sound" ".wav"))))))
            (line-command 
                (string+ sox-path " " " --combine mix " " " (format nil " -b ~d " (get-pref-value :audio :resolution))  sound-in-path " " (list->string-fun sound-in-out) )))
            (om-cmd-line line-command)
            (om-print (format nil "processando ~d samples!" (length sounds)) "Aguarde")
            (loop-until-probe-file (car sound-in-out))
            (car (om::list! sound-in-out))))

(compile 'sound-mix-sox-fun)
;;; ================================================================================

(defun prevent-mix-error (loop-in-parts actual-division)

(let* (
    (tamanhos (mapcar (lambda (x) (length x)) loop-in-parts))
    (if-length-equal-1 (remove nil (mapcar (lambda (x) (equal x 1)) tamanhos))))
    (if (null if-length-equal-1)
        loop-in-parts
        (setf loop-in-parts (prevent-mix-error (loop-in-parts (flat loop-in-parts) (- actual-division 1) (- actual-division 1)) (- actual-division 1))))))
        


;;; ================================================================================
(defun sound-mix-sox-responsive (sounds nomes number)
(let*  (
       (action1 (prevent-mix-error (loop-in-parts (flat sounds) 20 20) 20))
       (names (arithm-ser 1 (length (flat action1)) 1))
       (after-number (+ number 1))
       (action2 (mapcar (lambda (x y) (sound-mix-sox (flat x) (string+ (make-number-for-temp-sound) (write-to-string y) "-inside"))) action1 names)))
       (if (om::om= (length (flat action2)) 1)
           (car (flat action2))
         (if (om::om< (length (flat action2)) 20)
         (sound-mix-sox (flat action2) (string+ nomes "-finish"))
         (setf sounds (sound-mix-sox-responsive action2 nomes after-number))))))

(compile 'sound-mix-sox-responsive)

;;; ================================================================================

(defun sound-seq-sox-responsive (sounds nomes number)
(let*  (
       (action1 (loop-in-parts (flat sounds) 20 20))
       (names (arithm-ser 1 (length (flat action1)) 1))
       (after-number (+ number 1))
       (action2 (mapcar (lambda (x y) (sound-seq-sox (flat x) (string+ (make-number-for-temp-sound) (write-to-string y) "-seq-inside"))) action1 names)))
       (if (om::om= (length (flat action2)) 1)
           (car (flat action2))
         (if (om::om< (length (flat action2)) 20)
         (sound-seq-sox (flat action2) (string+ nomes "-seq-finish"))
         (setf sounds (sound-seq-sox-responsive action2 nomes after-number))))))

(compile 'sound-seq-sox-responsive)

;;; ================================================================================

(defun sound-seq-sox-fun (sounds name)
    (let* (
            (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
            (sound-in-path (names-to-mix sounds))
            (sound-in-out 
                (list (namestring (merge-pathnames "om-ckn/" 
                    (tmpfile (string+ name (make-number-for-temp-sound) "-seq-sound" ".wav"))))))
            (line-command 
                (string+ sox-path " " (format nil " -b ~d " (get-pref-value :audio :resolution)) " --combine sequence " " "  sound-in-path " " (list->string-fun sound-in-out)))
            (the-command (ckn-cmd-line line-command))
            (loading (loop-until-probe-file (car sound-in-out))))
            ;(ckn-clear-the-file sounds)
                (car sound-in-out)))

;;; ================================================================================

(defun sound-fade-sox-fun (sounds fade)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (tmpfile (string+ (first (om::string-to-list (get-filename sound-in-path) ".")) "-" (make-number-for-temp-sound) "-with-fade" ".wav"))))))
  (action-sound-fade (format nil " fade p ~d ~d" (first fade) (second fade)))
  (line-command 
    (string+ sox-path " " (format nil " -b ~d " (get-pref-value :audio :resolution)) (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out) " " action-sound-fade ))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
    (car sound-in-out)))

;;; ================================================================================

(defun sound-cut-sox-fun (sounds in out)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (tmpfile (string+ (first (om::string-to-list (get-filename sound-in-path) ".")) "-" (make-number-for-temp-sound) "-cut" ".wav"))))))
  (action-sound-vol (format nil " trim ~d ~d " in out))
  (line-command 
    (string+ sox-path " " (format nil " -b ~d " (get-pref-value :audio :resolution)) (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out) " " action-sound-vol))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
  ;(ckn-clear-the-file sounds)
    (car sound-in-out)))

;;; ================================================================================

(defun sound-mono-to-stereo-sox-fun (x)

        (ckn-cmd-line (string+ 
                (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
                " "
                (format nil " -b ~d " (get-pref-value :audio :resolution))
                (list->string-fun (list (namestring x)))
                " "
                (list->string-fun (list (namestring (tmpfile (string+ (get-filename x) "-v-stereo.wav") :subdirs "om-ckn"))))
                " "
                " channels 2 "))
        (loop-until-probe-file (tmpfile (string+ (get-filename x) "-v-stereo.wav") :subdirs "om-ckn"))
(tmpfile (string+ (get-filename x) "-v-stereo.wav") :subdirs "om-ckn"))

(compile 'sound-mono-to-stereo-sox-fun)


;;; ================================================================================

(defun sound-denoise-sox-fun (sound)

(let* (
        (sox-path (list->string-fun (list (namestring (get-pref-value :externals :sox-exe)))))
        (sound-markers (if (om::markers sound) (om::markers sound)  '(0 200)))
        (saved-tmp-sound (save-temp-sounds (om::list! sound) "with-Noise"))
        (saved-tmp-sound-formated (string+ (car (string-to-list (namestring (car saved-tmp-sound)) ".")) "-denoise.wav"))
        (cut-noise (save-temp-sounds (om::list! (sound-fade (om::sound-cut sound (first sound-markers) (second sound-markers)) 0.005 0.005))))
        (text-of-noise (tmpfile (string+ "Noise" ".prof") :subdirs "om-ckn"))
        (first-cmd-line (om-cmd-line (string+ sox-path " " (list->string-fun (list (namestring (car cut-noise)))) " -n noiseprof " (list->string-fun (list (namestring text-of-noise))))))
        (second-cmd-line (om-cmd-line (string+ sox-path " " (format nil " -b ~d " (get-pref-value :audio :resolution)) (list->string-fun (list (namestring (car saved-tmp-sound)))) " " (list->string-fun (list (namestring saved-tmp-sound-formated))) " noisered " (list->string-fun (list (namestring  text-of-noise))) " 0.21 "))))
        (progn
            (ckn-clear-the-file (car saved-tmp-sound))
            (ckn-clear-the-file (car cut-noise))
            (ckn-clear-the-file text-of-noise)
           saved-tmp-sound-formated)))


;;; ================================================================================

(compile 'voice->samples-sound-fun)
(compile 'samples-menores)
(compile 'acordes-de-samples)
(compile 'build-sound-mix-fun)
(compile 'build-sound-sequence-fun)
(compile 'normalize-chord-seq)
(compile 'ckn-transpose-a-sound) 
(compile 'build-seq-of-sounds)
(compile 'sound-seq-list-multi-threading)
(compile 'voice->samples-sound-ITD-fun)

