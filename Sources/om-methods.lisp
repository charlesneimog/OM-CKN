
(in-package :om)

; ========================================

(defmethod! complex-numbers-parts ((list list))
:initvals ' (NIL)
:indoc ' ("list of complex-numbers")
:icon '17359
:numouts 2
:outdoc '("imagpart" "realpart")
:doc ""
(values 
  (mapcar (lambda (x) (imagpart x)) list)
  (mapcar (lambda (x) (realpart x)) list)))

; ================================ Python Methods ==================================

(defmethod! bpf-python ((X list) (Y list) &optional (thickness 1) (color 'black))
:initvals '('(1 2 3) '(3 2 1) 0.2 black)
:indoc ' ("X points" "Y points" "Thickness" "The color (for example black)")
:icon '17359
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.

For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (x_if (if (not x) (om::arithm-ser 1 (length y) 1) x))
      (y_if (if (not y) (om::arithm-ser 1 (length x) 1) y))
      (X-PYTHON (lisp-list_2_python-list x_if))
      (Y-PYTHON (lisp-list_2_python-list y_if)))
(mp:process-run-function (string+ "BPF-PYTHON" (ckn-int2string (om::om-random 1 1000)))
      () 
                  (lambda (x-axis y-axis) (if
                                              (equal *app-name* "om-sharp")
                                                     (bpf-python-fun x-axis y-axis thickness color)
                                                     (bpf-python-om x-axis y-axis thickness)
                                            )) X-PYTHON Y-PYTHON)))

; ==================================================================================

(defmethod! save-bpf-python ((X list) (Y list) &optional (thickness 0.4) (outfile nil) (color 'black) (blackbackgroud nil) (dpi 300))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.

For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (outfile_in_python (list->string-fun (list (namestring outfile))))
      (black-backgroud (if blackbackgroud  "plt" "#plt")) 
      (x_if (if (not x) (om::arithm-ser 1 (length y) 1) x))
      (y_if (if (not y) (om::arithm-ser 1 (length x) 1) y))
      (X-PYTHON (lisp-list_2_python-list x_if))
      (Y-PYTHON (lisp-list_2_python-list y_if)))
(mp:process-run-function (string+ "Save-PYTHON-" (ckn-int2string (om::om-random 1 1000)))
      () 
                  (lambda (x-axis y-axis) (if
                                              (equal *app-name* "om-sharp")
                                                     (save-bpf-python-fun x-axis y-axis thickness color outfile_in_python black-backgroud dpi)))
                  X-PYTHON Y-PYTHON)))

; ==================================================================================

(defmethod! 3dc-python ((X list) (Y list) (Z list) &optional (thickness 1) (color 'black))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.

For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (X-PYTHON (lisp-list_2_python-list X))
      (Y-PYTHON (lisp-list_2_python-list Y))
      (Z-PYTHON (lisp-list_2_python-list Z)))
(mp:process-run-function (string+ "3DC-PYTHON" (ckn-int2string (om::om-random 1 1000)))
                 () 
                  (lambda (x-axis w-axis z-axis) (3dc-python-fun x-axis w-axis z-axis thickness color)) X-PYTHON Y-PYTHON Z-PYTHON)))


; ================================================== FFT-Class =====================

(defclass! ckn-fft-instance ()
( 
  (ckn-complex-numbers :initform '#(-6.1035157E-5 0.0) :initarg :ckn-complex-numbers :accessor ckn-complex-numbers)
  (sound-sample-rate :initform nil :initarg :sound-sample-rate :accessor sound-sample-rate)
  (fft-window :initform nil :initarg :fft-window :accessor fft-window)
  (ckn-hop-size :initform nil :initarg :ckn-hop-size :accessor ckn-hop-size)
  (fft-chunks :initform nil :initarg :fft-chunks :accessor fft-chunks)
  (ckn-tempo :initform nil :initarg :ckn-tempo :accessor ckn-tempo)
  (frequencias :initform nil :initarg :frequencias :accessor frequencias)
  (amplitudes :initform nil :initarg :amplitudes :accessor amplitudes)
  (phrase :initform nil :initarg :phrase :accessor phrase)
                                    )
  (:icon 17359))

;==================================================================================

(defclass! fft-complex-numbers ()
((complex-numbers :initform '#(-6.1035157E-5 0.0) :initarg :complex-numbers :accessor complex-numbers))
(:icon 17359))

;==================================================================================

(defclass! sound-bytes ()
((bytes :initform nil :initarg :bytes :accessor bytes))
(:icon 17359))
                                    
;==================================================

(defmethod! BORDEAUX-fft ((sound-array array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(time (fft-fun sound-array)))

;==================================================

(defmethod! do-fft ((sound sound) (fft-size number) (hop-size number) (window-type number))
:initvals '(nil 2048 512 nil)
:indoc '("Sound class" "FFT-size" "Hop-size" "Windows-type") 
:menuins '((3 (("hann" 1) ("blackman" 2) ("barlett" 3) ("hamming" 4) ("rectangular" 5) ("nenhuma" 6))))
:icon '17359
:doc "It does the FFT in a sound."

(if (compiled-function-p #'sapa-fft!) nil (compile 'sapa-fft!))

(case window-type
  (nil (om-message-dialog "You need to define which window-type (fourth inlet) will be used for the fft analysis.")))

(if 
    (equal *app-name* "om-sharp")
        (fft-ckn sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))

        (fft-ckn-om sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))))


;==================================================

(defmethod! complex-senoide-h ((freq number) (sec number) (samples-rate number))
:initvals '(nil)
:indoc '("Frequency in Hz" "Durations in sec." "Sample-rate")
:icon '17359
:doc "It does one senoide in the complex plan."

(let* (
      (durations (x-append 0 sec))
      (sec-samples (round (om::sec->samples sec samples-rate)))
      (sampling (nth 2 (om::multiple-value-list (om::om-sample durations sec-samples)))))
  (create-pure-tone-h freq sampling)))


;==================================================

(defmethod! complex-senoide ((freq number) (sec number) (samples-rate number))
:initvals '(nil)
:indoc '("Frequency in Hz" "Durations in sec." "Sample-rate")
:icon '17359
:doc "It does one senoide in the complex plan."

(let* (
      (durations (x-append 0 sec))
      (sec-samples (round (om::sec->samples sec samples-rate)))
      (sampling (nth 2 (om::multiple-value-list (om::om-sample durations sec-samples)))))
  (create-pure-tone freq sampling)))

;==================================================

(defmethod! fft->amplitude ((fft array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->amplitude-fun fft))

;=============================

(defmethod! fft->phrase ((fft array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->phrase-fun fft))

;==================================== ARRAY UTILITIES =======

(defmethod! list-to-array ((array-my list) (dimensions integer))
:initvals '((nil) (nil))
:indoc '("Sound class" "bla bla")
:icon '17359
:doc "It reads a wave file."

(list-to-array-fun array-my dimensions))

;=====================================

(defmethod! array-to-list ((my-array array))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359 
:doc "It reads a wave file."

(array-to-list-fun my-array))

;=====================================

(defmethod! by-N ((my-list list) (division-number number) (my-function function))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359 
:doc "It reads a wave file."

(by-N-fun my-list division-number my-function))

;=====================================

(defmethod! sound->bytes ((self sound))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359
:doc "It reads a wave file."

(if (equal *app-name* "om-sharp")
  (make-instance 'sound-bytes :bytes (sound->bytes-fun self))
  (sound->bytes-om-class self)))

;=====================================

(defmethod! bytes->sound ((self list) (quantos-canais number) (qual-canal number))
:initvals '(nil)
:indoc '("bytes 0 until 1 list" "number of channels" "where write?")
:icon '17359
:doc "It create a sound from list of bytes (0 until 1)."

(if (equal *app-name* "om-sharp")
    (bytes->sound-fun self quantos-canais qual-canal)))

;=====================================

(defmethod! cartopol ((fft cons))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:numouts 2
:outdoc ' ("phrase" "amplitude")
:icon '17359
:doc "Do the same thing that the cartopol of Max/MSP."

(values 
  (mapcar (lambda (x) (fft->phrase x)) fft)
  (mapcar (lambda (y) (fft->amplitude y)) fft)))


;=====================================

(defmethod! bin->freq ((fft-bin list) (sample-rate number) (fft-size number))
:initvals ' ((41 82 122 163 204) 44100 4096)
:indoc ' ("FFT bin number" "Sample-rate" "Fft-size")
:numouts 1
:outdoc ' ("Frequency")
:icon '17359
:doc ""

(bin->freq-fun fft-bin sample-rate fft-size))
;=================

(defun bin->freq-fun (fft-bin sample-rate fft-size)
  (om::om* fft-bin (float (om::om/ sample-rate fft-size))))

;==================================================

(defmethod! half ((fft-array array))
:doc "Só é necessário metade do resultado do FFT."

(half-fun half))

; =================================================


(defmethod! fft->dB ((amplitudes list))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
         (fft-size (* 2 (length amplitudes)))
				(action1 (om/ amplitudes (* 0.10519141 (print fft-size))))
				(action2 (mapcar (lambda (x) (log x 10)) action1)))
	(om* 20 action2)))

;; ============================================= SDIF ========================================

(defmethod! sdif->list ((sdif-file sdiffile))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(sdif->list-fun sdif-file))

;; =============================================

(defun sdif->list-fun (sdif-file) 

(let* (
    (action1 (second (multiple-value-list 
                (om::getsdifdata sdif-file 0 "1TRC" "1TRC" '(0 1 2) nil nil nil nil))))
    (action2 (getsdifframes sdif-file)))

        (loop 
            :for cknloop 
            :in (arithm-ser 1 (length action2) 1) 
                  :collect       
    
        (x-append 
              (get-slot-val (make-value-from-model 'sdifframe (posn-match action2 (1-  cknloop)) nil) "FTIME")
              (let* (

(action3-1 
        (posn-match 
                    (om::get-slot-val (make-value-from-model 'sdifmatrix 
                                          (first (om::get-slot-val 
                              (om::make-value-from-model 'sdifframe (posn-match action2 (1- cknloop)) nil)
                                            "LMATRIX")) nil) "DATA") '(0 1 2 3)))
(action3-2 (mat-trans (list (om::om-round (first action3-1)) (om::om-round (second action3-1) 2) (third action3-1) (fourth action3-1)))))
action3-2)))))

;; ====================================================

(defmethod! sdif-envelope ((sdif-file sdiffile))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(let* (
    (action1 (sdif->list-fun sdif-file))
    (action2 
        (mapcar (lambda (x)
          (reduce (lambda (xy xx) (+ xy xx))
              (mapcar
                  (lambda (xxx) (third xxx)) (last-n x (- (length x) 1))))) action1)))

(make-value 'bpf (list (list :x-points nil) (list :y-points action2)))))

;; ====================================================

(defmethod! save-spear-sdif ((sdif-frames list) (name symbol))
:initvals ' (NIL)
:indoc ' ("Sdif-File." "Name")
:icon '17359
:doc ""

(write-sdif-file sdif-frames :outpath
  (outfile (list->string-fun (x-append name '.sdif)))
  :types
  (list (make-value 'sdiftype (list (list :struct 'f) (list :signature "1TRC")
     (list :description (list (list "XNFO" "InfoMat") (list "XMAT" "datamat"))))))))

;; ====================================================

(defmethod! sound-seq-list ((sound list) &optional (fade-bewteen-sound 0.001))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(om-print "Aguarde alguns segundos!" "Verbose")

(let* (
      (action1 (build-sound-sequence-fun sound fade-bewteen-sound))
      (action2 
        (om::om-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))))

  (save-sound action1 (merge-pathnames (string+ "om-ckn/temp-sound" (list->string-fun (list (om::om-random 0 9999999))) ".wav") (outfile "")))))

;; ====================================================

(defmethod! sound-mix-list ((sound list))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(build-sound-mix-fun sound))

;; ====================================================


(defmethod! ckn-sound-transpose ((sound string) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(if (equal 0 cents) sound (ckn-transpose-a-sound sound cents)))

;; ====================================================

(defmethod! ckn-sound-transpose ((sound pathname) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(if (equal 0 cents) (namestring sound) (ckn-transpose-a-sound (namestring sound) cents)))

;; ====================================================

(om::defmethod! choose ((notelist list) (chord-n list))
:initvals ' ((1 2 3 4 5 6 7 8 9 10) 2)
:indoc ' ("List or list of lists of anything" "What element(s) do you want?") 
:icon '17359
:doc "This object choose an element in a list; or a list in a list of lists. If you put more that one element in the second inlet this object will choose all the elements that you put in second inlet.
Inlet1: (7 8 9 10 458)
Inlet2: (1 3 5)
Result: (7 9 458)."

(if (equal nil chord-n) nil (posn-match notelist (om::om- chord-n 1))))

;; ====================================================

(om::defmethod! choose ((notelist list) (chord-n number))
:initvals ' ((1 2 3 4 5 6 7 8 9 10) 2)
:indoc ' ("List or list of lists of anything" "What element(s) do you want?") 
:icon '17359
:doc "This object choose an element in a list; or a list in a list of lists. If you put more that one element in the second inlet this object will choose all the elements that you put in second inlet.
Inlet1: (7 8 9 10 458)
Inlet2: (1 3 5)
Result: (7 9 458)."

(if (equal nil chord-n) nil (posn-match notelist (om::om- chord-n 1))))

;; ====================================================

(defmethod! choose-to-rest ((voice voice) &optional (number-2 1))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(ckn-action1  (loop :for ckn-plus :in (om6-true-durations voice) :collect (if (plusp ckn-plus) 0 1)))
(ckn-action2 (loop :for cknloop :in ckn-action1 :collect (if (= 0 cknloop) (setq number-2 (+ number-2 1)) nil))))
(let* (
     (ckn-action3-1 
     (if (equal nil (first ckn-action2)) 0 (first ckn-action2))))
     (if (equal nil (first ckn-action2)) (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) (om::om+ (om::om- ckn-action2 ckn-action3-1) 1))          
      )))


;; ====================================================

(defmethod! ckn-position ((list list) (my-number number))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "Check the ALL the position of one number in one list."

(let* (
(ckn-action1  (loop :for ckn-loop :in list 
                    :for my-position :in (om::arithm-ser 1 (length list) 1)
                    :collect 
                          (if (equal ckn-loop my-number) my-position nil))))
(remove nil ckn-action1)))


;; ====================================================

(defmethod! ckn-position ((list list) (my-number list))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "Check the ALL the position of one number in one list."

(let* (
(ckn-action1  (loop :for ckn-loop :in list 
                    :for my-position :in (om::arithm-ser 1 (length list) 1)
                    :collect 
                          (if (equal ckn-loop my-number) my-position nil))))
(remove nil ckn-action1)))

;; ====================================================

(defmethod! choose-to-rest ((list list) &optional (number-2 1))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(ckn-action1  (loop :for ckn-plus :in list :collect (if (plusp ckn-plus) 0 1)))

(ckn-action2 (loop :for cknloop :in ckn-action1 :collect (if (= 0 cknloop) (setq number-2 (+ number-2 1)) nil))))


(let* (
     (ckn-action3-1 
     (if (equal nil (first ckn-action2)) 0 (first ckn-action2))))
     (if (equal nil (first ckn-action2)) (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) (om::om+ (om::om- ckn-action2 ckn-action3-1) 1))          
      )))

;; ====================================================

(defmethod! u-list->string ((list list))
:initvals '(nil nil)
:indoc '("A list of names?") 
:icon '17359
:doc "Transform a list in one string."

(list->string-fun list))

;; ====================================================

#| 
(defmethod! om6-true-durations ((voice voice))
:initvals '(nil)
:indoc '("a voice" ) 
:icon 'tree
:doc "Imported from OM6."

(om6-true-durations voice))

|#

;; ====================================================

(defmethod! sound-seq-multi ((sounds list) &optional (list-per-threading 30))
:initvals '(nil)
:indoc '("a list of sounds." "Among of sounds per threading.")
:icon '17359
:doc "Like sound-seq-list but multithreading (more fast)."
        (gc-all)
    (let* (
    (action1 (sound-seq-list-multi-threading (build-seq-of-sounds sounds list-per-threading))))
    (om::om-cmd-line (string+ "powershell -command " 
                              (list->string-fun (list (string+ "del " 
                                                (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))
            (gc-all)
        action1))

;; ====================================================

(defmethod! voice->samples ((voice voice) &optional (pan nil) (temp-files t))
:initvals '(nil nil t)
:indoc '("a voice" "panoramic information - see the object sound-stereo-pan" "Clear temp files") 
:icon '17359
:doc "Imported from OM6. It can take."

(om-print "Aguarde!" "Verbose")

;; (ckn-clear-temp-files)

(if (equal (check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")

(if (om-print (equal *app-name* "om-sharp") "app-name")
    (voice->samples-sound-fun voice pan temp-files)
    (voice->samples-sound-om6-fun voice pan temp-files))

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))

;; ====================================================

(defmethod! voice->samples-ITD ((voice voice) &optional (pan nil) (temp-files t))
:initvals '(nil nil t)
:indoc '("a voice" "panoramic information - see the object sound-stereo-pan" "Clear temp files") 
:icon '17359
:doc "Imported from OM6. It can take."

(om-print "Aguarde!" "Verbose")

(ckn-clear-temp-files)

(if (equal (check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")

(if (om-print (equal *app-name* "om-sharp") "app-name")
    (voice->samples-sound-ITD-fun voice pan temp-files))

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))

;; ====================================================

(defmethod! talk-with-omlily nil
:initvals '(nil)
:indoc '("a voice" ) 
:icon '17359
:doc "Imported from OM6 and omlily developep by Karim Haddad.It just works fine in Windows OS that use the Pdf Xchange Editor."

(om-cmd-line "TASKKILL /IM PDFXEdit.exe") 
(osc-send '("/test" 0) "127.0.0.1" 3000))

;; ====================================================

(defmethod! sound-seq-sox ((sounds list) (name string))
:initvals '(nil)
:indoc '("list of sounds" "Name of the sound after build sequence.") 
:icon '17359
:doc "Imported from OM6 and omlily developep by Karim Haddad.It just works fine in Windows OS that use the Pdf Xchange Editor."

(sox-sequence sounds name))

;; ====================================================

(defmethod! ckn-add-extras ((voice voice))
:initvals '(nil)
:indoc '("Add some notehead in OM-Score") 
:icon '17359
:doc "It works just if you follow the MIDI-Channels of this Library. See the documentation of the Ircam-Instruments."

(ckn-add-extras voice))

; ===========================================================================

(om::defmethod! osc-play ((voice voice))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."

(let* (
        (ckn-action1 (remove nil (voice->coll voice 1))))
          (let* (
                (action1 
                  (progn (om::osc-send (om::x-append '/reset 1) "127.0.0.1" 3003)
                    (loop 
                        :for cknloop 
                        :in ckn-action1 
                                :collect (om::osc-send (om::x-append '/note cknloop) "127.0.0.1" 3003))))

                (action2 (om::osc-send (om::x-append '/note-pause 1) "127.0.0.1" 3003)))
      '("play"))))

; ===========================================================================

(om::defmethod! osc-play ((self chord-seq))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."


  (let* ( 
        (durations-of-the-chors (om::om- 0 (om::om/ (om::x->dx (lonset self)) 1000)))
        (loop-notes (loop :for x :in (lmidic self) 
                          :for y :in durations-of-the-chors 
                          :collect (if (not x) y (abs y))))
        (to-voice (make-instance 'voice :tree (mktree loop-notes '(4 4)) :lmidic (remove nil (lmidic self)))))
(osc-play to-voice)))
       


; ===========================================================================

(om::defmethod! osc-play ((chord chord))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."

(let* (
    (chord-seq (make-instance 'chord-seq :lmidic (list chord))))
    (osc-play chord-seq)))

;; ====================================================

(defmethod* f->n ((freq list))
  :numouts 1
  :initvals (list 6000 nil)
  :indoc '("pitch or pitch list (midicents)" "frequency (Hz)")
  :icon 'conversion
  :doc "
Converts a (list of) freq pitch(es) to names of notes.
"

(mc->n (f->mc freq) 4))

;; ====================================================

(defmethod* ms->samples ((sec number) (sample-rate number))
  :numouts 1
  :initvals (list 6000 nil)
  :indoc '("numbers of seconds" "samples rate of the audio")
  :icon '141
  :doc "
Converts a (list of) seconds to milisseconds.
"
(om::om-round (sec->samples (om::om/ sec 1000) sample-rate)))

;; ==================================================== FFT APPROACH LIKE SPEAR =====================================

(defmethod! fft->chord-seq  ((ckn-fft-instance list) (down number) (up number))
:initvals '(nil 3600 8400)
:indoc '("pitch or pitch list (midicents)" "frequency (Hz)")
:icon '17359
:doc "
Converts a (list of) freq pitch(es) to names of notes."


(let* (
    (fft->chords 
     (loop :for x 
           :in ckn-fft-instance 
           :collect (let* (
                        (amplitudes (amplitudes x))
                        (frequencias (frequencias x))
                        (freq-to-midicents (f->mc frequencias))
                        (lin->vel (om::om-scale amplitudes 20 127 0.0 1.0)))
                        (make-instance 'chord
                                          :lmidic (remove nil freq-to-midicents)
                                          :lvel (remove nil lin->vel)))))
    (filter (lambda (x) 
              (let* (
                     (matrix-transformation (mat-trans (list (lmidic x) (lvel x))))
                     (lambda-filter (loop :for y :in matrix-transformation 
                                          :collect (let* (
                                                          (notas (first y))
                                                          (dinamicas (second y))
                                                          (boolean-if (and (< notas up) (> notas down))))
                                                     (if boolean-if (x-append notas dinamicas) nil)))))
                     (let* (
                            (remove-nil (remove nil lambda-filter))
                            (second-matrix-transformation (mat-trans remove-nil)))
                       (make-instance 'chord :lmidic (first second-matrix-transformation) :lvel (second second-matrix-transformation))))))
  (make-chords (mapcar filter fft->chords)))
  (make-instance 'chord-seq :lmidic make-chords :lonset (list 0 (om::om-round (samples->ms (ckn-hop-size (first ckn-fft-instance)) (sound-sample-rate (first ckn-fft-instance))))))))


    

; ===========================================================================

(defmethod! fft->sin-model ((ckn-instances list) (db-filter number))
:initvals ' ((nil) -60)       
:indoc ' ("A list of ckn-fft-instance class." "Threshold in dB.")
:outdoc ' ("list of ckn-fft-instance with the approach of Spear software.")
:icon '17359
:doc ""

(fft->sin-model-fun ckn-instances db-filter))


; ===========================================================================

(defmethod! markers-cut ((sound sound))
:initvals ' ((nil) -60)       
:indoc ' ("A sound.")
:outdoc ' ("The new sound")
:icon '17359
:doc "It will cut one sound using the TWO FIRST markers."

(let* (
      (sound-markers (om::markers sound)))
      (om::sound-cut sound (first sound-markers) (second sound-markers))))

;; ORCHIDEA INSTRUMENTS ===============================

(defmethod! o-voice2samples ((voice voice) &optional (pan nil) (temp-files t))
:initvals ' ((nil) '-60)       
:indoc ' ("A list of ckn-fft-instance class." "Threshold in dB.")
:outdoc ' ("list of ckn-fft-instance with the approach of Spear software.")
:icon '17360
:doc ""

(om-print "Aguarde!" "Verbose")
;; (ckn-clear-temp-files)
(if (equal (o-check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")
(if (equal *app-name* "om-sharp")
    (o-voice->samples-sharp voice pan temp-files)
    )

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))


;; ====================================================
