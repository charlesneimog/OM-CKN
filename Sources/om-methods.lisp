
(in-package :om)


;================================================== FFT-Class =====================

(defclass! ckn-fft-instance ()
( 
  (ckn-fft :initform '#(-6.1035157E-5 0.0) :initarg :ckn-fft :accessor ckn-fft)
  (fft-window :initform '1024 :initarg :fft-window :accessor fft-window)
  (fft-chunks :initform '("fft-1" "fft-2" "fft-3") :initarg :fft-chunks :accessor fft-chunks)
  (ckn-tempo :initform '(1) :initarg :ckn-tempo :accessor ckn-tempo)
  (amplitudes :initform '(0.3 0.3 0.3) :initarg :amplitudes :accessor amplitudes)
  (phrase :initform '(0.1 0.3 0.1) :initarg :phrase :accessor phrase)
  (frequencias :initform '(220 440 880) :initarg :frequencias :accessor frequencias)
                                    )
  (:icon 17359))

;; asdf

;==================================================================================
(defclass! fft-complex-numbers ()
( 
  (complex-numbers :initform '#(-6.1035157E-5 0.0) :initarg :complex-numbers :accessor complex-numbers)
                                    ))
                                    
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

;====================================

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
  (sound->bytes-fun self)
  (sound->bytes-om self)))

;=====================================

(defun sound->bytes-fun (self)
(let* ((verbose (print (format nil "Read bytes of the sound."))))
(with-audio-buffer (b self)
          (let ((channel-ptr (om-read-ptr (om-sound-buffer-ptr b) (1- (n-channels self)) :pointer)))
          (loop :for i :from 0 :to (n-samples self) :by 1 :collect (om-read-ptr channel-ptr i :float))))))


(defun sound->bytes-om (self)

  (let* ((pontos
          (audio-io::om-get-sound-buffer (filename self) :float t)))
(loop :for i :from 0 :to (om-sound-n-samples self) :by 1 :collect (om-read-ptr pontos i :float))))


;=====================================

(defmethod! cartopol ((fft cons))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:numouts 2
:outdoc ' ("phrase" "amplitude")
:icon '17359
:doc ""

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

    (loop :for cknloop :in (arithm-ser 1 (length action2) 1) :collect       
        (x-append  
          (get-slot-val (make-value-from-model 'sdifframe 
              (posn-match action2 (om::om- cknloop 1)) nil) "FTIME")  
          (mat-trans 
            (posn-match 
              (get-slot-val 
                  (make-value-from-model 'sdifmatrix 
                            (first (get-slot-val (make-value-from-model 'sdifframe (posn-match action2 (om- cknloop 1)) nil)
                            "LMATRIX")) nil) "DATA") '(0 1 2))))
        )
    )
)

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
(ckn-action1  (loop :for ckn-plus :in (true-durations voice) :collect (if (plusp ckn-plus) 0 1)))

(ckn-action2 (loop :for cknloop :in ckn-action1 :collect (if (= 0 cknloop) (setq number-2 (+ number-2 1)) nil))))


(let* (
     (ckn-action3-1 
     (if (equal nil (first ckn-action2)) 0 (first ckn-action2))))
     (if (equal nil (first ckn-action2)) (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) (om::om+ (om::om- ckn-action2 ckn-action3-1) 1))          
      )))

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

(defmethod! om6-true-durations ((voice voice))
:initvals '(nil)
:indoc '("a voice" ) 
:icon 'tree
:doc "Imported from OM6."

(true-durations voice))

;; ====================================================

(defmethod! sound-seq-multi ((sounds list) &optional (list-per-threading 30))
:initvals '(nil)
:indoc '("a list of sounds." "Among of sounds per threading.")
:icon '17359
:doc "Like sound-seq-list but multithreading (more fast)."

(let* (
(action1 (sound-seq-list-multi-threading (build-seq-of-sounds sounds list-per-threading))))
(om::om-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))
(gc-all)
action1))

;; ====================================================

(defmethod! voice->samples-sound ((voice voice) &optional (pan nil) (temp-files t))
:initvals '(nil nil t)
:indoc '("a voice" "panoramic information - see the object sound-stereo-pan" "Clear temp files") 
:icon '17359
:doc "Imported from OM6. It can take."

(om-print "Aguarde!" "Verbose")

(ckn-clear-temp-files)

(if (equal (check-samples-in-voice voice)  "Todas as alturas possuem samples correspondentes")

(voice->samples-sound-fun voice pan temp-files)

(let* ((action1 (print "I am not able to finding all the samples")))
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

;; ====================================================
(compile 'sound-seq-multi)