(in-package :om)

;==================================== FUNCTIONS ===================

(defun list->string-fun (ckn-list)
  (when ckn-list
    (concatenate 'string 
                 (write-to-string (car ckn-list)) (list->string-fun (cdr ckn-list)))))

;====================================

(defun list-dimensions (list depth)
  (loop :repeat depth
        :collect (length list)
        :do (setf list (car list))))

;=====================================

(defun list-to-array-fun (list depth)
  (make-array (list-dimensions list depth)
              :initial-contents list))

;=====================================

(defun array-to-list-fun (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop :for j :below (nth n dimensions)
                     :do (setf (nth n indices) j)
                     :collect (if (= n depth)
                                (apply #'aref array indices)
                                (recurse (1+ n))))))
      (recurse 0))))

;=====================================

(defun by-N-fun (list n fun) 
  (loop for tail on list by (lambda (l) (nthcdr n l)) 
    :collect (funcall fun (subseq tail 0 (min (length tail) n)))))

;=====================================

(defun fft->phrase-fun (fft)
    (let* (   
        (fft-list (array-to-list-fun fft))
        (i-n (mapcar (lambda (x) (imagpart x)) fft-list))
        (r-n (mapcar (lambda (y) (realpart y)) fft-list)))
    (mapcar (lambda (x y) (atan x y)) i-n r-n)))

;=====================================

(defun fft->amplitude-fun (fft)
 (let* (   
       (fft-list (array-to-list-fun fft))
       (i-n (mapcar (lambda (x) (imagpart x)) fft-list)) ;; FILTRA A PARTE IMAGINÁRIA DO FFT
       (r-n (mapcar (lambda (y) (realpart y)) fft-list))) ;; FILTRA A PARTE REAL DO FFT
       
   (mapcar (lambda (x y) (sqrt (om::om+ (om::om^ x 2) (om::om^ y 2)))) i-n r-n)))

;======================================

(deftype complex-sample-array (&optional size)
  `(simple-array complex-sample (,size)))

;======================================

(defun energy (x)
  (declare (type complex-sample-array x)
           (optimize speed))
  (let ((acc 0d0))
    (declare (type double-float acc))
    (map nil (lambda (x)
               (let ((r (realpart x))
                     (i (imagpart x)))
                 (incf acc (+ (* r r) (* i i)))))
         x)
    acc))

;======================================

(defvar *optimization-policy* '(optimize speed (safety 0))) ;; Otimização de processamento

;=====================================

(defun real-samplify (vec &optional (size (length vec)))
  (etypecase vec
    (real-sample-array vec)
    ((simple-array single-float 1)
     (map-into (make-array size
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))
    (sequence
     (map-into (make-array size
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))))

;=====================================

(deftype real-sample-array (&optional size)
  `(simple-array real-sample (,size)))

; ============================

(deftype real-sample ()
  'double-float)

; ============================

(defun lb (n)
  (integer-length (1- n)))

; ============================

(defun get-function-discriminators (function-name)
  "Get the discriminators of a function and sort them appropriately to
be used for urlmapping."
  (let ((discriminators
	 (mapcar (fn (method)
		   (mapcar (compose #'^symbol #'class-name)
			   (method-specializers method)))
		 (generic-function-methods (symbol-function
					    function-name)))))
    (sort-function-discriminators discriminators)))

; ============================

(defun half-fun (in-array)
  (make-array (/ (length in-array) 2)
	      :element-type (array-element-type in-array)
	      :displaced-to in-array))

; ============================

(defun positive? (x) (equal (abs x) x))

; ============================

(defun sound-window (sound-bytes-window window hop-size windows-type &optional result)

(let* (
      (action1 (if (equal nil windows-type)

                (list-to-array (first-n sound-bytes-window window) 1) 
                #|
                ;; First-n seleciona os primeiros n bytes de todos os bytes do sample em análise. 
                ;; Sendo que n é o hop size.
                ;; O trecho acima transforma os bytes de um sample em arrays em list (list-to-array).
                |#
                (om-ckn::apply-window (list-to-array (first-n sound-bytes-window window) 1) windows-type))) 

      (action2 (last-n sound-bytes-window (let* ((number (- (length sound-bytes-window) hop-size)))
                                            (if (positive? number) number 1)))))
(if (< (length (remove nil action2)) window) 
    (reverse (x-append (list action1) result))
    (setf sound-bytes-window (sound-window action2 window hop-size windows-type (push action1 result))))))


; ============================

(defun sound-window-list (sound-bytes-window window hop-size &optional result)

(let* (
      (action1 (first-n sound-bytes-window window))
      (action2 (last-n sound-bytes-window (let* ((number (- (length sound-bytes-window) hop-size)))
                                            (if (positive? number) number 1)))))
(if (< (length (remove nil action2)) window) 
    (x-append result (list action1)) 
  (setf sound-bytes-window (sound-window-list action2 window hop-size (push action1 result))))))

; ============================

(defun loop-in-parts (sound-bytes-window window hop-size &optional result)

(let* (
      (action1 (first-n sound-bytes-window window))
      (action2 (let* ((number (- (length sound-bytes-window) hop-size)))
                      (if (positive? number)      
                        (last-n sound-bytes-window number)
                        sound-bytes-window))))
(if (< (length (remove nil action2)) window)
    (reverse (x-append (list action1) result))
  (setf sound-bytes-window (loop-in-parts action2 window hop-size (push action1 result))))))


 ; ============================ OM-SYNTH ========================================================

;; This is a code stolen from Jean Bresson OM-Sharp

 (defun do-senoide (dur freq gain envelope)

  (let* (
         (sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop :for f :in freqs :collect (/ f sr)))
         (sampled-envelope (om-scale (nth 2 (multiple-value-list (om-sample envelope nbsamples))) 0.0 1.0)))

   (loop :for x :from 0 :to (1- nbsamples)
         :for y-list := (make-list (length steps) :initial-element 0) :then (om+ y-list steps)
         :for amp :in sampled-envelope
         :collect
            (om::om* 
             (om::om* gain amp)
             (apply '+ (loop :for y :in y-list :collect (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor y)))))))))))

;=========================

(defun synth (dur freq gain envelope)

  (let* ((sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop for f in freqs collect (/ f sr)))
         (sampled-envelope (om-scale (nth 2 (multiple-value-list (om-sample envelope nbsamples))) 0.0 1.0)))

    (with-sound-output (mysound :nch 2 :size nbsamples :sr 44100 :type :float)

      (loop for x from 0 to (1- nbsamples)
            for y-list = (make-list (length steps) :initial-element 0) :then (om+ y-list steps)
            for amp in sampled-envelope
            do
            (write-in-sound mysound 1 x
                            (om* (om* gain amp)
                               (apply '+ (loop for y in y-list collect (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor y))))))

                                      ))
                            )
            )
      )))


;=========================

#| (lambda (sound-self fft-size hop-size)

  (let* (
        (start (if (equal (markers sound-self) nil) 
                   0 
                 (1+ (sec->samples (ms->sec (first (markers sound-self))) (sample-rate sound-self)))))
        (finish (if (< (length (markers sound-self)) 2)
                (1- (n-samples sound-self))
                (1+ (sec->samples (ms->sec (second (markers sound-self))) (sample-rate sound-self)))))
        (sound (sound->bytes sound-self))
        (sound-selection (let* ((action1 (first-n sound finish))
                                (action2 (- finish start)))
                           (last-n action1 action2)))
        (sound-windows (sound-window sound-selection fft-size hop-size))
        (sound-windows-parts (loop-in-parts sound-windows 128 128))
        (sound-windows-length (length sound-windows))
        (fft-chunk-to-ms (arithm-ser 1 sound-windows-length 1))
        (fft-chunk-to-ms-parts (loop-in-parts fft-chunk-to-ms 128 128))
        (boolean-window-size (if (om> sound-windows-length 129) 

)))))

|#

;=========================


(defun fft-multiple-thread (fft-chunks mail-box nomes chunks hop-size)

(let* ()

(loop 
    :for ckn-fft-chunks :in fft-chunks 
    :for create-mailbox :in mail-box
    :for names-process :in nomes
    :for chunks-number :in chunks
    :do 
        (mp:process-run-function names-process
                 () 
                  (lambda (x w z) (mp:mailbox-send w 
                      (let* (
                              (fft (half-fun (sapa-fft! x)))
                              (amp (fft->amplitude fft))
                              (phrase (fft->phrase fft)))
                             ;(length-amp (length amp))) ;; colocar a sequencia fft                                                               
                              (make-instance 'ckn-fft-instance 
                                 :ckn-fft (make-instance 'fft-complex-numbers :complex-numbers fft)
                                 :fft-window (* 2 (length amp))
                                 :fft-chunks z
                                 :ckn-tempo (om::sec->ms (om::samples->sec (om::om* hop-size z) 44100))
                                 :amplitudes amp
                                 :phrase phrase
                                 :frequencias nil))))
                   ckn-fft-chunks create-mailbox chunks-number))

(loop :with mailbox-empty = nil 
      :while (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mail-box)))
      :do 
            (let* () (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mail-box))
                  mailbox-empty))

(mapcar (lambda (x) (mp:mailbox-peek x)) mail-box)))


;========================= 

(defun do-fft-chunks (fft-chunks)

      (let* ()
            (om-print "Aguarde" "Verbose :: ")

  (loop :for chunks-number :in (arithm-ser 1 (length fft-chunks) 1)
        :collect (list->string-fun (list 'fft- chunks-number)))))

;================================================

(defun ckn-make-mail-box (names-of-all-process)

(loop :for name-process :in names-of-all-process
      :collect (mp:make-mailbox :lock-name name-process)))

;===============================================

(defun fft-ckn (sound-self fft-size hop-size windows-type)

  (let* (
        (start (if (equal (markers sound-self) nil)
                   0 
                 (1+ (om::sec->samples (om::ms->sec (first (markers sound-self))) (sample-rate sound-self)))))
        (finish (if (< (length (markers sound-self)) 2)
                (1- (om::n-samples sound-self))
                (1+ (sec->samples (ms->sec (second (markers sound-self))) (sample-rate sound-self)))))
        (sound (sound->bytes sound-self))
        (sound-selection (let* ((action1 (first-n sound finish))
                                (action2 (- finish start)))
                           (last-n action1 action2)))
        (zero-padding (x-append sound-selection 
                                (loop :for i :from 1 :to (om::om- (om* (ceiling (om/ (length sound-selection) fft-size)) fft-size) (length sound-selection))
                                      :collect (let* () 0))))
        (sound-windows (sound-window zero-padding fft-size hop-size windows-type))
        (sound-windows-parts (loop-in-parts sound-windows 128 128))
        (sound-windows-length (length sound-windows))
        (fft-chunk-to-ms (om::arithm-ser 1 sound-windows-length 1))
        (fft-chunk-to-ms-parts (loop-in-parts fft-chunk-to-ms 300 300))
        (boolean-window-size (om::om> sound-windows-length 300)))

(if boolean-window-size
    (flat (loop :for loop-sound-windows-parts :in sound-windows-parts 
          :for loop-fft-chunk-to-ms-parts :in fft-chunk-to-ms-parts
          :collect (let* (
                         (action1 (do-fft-chunks loop-sound-windows-parts))
                         (action2 (ckn-make-mail-box action1)))
                     (fft-multiple-thread loop-sound-windows-parts action2 action1 loop-fft-chunk-to-ms-parts hop-size))) 1)
  
  (let* (
                         
                         (action1 sound-windows)
                         (action2 fft-chunk-to-ms)                         
                         (action3 (do-fft-chunks sound-windows))
                         (action4 (ckn-make-mail-box action3)))
                     (fft-multiple-thread action1 action4 action3 action2 hop-size)))))

;=====================================================================
(defun fft-ckn-om (sound-self fft-size hop-size windows-type)
(om-print "Aguarde!" "OM-CKN - Verbose ::")
  (let* (
        (start (if (equal nil nil)
                   0 
                 (1+ (sec->samples (first (markers sound-self)) (sample-rate sound-self)))))
        (finish (if t   ; (< (length (markers sound-self)) 2)
                (1- (om-sound-n-samples sound-self))
                (1+ (sec->samples (second (markers sound-self)) (sample-rate sound-self)))))
        (sound (sound->bytes-om sound-self))
        (sound-selection (let* ((action1 (first-n sound finish))
                                (action2 (om::om- finish start)))
                           (last-n action1 action2)))
        (sound-windows (sound-window sound-selection fft-size hop-size windows-type))
        (sound-windows-parts (loop-in-parts sound-windows 128 128))
        (sound-windows-length (length sound-windows))
        (fft-chunk-to-ms (arithm-ser 1 sound-windows-length 1))
        (fft-chunk-to-ms-parts (loop-in-parts fft-chunk-to-ms 128 128))
        (boolean-window-size (om> sound-windows-length 129)))

(om-print "Conversao para Bytes concluida, aguarde o FFT." "OM-CKN - Verbose ::")

(if boolean-window-size
    (flat (loop :for loop-sound-windows-parts :in sound-windows-parts 
          :for loop-fft-chunk-to-ms-parts :in fft-chunk-to-ms-parts
          :collect (let* (
                         (action1 (do-fft-chunks loop-sound-windows-parts))
                         (action2 (ckn-make-mail-box action1)))
                     (fft-multiple-thread loop-sound-windows-parts action2 action1 loop-fft-chunk-to-ms-parts hop-size))) 1)
  
  (let* (
                         
                         (action1 sound-windows)
                         (action2 fft-chunk-to-ms)                         
                         (action3 (do-fft-chunks sound-windows))
                         (action4 (ckn-make-mail-box action3)))
                     (fft-multiple-thread action1 action4 action3 action2 hop-size)))))

;=====================================================================


(defun fft->Sin-model-fun (ckn-fft-instance filtro)

(loop 
        :for x :in ckn-fft-instance 
        :collect 
            (let* (
                  (FFT-SIZE (FFT-WINDOW x))
                  (TEMPO (ckn-tempo x))
                  (AMPLITUDES (amplitudes x))
                  (PHRASE (phrase x))
                  (CORRECTION-FOR-DB (case FFT-SIZE 
                                          (512 124.53343)
                                          (1024 250.19278749034922D0)
                                          (2048 501.4303903221932D0)
                                          (4096 1026.685)))
                  (MAG->DB 
                        (let* (
                                (action1 (om::om/ AMPLITUDES CORRECTION-FOR-DB))
                                (action2 (mapcar (lambda (x) (if (plusp x) (log x 10) -150.0)) action1)))
                          (om::om* 20 action2)))

                  (SPEAR-CORRECTION 
                        (spear-approach MAG->DB filtro FFT-SIZE PHRASE (sound-sample-rate x))))
                                                                        ;; COLOCAR SAMPLE-RATE NA CKN-FFT-INSTANCE
(make-instance 'ckn-fft-instance 
                :fft-window FFT-SIZE
                :ckn-tempo TEMPO 
                :frequencias (first (om::mat-trans SPEAR-CORRECTION))
                :amplitudes (second (om::mat-trans SPEAR-CORRECTION))))))


;;; ============== isso é o principal

(defun spear-approach (deb filtro fft-size phrase sample-rate)
  
(let* (
  (action1 
      (loop   
            :with condition-to-stop = nil
            :for loop-amplitudes :on deb
            :for loop-number-bin :in (om:arithm-ser 1 (length deb) 1)
                                      ;; (om:arithm-ser 0 (1- (length deb)) 1) 

                                      ;; Isso é um ótimo exemplo de erro comum, o código 
                                      ;; (om:arithm-ser 0 (1- (length deb)) 1) pode transformar o resultado final das 
                                      ;; frequencias em certa de 2 ou três Hertz.

            :for phrase-loop :in phrase
            :while (setf condition-to-stop (om::om< 3 (length loop-amplitudes)))
          :collect 
              (if  (let* (
                          (first-amp (first loop-amplitudes))
                          (second-amp (second loop-amplitudes))
                          (third-amp (third loop-amplitudes)))
                    
                    (and 
                          (om::om< first-amp second-amp)
                          (om::om> second-amp third-amp)
                          (om::om< filtro second-amp)))

                                      ;;; Este pedaço de código é responsável por criar o Local Maxima
                                      ;; É necessário cumprir 3 coisas
                                      ;; 1ª = A amplitude do bin (x-1) precisa ser menor que a amplitude de x 
                                      ;; 2ª = A amplitude do bin (x+1) precisa ser menor que a amplitude de x
                                      ;; 3ª = A amplitude de x precisa ser maior que a amplitude do filtro
                  (let* (
                    (Local-Maxima (list (1- loop-number-bin)  loop-number-bin (1+ loop-number-bin)))
                    (Local-Maxima-is-positive 
                              (if (equal (om::om-abs Local-Maxima) Local-Maxima)
                                  Local-Maxima 
                                  '(1 2 3)))
                    (amplitudes-de-Local-Maxima (om::posn-match deb Local-Maxima-is-positive))
                    (BIN-CORRECTION      
                                (if (equal 3 (length amplitudes-de-Local-Maxima))
                                      (let* (
                                            (a (first amplitudes-de-Local-Maxima))
                                            (b (second amplitudes-de-Local-Maxima))
                                            (c (third amplitudes-de-Local-Maxima)))
                                            (* 1/2 (/ (- a c) (+ (- a (* 2 b)) c))))))
                    (bin-para-frequencia 
                            (om::om-round (first (bin->freq (list (om::om+ loop-number-bin BIN-CORRECTION)) sample-rate fft-size)) 2))
                    (correcao_de_amplitude 
                                          (if (equal 3 (length amplitudes-de-Local-Maxima))
                                              (let* (
                                                    (a (first amplitudes-de-Local-Maxima))
                                                    (b (second amplitudes-de-Local-Maxima))
                                                    (c (third amplitudes-de-Local-Maxima)))
                                                (- b (* 1/4 (- a c) BIN-CORRECTION))))))
                    (list bin-para-frequencia (om::db->lin correcao_de_amplitude) phrase-loop))))))

    (remove nil action1)))





;===================================================================== SOUNDS WITH OPENMUSIC =====================================

(defun sound-mix-loop (sounds  &optional result)

(let*  (
    (action1 (sound-mix (first sounds) (second sounds)))
    (action2 (if (<= (length sounds) 2)
            (last-n action1 (- (length sounds) 2))
               action1)))
    (if (< (length (list action2)) 2 )
      (x-append result action2)
      (setf sounds (sound-mix-loop action2 (push action1 result))))))

;=====================================================================

(defun sound-sequence-loop (sounds &optional result)

(let*  (
    (action1 (sound-seq (first sounds) (second sounds)))
    (action2 (if (<= (length sounds) 2)
            (last-n sounds (- (length sounds) 2))
               action1)))

    (if (< (length (list action2)) 2 )
      (x-append result action2)
      (setf sounds (sound-sequence-loop action2 (push action1 result))))))


;=====================================================================

(defun ckn-clear-temp-files ()

(let* ()
(om::om-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))
(om::om-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.wav" (outfile ""))))))))))))

;=====================================================================

(defun ckn-list-to-string (lst)
    (format nil "~{~A ~}" lst))

;=====================================================================


(defun loop-until-probe-file (my-file)
        (loop :with file = nil 
              :while (equal nil (setf file (probe-file my-file)))
        :collect file)

(probe-file my-file))

;=====================================================================

(defun ckn-antescofo-score (voice variance local)
(let* (
    (voice-tempo (tempo voice))
    (voice-tree (om-abs (ms->sec (om6-true-durations (make-value 'voice (list (list :tree (mktree (tree2ratio (tree voice)) '(4 4))) (list :tempo voice-tempo)))))))
    (voice-midi (lmidic voice))
    (voice-to-rest (choose-to-rest voice))
    (SCORE
            (loop :for voice-tree-loop :in voice-tree 
            :for rest-loop :in voice-to-rest
            :collect
                    (x-append (if (equal (length (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))) 1) 
                    "NOTE"
                    "CHORD")

                        (if (equal (length (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))) 1)
                            (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))
                            (list (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))))

                        voice-tree-loop
                        (if (equal (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop)) (list 0))
                            nil
                            "@pizz"))))

    (Score-acabada (x-append (list (x-append "BPM" voice-tempo) (x-append "Variance" variance)) SCORE)))

(save-as-text Score-acabada local)))



; ========================================== OSC-PLAY =======================

(defun chord->voice (lista-de-notas)

(mktree (loop :for i :from 1 to (length lista-de-notas) :collect (let* () 1/4)) (list 4 4)))

;; ==================

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

;; ======================================================================

(defun voice->coll (ckn number-2)

(let* (
  (ckn-action1  (loop 
                      :for ckn-plus :in (true-durations ckn) 
                      :collect (if (plusp ckn-plus) 0 1)))
  (ckn-action2  (loop 
                      :for cknloop :in ckn-action1 
                      :collect (if 
                                    (om::om= 0 cknloop)
                                    (setq number-2 (om::om+ number-2 1)))))

  (ckn-action3 
            (let* ((ckn-action3-1 
                          (if 
                              (equal nil (first ckn-action2))
                              0 
                              (first ckn-action2))))
                                  
                                  
                  (if 
                              (equal nil (first ckn-action2)) 
                              (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) 
                              (om::om+ (om::om- ckn-action2 ckn-action3-1) 1)))))

(loop 
      :for cknloop-1 :in ckn-action3 
      :for cknloop-2 :in (om::dx->x 0 (loop :for y :in (true-durations ckn) :collect (abs y))) 
      :for cknloop-3 :in (true-durations ckn) 
      :collect          
      
      (if (plusp cknloop-3) 
            (om::x-append 
               (if (plusp cknloop-3) cknloop-2 nil)
                  (om::x-append  
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "LMIDIC") cknloop-1) 
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "lvel") cknloop-1)
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "lchan") cknloop-1)
                    (if (plusp cknloop-3) cknloop-3 nil) 
                       ))))))

;; ======================================================================

(defun choose-ratio-of-note (voice note-number)
  (let* (
        (action1 (remove nil (mapcar (lambda (x) (if (plusp x) x nil)) (tree2ratio (tree voice)))))
        (action2 (mktree (list (choose action1 note-number)) (list 4 4)))
        (action3 (make-instance 'voice :tree action2 :tempo (tempo voice))))
    (car (true-durations action3))))


;===================================================================== Compile in OM-SHARP =================================

(compile 'loop-in-parts)
(compile 'sound-window-list)
(compile 'sound-window)
(compile 'positive?)
(compile 'real-samplify)
(compile 'energy)
(compile 'fft->amplitude-fun)
(compile 'fft->phrase-fun)
(compile 'by-N-fun)
(compile 'array-to-list-fun)
(compile 'list-to-array-fun)
(compile 'list-dimensions)
(compile 'list->string-fun)
(compile 'fft-multiple-thread)
(compile 'do-fft-chunks)
(compile 'ckn-make-mail-box)
(compile 'fft-ckn)
(compile 'sound-mix-loop)
(compile 'sound-sequence-loop)
(compile 'ckn-clear-temp-files)
(compile 'spear-approach )
(compile 'fft->sin-model-fun)
(compile 'do-senoide)
(compile 'sound->bytes-om)



    
    









