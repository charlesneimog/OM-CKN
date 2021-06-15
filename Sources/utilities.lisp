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


 (defun do-senoide (dur freq gain envelope)

  (let* ((sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop for f in freqs collect (/ f sr)))
         (sampled-envelope (om-scale (nth 2 (multiple-value-list (om-sample envelope nbsamples))) 0.0 1.0)))

      (loop for x from 0 to (1- nbsamples)
            for y-list = (make-list (length steps) :initial-element 0) :then (om+ y-list steps)
            for amp in sampled-envelope
            :collect
            (om* (om* gain amp)
                               (apply '+ (loop for y in y-list collect (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor y))))))

                                      ))
                            )
            )
      )

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

(print (format nil "São ~a janelas" (length fft-chunks)))

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
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.wav" (outfile ""))))))))))
))

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

                        "@hook"
                        (if (equal (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop)) (list 0))
                            nil
                            "@pizz"))))

    (Score-acabada (x-append (list (x-append "BPM" voice-tempo) (x-append "Variance" variance)) SCORE)))

(save-as-text Score-acabada local)))

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



    
    









