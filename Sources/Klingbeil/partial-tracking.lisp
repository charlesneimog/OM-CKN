; crie uma classe com Freq, Amp, Phase, Forwardmatch, Backmatch

(defclass peak ()
(
    (p-index :initarg :p-index :initform nil :accessor p-index)
    (p-freq :initarg :p-freq :initform nil :accessor p-freq)
    (p-amp :initarg :p-amp :initform nil :accessor p-amp)
    (p-phase :initarg :p-phase :initform nil :accessor p-phase)
    (p-forwardmatch :initarg :p-forwardmatch :initform nil :accessor p-forwardmatch) ; Save a class peak of the next frame
    (p-backmatch :initarg :p-backmatch :initform nil :accessor p-backmatch))) ; Save a class peak of the previous frame

; ===============
(defun klingbeil-eq-2.21 (a b c)
    (* 1/2 (/ (- a c) (+ (- a (* 2 b)) c))))

(compile 'klingbeil-eq-2.21)
; ===============

(defun stft->frames-list (fft-instances db-treshold)
    (let*   (
            ;; create a variable to store all peaks (Not possible to know the size of the list)
            (all-peaks nil)
            (stft-root (/ (sound-sample-rate fft-instances) (fft-size fft-instances)))
            (amplitudes-normalization (om::om/ (get-amplitude fft-instances) (fft-size fft-instances)))
            (phases (make-list (/ (length amplitudes-normalization) 2) :initial-element 0)))
    ;; (print (length amplitudes-normalization))
    (loop   :for index :from 0 :to (length amplitudes-normalization) :by 1
            :while (nth (+ index 2) amplitudes-normalization)
            :do (let*    (
                        (amplitude-bin-1 (nth index amplitudes-normalization))
                        (amplitude-bin-2 (nth (1+ index) amplitudes-normalization))
                        (amplitude-bin-3 (nth (+ index 2) amplitudes-normalization)))
                        (if (and (< amplitude-bin-1 amplitude-bin-2) (> amplitude-bin-2 amplitude-bin-3) (> (om::lin->db amplitude-bin-2) db-treshold))
                            (let*   (
                                    (parabola-freq-formula (klingbeil-eq-2.21 amplitude-bin-1 amplitude-bin-2 amplitude-bin-3))
                                    (freq (+ (* stft-root (+ (1+ index) parabola-freq-formula))))
                                    (peak (make-instance 'peak :p-freq (coerce freq 'double-float) :p-amp (coerce amplitude-bin-2 'double-float) :p-phase (coerce (nth index phases) 'double-float))))
                                    (setf all-peaks (append all-peaks (list peak))))))

            ;; finally remove nils
            :finally (return (remove nil all-peaks)))))

(compile 'stft->frames-list)                                           
; ===============

(defmethod! klingbeil-partial-tracking ((fft-instances list) &key (db-treshold -60) (treshold_distance 30))
(let* (
    (all-frames (mapcar #'(lambda (x) (stft->frames-list x db-treshold)) fft-instances))
    (hop-size-tempo (coerce (samples->sec (hop-size (car fft-instances)) (sound-sample-rate (car fft-instances))) 'double-float))
    (stft-frames (copy-list all-frames))
    (index (coerce 0 'double-float))
    (the_time (coerce 0.0001 'double-float))
    (partial-tracking 
        (loop :for i :from 0 :to (length stft-frames) :by 1 :do
                (let 
                    (
                    (prevframe (nth i stft-frames))
                    (frame (nth (1+ i) stft-frames)))
                    (loop :for prevpeak :in prevframe
                            :do
                                (loop :for curpeak :in frame :do
                                    (let (
                                        (distante (abs (- (p-freq prevpeak) (p-freq curpeak)))))
                                        (if (< distante treshold_distance)
                                            (let (
                                                (existing-distance (if (p-backmatch curpeak)
                                                                    (abs (- (p-freq (p-backmatch curpeak)) (p-freq curpeak)))
                                                                    treshold_distance)))
                                                (if (< distante existing-distance)
                                                    (progn
                                                        (if (p-backmatch curpeak)
                                                            (setf (p-forwardmatch (p-backmatch curpeak)) nil))
                                                        (setf (p-backmatch curpeak) prevpeak)
                                                        (setf (p-forwardmatch prevpeak) curpeak)))))))
                            :do (if (p-backmatch prevpeak) ; se ha um peak anterior
                                    (setf (p-index prevpeak) (p-index (p-backmatch prevpeak)))
                                    (progn 
                                        (incf index) ;; incrementa o index
                                        (setf (p-index prevpeak) index)))))
        :finally (return stft-frames)))
    (sdif-frames 
        (loop :for frame :in partial-tracking 
            
            :collect (let* (
                            (index (mapcar #'p-index frame))
                            (frequencies (mapcar #'p-freq frame))
                            (amplitudes (mapcar #'p-amp frame))
                            (phases (mapcar #'p-phase frame))
                            (data (list index frequencies amplitudes phases))
                            (silence (list (list (coerce 0 'double-float)) (list (coerce 0 'double-float)) (list (coerce 0 'double-float)) (list (coerce 0 'double-float)))))
                            (if (not (null (remove nil data)))
                                (make-instance 'sdifframe :ftime the_time :frametype "RBEP" :lmatrix (list (make-instance 'sdifmatrix :matrixtype "RBEP" :data data)))
                                (make-instance 'sdifframe :ftime the_time :frametype "RBEP" :lmatrix (list (make-instance 'sdifmatrix :matrixtype "RBEP" :data silence)))))
            :do (incf the_time hop-size-tempo))))
                            
                                
    
    
    sdif-frames))
    ;; (write-sdif-file sdif-frames :types (list (make-instance 'sdiftype :struct 'm :signature "1TRC" :description nil)))))

(compile 'klingbeil-partial-tracking)
                    



    
                        
            
    
                            
                                                            
            

        
                                 
                                                                
                                                                
                                                        
                                                    





