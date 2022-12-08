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

;; (defun Klingbeil-partial-tracking (all-frames &key (treshold_distance 30))
;;     (loop :for i :from 0 :to (length all-frames) :by 1 :do
;;         (let (
;;             (frame (nth i all-frames))
;;             (next-frame (nth (1+ i) all-frames)))
;;             (loop :for prevpeak :in frame :do
;;                     (loop :for curpeak :in next-frame :do
;;                         (let (
;;                             (distante (abs (- (p-freq prevpeak) (p-freq curpeak)))))
;;                             (if (< distante treshold_distance)
;;                                 (let (
;;                                     (existing-distance (if (p-backmatch curpeak)
;;                                                         (abs (- (p-freq (p-backmatch curpeak)) (p-freq curpeak)))
;;                                                         treshold_distance)))
;;                                     (if (< distante existing-distance)
;;                                         (progn
;;                                             (if (p-backmatch curpeak)
;;                                                 (setf (p-forwardmatch (p-backmatch curpeak)) nil))
;;                                             (setf (p-backmatch curpeak) prevpeak)
;;                                             (setf (p-forwardmatch prevpeak) curpeak)))))))))
;;     :finally (return all-frames)))
                                
; ===============

(defmethod! klingbeil-partial-tracking ((all-frames list) &key (treshold_distance 30))
(let* (
    (stft-frames (copy-list all-frames))
    (index 0)
    (the_time 0)
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
            :do (setf the_time (+ the_time 12))
            :collect (let* (
                            (index (mapcar #'p-index frame))
                            (frequencies (mapcar #'p-freq frame))
                            (amplitudes (mapcar #'p-amp frame))
                            (phases (mapcar #'p-phase frame))
                            (data (list index frequencies amplitudes phases))
                            (frame-matrix (make-instance 'sdifmatrix :matrixtype "1TRC" :data data)))
                            (make-instance 'sdifframe :ftime (ms->sec the_time) :frametype "1TRC" :lmatrix frame-matrix)))))
    sdif-frames))
    
    
    
    
    
    ;; (sdif-frames 
    ;;     (loop :for frame :in partial-tracking 
    ;;         :collect (let* (
    ;;                         (index (mapcar #'p-index frame))
    ;;                         (frequencies (mapcar #'p-freq frame))
    ;;                         (amplitudes (mapcar #'p-amp frame))
    ;;                         (phases (mapcar #'p-phase frame))
    ;;                         (data (list index frequencies amplitudes phases))
    ;;                         (frame-matrix (make-instance 'sdifmatrix :matrixtype "1TRC" :data data)))
    ;;                         (make-instance 'sdifframe :frametype "1TRC" :lmatrix frame-matrix)))))
    ;; (write-sdif-file sdif-frames :types (list (make-instance 'sdiftype :struct 'm :signature "1TRC" :description nil)))))


                    



    
                        
            
    
                            
                                                            
            

        
                                 
                                                                
                                                                
                                                        
                                                    





