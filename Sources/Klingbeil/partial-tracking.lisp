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

(defun Klingbeil-partial-tracking (all-frames &optional (partial-tracking nil))
    (let  (
        (freq_threshold 10)
        (frame-track nil)
        (frameK_prev    (first all-frames))
        (frameK      (second all-frames))
        (frameK_next    (third all-frames))
        (track  (loop   :for prevpeak :in frameK_prev
                        :do (loop  :for curpeak :in frameK
                                    :do 
                                        (let*   (
                                                (distance (- (p-freq prevpeak) (p-freq curpeak))))
                                                (if (< distance freq_threshold)
                                                    (let*   (
                                                            (existing_distante  (if (not    (null (p-backmatch curpeak)))
                                                                                            (- (p-freq (p-backmatch curpeak)) (p-freq curpeak))
                                                                                            freq_threshold)))
                                                            (if (< distance existing_distante)
                                                                (progn
                                                                    ;; curpeak.Backmatch.Forwardmatch = null
                                                                    ;; curpeak.Backmatch ← prevpeak
                                                                    ;; prevpeak.Forwardmatch ← curpeak
                                                                    (setf (p-forwardmatch (p-backmatch curpeak)) nil)
                                                                    (setf (p-backmatch curpeak) prevpeak)
                                                                    (setf (p-forwardmatch prevpeak) curpeak))))))
                                :collect curpeak)
                        :collect prevpeak)))
        (print "Ok")
        (if (null frameK_next)
            (append partial-tracking (list track))
            (Klingbeil-partial-tracking (cdr all-frames) (append partial-tracking (list track))))))

        
                                 
                                                                
                                                                
                                                        
                                                    





