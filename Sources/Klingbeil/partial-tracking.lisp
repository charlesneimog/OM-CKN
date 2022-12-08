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

;; for frame in range(len(local_maxima)):       
;;             if local_maxima[frame + 1] == []:
;;                 break
;;             else:
;;                 for prevpeak in local_maxima[frame]:
;;                     for curpeak in local_maxima[frame+1]:
;;                         distante = abs(prevpeak.freq - curpeak.freq)
;;                         
;;                         if distante < self.treshold_distance:
;;                             




;;                              if curpeak.Backwardmatch != None:
;;                                 existing_distance = curpeak.Backwardmatch.freq - curpeak.freq
;;                             else:
;;                                 existing_distance = self.treshold_distance 
                                
;;                             if distante < existing_distance:
;;                                 if curpeak.Backwardmatch != None:
;;                                     curpeak.Backwardmatch.Forwardmatch = None
;;                                 curpeak.Backwardmatch = prevpeak # This is the new match
;;                                 prevpeak.Forwardmatch = curpeak # This is the new match
                
;;         return local_maxima



(defun Klingbeil-partial-tracking (all-frames &key (treshold_distance 30))
    (loop :for i :from 0 :to (length all-frames) :by 1 :do
        (let (
            (frame (nth i all-frames))
            (next-frame (nth (1+ i) all-frames)))
            (loop :for prevpeak :in frame :do
                    (loop :for curpeak :in next-frame :do
                        (let (
                            (distante (abs (- (p-freq prevpeak) (p-freq curpeak)))))
                            (if (< distante treshold_distance)
                                (let (
                                    (existing-distance (if (p-backmatch curpeak)
                                                        (- (p-freq (p-backmatch curpeak)) (p-freq curpeak))
                                                        treshold_distance)))
                                    (if (< distante existing-distance)
                                        (progn
                                            (if (p-backmatch curpeak)
                                                (setf (p-forwardmatch (p-backmatch curpeak)) nil))
                                            (setf (p-backmatch curpeak) prevpeak)
                                            (setf (p-forwardmatch prevpeak) curpeak)))))))))
    :finally (return all-frames)))
                                

                            
                                                            
            

        
                                 
                                                                
                                                                
                                                        
                                                    





