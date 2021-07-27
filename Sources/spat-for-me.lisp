(defclass! spat-for-me ()
( 
  (numcols :initform 2 :initarg :numcols :accessor numcols)
  (lcontrols :initform nil :initarg :lcontrols :accessor lcontrols)
  (ckn-sounds :initform nil :initarg :ckn-sounds :accessor ckn-sounds)
  (src-names :initform nil :initarg :src-names :accessor src-names)
  (trajectories :initform nil :initarg :trajectories :accessor trajectories)
  (durations :initform nil :initarg :durations :accessor durations)
  (onsets :initform nil :initarg :onsets :accessor onsets)
  (orientations :initform nil :initarg :orientations :accessor orientations)
  (apertures :initform nil :initarg :apertures :accessor apertures)
  (presence :initform 90 :initarg :presence :accessor presence)
  (warmth :initform 30 :initarg :warmth :accessor warmth)
  (brillance :initform 30 :initarg :brillance :accessor brillance)
  (room-presence :initform 48 :initarg :room-presence :accessor room-presence)
  (running-reverberance :initform 34 :initarg :running-reverberance :accessor running-reverberance)
  (envelopment :initform 24 :initarg :envelopment :accessor envelopment)
  (omni-filter :initform '(0 1.7 0 -3.8 177 5657) :initarg :omni-filter :accessor omni-filter)
  (axis-filter :initform '(0 0 0 0 177 5657) :initarg :axis-filter :accessor axis-filter)
  (ckn-room :initform 1 :initarg :ckn-room :accessor ckn-room)))


(defun integer-to-string (int)
   (format nil "~D" int))

(lambda (self out stream-mode export-sounds rooms)


   (let* ((error nil) time
          (filepath (or (and out (handle-new-file-exists out))
                        (om-choose-new-file-dialog :types (list (format nil (om-str :file-format) "SDIF") "*.sdif" ))))
          (columns (lcontrols self))
          (nstreams (length columns))
          (sounds (ckn-sounds self))
          (names (scr-names self))
          (trajects (trajectories self))
          (durs (durations self))
          (onsets (onsets self))
          (orients (orientations self))
          (aperts (apertures self))
          (sndtable nil) (nametable nil) (trajdurs nil)
          (spatframes nil) (orientationframes nil) (apertureframes nil)
          (omnifilterframes nil) (axisfilterframes nil)
          (roomframes nil)
          (mode (or 'sep 'sep)))
     (when filepath
       
       ;;; SOURCES
       (unless sounds
         (setf sounds (make-list (numcols self))))

       (setf sndtable (loop for s in sounds 
                            for i = 1 then (+ i 1) 
                            collect (let ((soundpath (cond ((and (typep s 'sound) (om-sound-file-name s))
                                                            (pathname (om-sound-file-name s)))
                                                           ((or (pathnamep s) (stringp s))
                                                            (pathname s))
                                                           (t nil))))
                                      (when (and soundpath export-sounds)
                                        (let ((tempfile (unique-pathname filepath (pathname-name soundpath) (pathname-type soundpath))))
                                          (om-copy-file soundpath tempfile)
                                          (when (equal export-sounds :temp)
                                            (add-tmp-file tempfile))
                                          ))
                                      (cons (integer-to-string i)
                                            (list (if soundpath (string+ (pathname-name soundpath) "." (pathname-type soundpath))
                                                    "unknown-source")))
                                      )))
       
       ;;; SOURCE NAMES
       (setf nametable (loop for name in names 
                             for i = 1 then (+ i 1)
                             when name
                             collect (list (integer-to-string i) 
                                           (if (stringp name) name (format nil "~A" name)))))

       ;;; TRAJECTORIES
       (if trajects
           (setf trajects (mapcar #'(lambda (obj) 
                                      (when obj
                                        (objfromobjs obj (make-instance '3D-trajectory))))
                                  trajects))
         (setf trajects (make-list (numcols self))))
     
       (setf trajdurs (loop for i from 0 to (1- (numcols self)) collect 
                            (or (and durs (nth i durs))
                                (if (and (nth i trajects) (find-if 'numberp (times (nth i trajects)) :from-end t))
                                    nil
                                  (and sounds (nth i sounds) (sound-dur (nth i sounds)))))))
     
       ;;; scale
       
       (setf trajects 
             (mapcar #'(lambda (traj d)
                         (let ((tr (get-full-trajectory traj)))
                           (when d (setf (times tr) (om+ (car (times tr)) (om-scale (times tr) 0 d))))
                           tr)
                         ) trajects trajdurs))
         
       (when onsets 
         (mapcar #'(lambda (traj o) 
                     (when traj (setf (times traj) (om+ (times traj) o))))
                 trajects onsets))
       
       ;;; ORIENTATION
       (when orients
         (setf orientationframes (get-multi-value-frames orients onsets trajdurs "XORI")))
       
       ;;; APERTURES
       (when aperts
         (setf apertureframes (get-simple-value-frames aperts onsets trajdurs "XAPE")))
     
       ;;; FILTERS (FOR REVERB AND PERCEPTUAL EFFECTS)
       (when (omni-filter self)
         (setf omnifilterframes (get-multi-value-frames (omni-filter self) onsets trajdurs "OMNI")))
       (when (axis-filter self)
         (setf axisfilterframes (get-multi-value-frames (axis-filter self) onsets trajdurs "AXIS")))


       ;;; PERCEPTUAL PARAMS
       (setf pereptualframes (remove nil (append
                                          (get-simple-value-frames (presence self) onsets trajdurs "PRES")
                                          (get-simple-value-frames (warmth self) onsets trajdurs "WARM")
                                          (get-simple-value-frames (brillance self) onsets trajdurs "BRIL")
                                          (get-simple-value-frames (room-presence self) onsets trajdurs "PRER")
                                          (get-simple-value-frames (running-reverberance self) onsets trajdurs "REVP")
                                          (get-simple-value-frames (envelopment self) onsets trajdurs "ENVP")
                                          )))
       
       (setf ridframes (get-simple-value-frames (ckn-room self) onsets trajdurs "XRID"))
       
       
       (when rooms
         (setf roomframes (sort (flat (mapcar 'gen-room-frames (list! rooms))) '< :key 'ftime)))
         
       (multiple-value-bind (spatframes xmin xmax ymin ymax zmin zmax) 
           (gen-trajectories-frames trajects)
         
         ;;; MERGE MODE
         (when (equal 'merge mode)
           (setf spatframes (merge-frame-data spatframes))
           (setf orientationframes (merge-frame-data orientationframes))
           (setf apertureframes (merge-frame-data apertureframes))
           (setf pereptualframes (merge-frame-data pereptualframes))
           (setf omnifilterframes (merge-frame-data omnifilterframes))
           (setf axisfilterframes (merge-frame-data axisfilterframes))
           (setf ridframes (merge-frame-data ridframes))

           (setf roomframes (merge-frame-data roomframes))
           )
     
         ;;; WRITE SDIF
         (let* ((outfile (sdif::sdif-open-file (namestring filepath) :eWriteFile))
                (datatype 4)
                (sdifvalues (om-make-pointer (* 3 datatype (if (equal 'merge mode) (numcols self) 1)))))
           (sdif::SdifFWriteGeneralHeader outfile)
           (write-nvt-tables outfile (remove nil 
                                             (list (default-om-NVT)
                                                   (make-instance 'SDIFNVT 
                                                                  :tablename "Sources"
                                                                  :ID 0
                                                                  :NV-pairs sndtable)
                                                   (make-instance 'SDIFNVT 
                                                                  :tablename "Dimensions"
                                                                  :ID 0
                                                                  :NV-pairs (list (list "Xmin" (format nil "~D" xmin))
                                                                                  (list "Xmax" (format nil "~D" xmax))
                                                                                  (list "Ymin" (format nil "~D" ymin))
                                                                                  (list "Ymax" (format nil "~D" ymax))
                                                                                  (list "Zmin" (format nil "~D" zmin))
                                                                                  (list "Zmax" (format nil "~D" zmax))
                                                                                  ))
                                                   (when t ; nametable 
                                                     (make-instance 'SDIFNVT 
                                                                    :tablename "SourceNames"
                                                                    :ID 0
                                                                    :NV-pairs nametable))
                                                   )))   
           (when *spat-sdiftypes*
             (write-sdif-types outfile *spat-sdiftypes*))
           (sdif::SdifFWriteAllASCIIChunks outfile)
     
           (loop for frame in (merge-frames (append spatframes orientationframes apertureframes pereptualframes omnifilterframes axisfilterframes ridframes roomframes)) do 
                 (save-sdif frame outfile))

           (sdif::sdif-close-file outfile)
           (om-free-pointer sdifvalues)
           )
         (probe-file (om-namestring filepath))
         )))