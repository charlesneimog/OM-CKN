(in-package :om)


;; ================ definir Classe

(defclass! spat-matrix (class-array) 
  ((sounds :initarg :sounds :initform nil)
   (src-names :initarg :src-names :initform nil)
   (trajectories :initarg :trajectories :initform nil)
   (durations :initarg :durations :initform nil)
   (onsets :initarg :onsets :initform nil)
   (orientations :initarg :orientations :initform nil)
   (apertures :initarg :apertures :initform nil)
   (presence :initarg :presence :initform 90)
   (warmth :initarg :warmth :initform 30)
   (brillance :initarg :brillance :initform 30)
   (room-presence :initarg :room-presence :initform 48)
   (running-reverberance :initarg :running-reverberance :initform 34)
   (envelopment :initarg :envelopment :initform 24)
   (omni-filter :initarg :omni-filter :initform '((0 1.7 0 -3.8 177 5657)))
   (axis-filter :initarg :axis-filter :initform '((0 0 0 0 177 5657)))
   (room :initarg :room :initform 1)
   ))


;; ============================================================================= Salvar SDIF ==============
(defmethod! ckn-save-spat-sdif ((self class-array) &key out stream-mode export-sounds rooms)
   :icon '(638)
   :menuins '((2 (("separate streams" sep) ("merge streams" merge))))
   :indoc '("a SPAT-MATRIX" "output SDIF file name" "export stream format" "sources export mode" "room(s) descriptions")
   :outdoc '("sdif file pathname")
   :initvals '(nil nil sep nil)
   :doc "Saves a SPAT-MATRIX into an SDIF File.

- If <out> is not specified, a file chooser opens to choose the output file.
- <stream mode> determines if source trajectories are stored in separate SDIF streams ('sep) or in a single stream ('merge)
- If <export-sounds> is T, the source files (if provided in the SPAT-MATRIX) are copied and exported to the SDIF file location.
  If <export-sound> is :temp, they are exported and registered as 'temporary files' to delete after synthesis.
- <room> is an object (or a list of objects) of type SPAT-ROOM, determining reverbs and room parameters used in the spat process. 
"
   (let* ((error nil) time
          (filepath (or (and out (handle-new-file-exists out))
                        (om-choose-new-file-dialog :types (list (format nil (om-str :file-format) "SDIF") "*.sdif" ))))
          (columns (lcontrols self))
          (nstreams (length columns))
          (sounds (find-array-field self :sounds))
          (names (find-array-field self :src-names))
          (trajects (find-array-field self :trajectories))
          (durs (find-array-field self :durations))
          (onsets (find-array-field self :onsets))
          (orients (find-array-field self :orientations))
          (aperts (find-array-field self :apertures))
          (sndtable nil) (nametable nil) (trajdurs nil)
          (spatframes nil) (orientationframes nil) (apertureframes nil)
          (omnifilterframes nil) (axisfilterframes nil)
          (roomframes nil)
          (mode (or stream-mode 'sep)))
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
                                          ; (om-make-pathname :directory filepath :name (pathname-name soundpath) :type (pathname-type soundpath))
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
       (when (find-array-field self :omni-filter)
         (setf omnifilterframes (get-multi-value-frames (find-array-field self :omni-filter) onsets trajdurs "OMNI")))
       (when (find-array-field self :axis-filter)
         (setf axisfilterframes (get-multi-value-frames (find-array-field self :axis-filter) onsets trajdurs "AXIS")))


       ;;; PERCEPTUAL PARAMS
       (setf pereptualframes (remove nil (append
                                          (get-simple-value-frames (find-array-field self :presence) onsets trajdurs "PRES")
                                          (get-simple-value-frames (find-array-field self :warmth) onsets trajdurs "WARM")
                                          (get-simple-value-frames (find-array-field self :brillance) onsets trajdurs "BRIL")
                                          (get-simple-value-frames (find-array-field self :room-presence) onsets trajdurs "PRER")
                                          (get-simple-value-frames (find-array-field self :running-reverberance) onsets trajdurs "REVP")
                                          (get-simple-value-frames (find-array-field self :envelopment) onsets trajdurs "ENVP")
                                          )))
       
       (setf ridframes (get-simple-value-frames (find-array-field self :room) onsets trajdurs "XRID"))
       
       
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
         ))))

  