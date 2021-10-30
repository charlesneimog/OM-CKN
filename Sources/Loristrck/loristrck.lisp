(in-package :om)

;; ======================== Partial Tracking ===========================

(defmethod! loristrck-analysis ((sound pathname) (outfile string) &optional (r 60) (w -1) (ht 0) (hop-o 0) (amp -90) (fd -1) (sl -1) (rbw 2000) (sdif-type 'rbep) (minbps 2) (minamp -90) (fade-time 0))
:initvals ' (NIL)
:indoc ' ("list of complex-numbers")
:icon '17359
:outdoc '("sdif")
:doc ""

(let* (
    (py-script (list->string-fun (list (namestring (merge-pathnames "sources/loristrck/partialtracking.py" (mypathname (find-library "OM-CKN")))))))
    (sndfile (list->string-fun (list (namestring sound))))
    (sdif-out (string+ " --outfile " (list->string-fun (list (namestring (outfile outfile))))))
    (resolution (format nil " -r ~d " r))
    (winsize (format nil " -w ~d " w))
    (h-time (format nil " --hoptime ~d " ht))
    (hop-overlap (format nil " --hopoverlap ~d " ht))
    (ampfloor (format nil " --ampfloor ~d " amp))
    (freqdrift (format nil " --freqdrift ~d " fd))
    (sidelobe (format nil " --sidelobe ~d " sl))
    (residuebw (format nil " --residuebw ~d " rbw))
    (croptime (format nil " --croptime ~d " hop-o))
    (sdiftype (format nil " --sdiftype ~d " sdif-type))
    (minbps (format nil " --minbps ~d " minbps))
    (minamp (format nil " --minamp ~d " minamp))
    (fadetime (format nil " --fadetime ~d " fade-time))
    (cmd (string+ "python " py-script " " sndfile " " sdif-out resolution winsize h-time hop-overlap ampfloor freqdrift sidelobe residuebw croptime sdiftype minbps minamp fadetime)))
    (ckn-cmd-line cmd)
    (loop-until-probe-file (outfile outfile))
    (outfile outfile)))


;; ====
(defmethod! loristrck-analysis ((sound sound) (outfile string) &optional (r 60) (w -1) (ht 0) (hop-o 0) (amp -90) (fd -1) (sl -1) (rbw 2000) (sdif-type 'rbep) (minbps 2) (minamp -90) (fade-time 0))
:initvals ' (NIL)
:indoc ' ("list of complex-numbers")
:icon '17359
:outdoc '("sdif")
:doc ""

(loristrck-analysis (file-pathname sound) outfile r w ht hop-o amp fd sl rbw sdif-type minbps minamp fade-time))

;; ======================== synth sdif ===========================

(defmethod! loristrck-synth ((sdif sdiffile) (outfile string) &optional (speed 1) (transposition_cents 0) (noise "gaussian"))
:initvals ' (NIL)
:indoc ' ("list of complex-numbers")
:icon '17359
:outdoc '("sdif")
:doc ""

(let* (
    (py-script (list->string-fun (list (namestring (merge-pathnames "sources/loristrck/loristrck_synth.py" (mypathname (find-library "OM-CKN")))))))
    (sdif-file (list->string-fun (list (namestring (file-pathname sdif)))))
    (sound-out (string+ " --out " (list->string-fun (list (namestring (outfile outfile))))))
    (sound-speed (format nil " --speed ~d " speed))
    (transposition (format nil " --transposition ~d " (/ transposition_cents 100)))
    (noise (format nil " --noise ~d " "gaussian"))
    (cmd (string+ "python " py-script " " sdif-file " " sound-out " " sound-speed transposition noise)))
    (ckn-cmd-line cmd)
    (loop-until-probe-file (outfile outfile))
    (outfile outfile)))

