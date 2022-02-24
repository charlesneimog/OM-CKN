(in-package :om)


; ========================================== FUNCTIONS ==========================================

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement. From http://cl-cookbook.sourceforge.net/strings.html"
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


; ================================================================

(defclass! pure-data ()
    (
        (pd :initform (list->string-fun (list (namestring (get-pref-value :externals :PureData)))) :initarg :pd :accessor pd)
        (pd-path :initform nil :initarg :pd-path :accessor pd-path))
                                                                                    )

; ================================================================

(defmethod! pd~ ((sound-in null) (sound-out pathname) (patch pure-data) (var list) &key (gui t) (offline t) (verbose nil))
(ckn-pd~ sound-in sound-out patch var gui offline))

; ================================================================


(defmethod! pd~ ((sound-in pathname) (sound-out pathname) (patch pure-data) (var list) &key (gui t) (offline t) (verbose nil))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc ""

(ckn-pd~ sound-in sound-out patch var gui offline verbose))

; ================================================================

(defun ckn-pd~ (sound-in sound-out patch var gui offline verbose)

(let* (
    (outfile (if (null sound-out) "" (om::string+ "outfile " (replace-all (namestring sound-out) "\\" "/") ", "))) 
    (infile (if (null sound-in) "" (om::string+ "infile " (replace-all (namestring sound-in) "\\" "/") ", ")))
    (make_var (concatstring (loop :for all_variables :in var :collect (om::string+ (car all_variables) " " (concatstring (mapcar (lambda (x) (om::string+ (write-to-string x) " ")) (cdr all_variables))) ", "))))
    (variaveis (list->string-fun (list (string+ "from_om " outfile infile make_var))))
    (pd-executable (pd patch))
    (verbose (if verbose " " " -noverbose "))
    (gui (if gui " " " -nogui"))
    (offline (if offline " -batch " ""))
    (pd-path (replace-all (namestring (pd-path patch)) "\\" "/")))
    (oa::om-command-line (om::string+ pd-executable gui offline " -open " pd-path " -send " variaveis) verbose)
    (if gui (om::om-print "Finish!" "PD") nil)
    sound-out))

; ================================================================

(defmethod! pd-list-patches ()
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc ""
(let* (
        (thepath (get-pref-value :externals :Pd-Patches))
        (thefilelist (search-patches "pd")))
        (remove nil 
            (loop for patches in thefilelist 
                :collect (let* (
                    (name-of-patch (name-of-file patches))
                    (check-if-is-children (car (string-to-list name-of-patch "_"))))
                    (if (not (equal check-if-is-children "Children"))
                        (name-of-file patches)))))))


; ================================================================
(defmethod! pd-define-patch ((name-of-patch string))
:initvals '(nil)
:indoc '("Define the fxp-presets to use in your sound.") 
:icon 'pd
:doc "It defines the fxp-presets to use in your sound with the object ckn-VST2. You need be careful because the binaries that I am using not accept very long paths. Then prefer smaller paths."

(let* (
(action1 (probe-file (merge-pathnames name-of-patch (namestring (get-pref-value :externals :Pd-Patches))))))
(if 
    (equal nil action1) 
    (let* ((action1 (om-print "fxp-presets not found" "Abort"))) (abort)) 
    (om::make-value 'pure-data (list (list :pd-path action1))))))


; ================================================================

(defun search-patches (extension)

      (let* (
            (thepath (get-pref-value :externals :Pd-Patches))
            (thefilelist (om-directory thepath 
                                                :type extension
                                                :directories t 
                                                :files t 
                                                :resolve-aliases nil 
                                                :hidden-files nil))

            (action1 
                  (loop :for loop-files :in thefilelist 
                        :collect (if 
                                          (system::directory-pathname-p loop-files)
                                          (search-inside-some-folder loop-files extension)
                                          loop-files))))
            (remove nil (flat action1))))