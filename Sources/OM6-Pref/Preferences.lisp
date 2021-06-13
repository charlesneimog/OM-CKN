(in-package :om)


(defvar *SOX-PATH* "path to OM-SOX")

(pushr 'om-ckn *external-prefs*)

(defmethod get-external-name ((module (eql 'om-ckn))) "OM-CKN")


(defmethod get-external-module-path ((module (eql 'om-ckn)) modulepref) (get-pref modulepref :om-ckn))

(defmethod set-external-module-path ((module (eql 'om-ckn)) modulepref path) 
  (set-pref modulepref :om-ckn path))


(defun set-lilypond-path ()
      (set-pref (find-pref-module :externals) :om-ckn (pathname nil))
    )


(defmethod save-external-prefs ((module (eql 'om-ckn))) 
  `(:om-ckn ,(om-save-pathname *SOX-PATH*)))


(defmethod put-external-preferences ((module (eql 'om-ckn)) moduleprefs)
    (when (get-pref moduleprefs :om-ckn)
      (setf *SOX-PATH* (find-true-external (get-pref moduleprefs :om-ckn))))
    t)