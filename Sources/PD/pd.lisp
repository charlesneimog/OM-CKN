(in-package :om)

; ================================================================

(defclass! pd ()
    (
        (pacth-path :initform nil :initarg :pacth-path :accessor pacth-path))
                                                                                    )

; ================================================================

(defmethod! pd~ ((pacth pd) (var list) &key (gui t))
:initvals '(nil)
:indoc ' ("Use PD patches inside OM-Sharp")
:icon 'pd
:doc ""
)

