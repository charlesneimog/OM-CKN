(defmethod special-box-p ((name (eql 'py-collect))) t)
(defmethod special-item-reference-class ((item (eql 'py-collect))) ' )

(defclass CKN-Collect (OMPatchComponentWithMemory) ()
  (:documentation "Collect data in an local memory.
Inputs:
- Collected value
- When t: trigger reactive notification through outputs.
- Initial value.
Outputs:
- Collect from input 1 and return this value
- Return current state of memory
- Initialize memory with input 3."))

(defclass CKN-CollectBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self CKN-Collect)) 'CKN-CollectBox)
(defmethod box-symbol ((self CKN-Collect)) 'collect)

(defmethod get-icon-id ((self CKN-CollectBox)) :py-f)
(defmethod object-name-in-inspector ((self CKN-CollectBox)) "COLLECTOR box")

(defmethod omNG-make-special-box ((reference (eql 'collect)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall
     (make-instance 'CKN-Collect :name (if name (string name) "collect"))
     pos)))


(defmethod create-box-inputs ((self CKN-CollectBox))
  (list
   (make-instance
    'box-input :box self :value NIL
    :name "data-in" :doc-string "(collected in memory)")
   (make-instance
    'box-input :box self :value T
    :name "push" :doc-string "propagates reactive notification to the data-out outlet")
   (make-instance
    'box-input :box self :value NIL
    :name "init" :doc-string "reinitializes memory")))

(defmethod create-box-outputs ((self CKN-CollectBox))
  (list
   (make-instance
    'box-output :box self :value NIL
    :name "collect" :doc-string "collects and outputs data-in")
   (make-instance
    'box-output :box self :value NIL
    :name "data-out" :doc-string "get collected data")
   (make-instance
    'box-output :box self :value NIL
    :name "init" :doc-string "reinitializes memory")))


;;; COLLECT DOESN'T RESPOND TO EV-ONCE AT ALL: CAN BE CALLED SEVERAL TIMES
(defmethod omNG-box-value ((self CKN-CollectBox) &optional (numout 0))

  (unless nil ;;; (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*))

    (unless (value self) (setf (value self) (list nil)))

    (case numout
      ;;; collect
      (0 (let ((inval (omng-box-value (nth 0 (inputs self)))))
           (push inval (car (value self)))))
      ;;; output ; does nothing to the memory
      (1 NIL)
      ;;; init with the value in
      (2 (let ((initval (omng-box-value (nth 2 (inputs self)))))
           (setf (car (value self))
                 (if (equal initval t) NIL
                   (list! (om-copy initval))))))
      )
    )

  (return-value self numout))

(defmethod return-value ((self CKN-CollectBox) &optional (numout 0))
  (case numout
    ;;; last pushed
    (0 (caar (value self)))
    ;;; output ; does nothing to the memory
    (1 (reverse (car (value self))))
    ;;; init
    (2 nil)
    ))

;;; called in reactive mode
(defmethod current-box-value ((self CKN-CollectBox) &optional (numout nil))
  (if numout (return-value self numout) (value self)))


;;; REACTIVE BEHAVIOUR
;;; NOTIFY ONLY IF PUSH COMES IN
;;; stops reactive notification,
;;; performs evaluations if needed
;;; continue or not...

(defmethod OMR-Notify ((self CKN-CollectBox) &optional input-name)

  (unless nil ; (push-tag self) ;; allow several push at different inputs

    (setf (push-tag self) t)

    (cond
     ((string-equal input-name "data-in")
      (omNG-box-value self 0))

     ((string-equal input-name "init")
      (omNG-box-value self 2))

     ((string-equal input-name "push")

      (let ((listeners (get-listeners self)))
        (when listeners
          (setf (gen-lock self) t)
          (loop for listener in listeners do (omr-notify (car listener) (cadr listener)))
          (setf (gen-lock self) nil))))
     )
    ))
