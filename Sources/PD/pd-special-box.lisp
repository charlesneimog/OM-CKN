(in-package :om)



;;;===============================================================
;;; PATCH SELECTION, FILE BY JEAN BRESSON
;;;===============================================================

(defclass PD-Patches (OMInterfaceBox)
  ((items :accessor items :initarg :items :initform (pd-patches-list))
   (selection :accessor selection :initarg :selection :initform 0)
   (output-mode :accessor output-mode :initarg :output-mode :initform :value)
   (font :accessor font :initarg :font :initform (om-def-font :font1b)))
  (:documentation "An interface box to graphically select among different items in a list, using a drop-down menu.
Use the optional inputs to set the list of items.
Click with CMD (Mac) or Shift+Ctrl (Windows/Linux) to change the selected item.
Returns the selected item, or the selected index depending on how this is set in the box properties."
   ))

;; ==========================================
(defmethod special-box-p ((self (eql 'pd-list-patches))) t)
(defmethod special-item-reference-class ((item (eql 'pd-list-patches))) 'PD-Patches)

;; ==========================================
(defmethod default-size ((self PD-Patches)) (omp 180 30))
(defmethod maximum-size ((self PD-Patches)) (omp nil 30))
(defmethod minimum-size ((self PD-Patches)) (omp 180 30))

;; ==========================================

(defmethod get-all-keywords ((self PD-Patches))
  '((:items)))

;; ==========================================

(defmethod get-properties-list ((self PD-Patches))
  (add-properties (call-next-method)
                  "Menu selection display"
                  `((:font "Font" :font font)
                    (:output-mode "Output mode" (:value :index) ckn-output-mode-accessor)
                    
                    )))


;; ==========================================

(defmethod ckn-output-mode-accessor ((self pd-patches) &optional value)
  (when value
    (setf (output-mode self) value)
    (update-value-from-selection self))
  (output-mode self))


;; ==========================================

(defmethod update-value-from-selection ((self PD-Patches))
  
  (set-value self
             (pd-define-patch (if (equal (output-mode self) :value)
                 (and (selection self) (list (nth (selection self) (items self))))
               (list (selection self))))))


;; ==========================================

(defmethod apply-box-attributes ((self PD-Patches) attributes)
  
(if (null (value self))
      (om::om-message-dialog "There is no Patch selected!"))

  (when attributes
    (let ((newlist (getf attributes :items)))
      (unless (equal newlist (items self))
        (setf (selection self) 0)
        (set-value self nil))))
  (call-next-method))


;; ==========================================

(defmethod omng-save ((self PD-Patches))
  (append (call-next-method)
          `((:items ,(omng-save (items self)))
            (:selection ,(omng-save (selection self))))))

;; ==========================================

(defmethod load-box-attributes ((box PD-Patches) data)
  (setf (items box) (omng-load (find-value-in-kv-list data :items)))
  (setf (selection box) (omng-load (find-value-in-kv-list data :selection)))
  box)

;; ==========================================

(defmethod omNG-make-special-box ((reference (eql 'pd-list-patches)) pos &optional init-args)
  (let* ((box (make-instance 'PD-Patches
                             :name "pd-list-patches"
                             :reference 'pd-list-patches)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

;; =======================

(defmethod draw-interface-component ((self PD-Patches) x y w h)

(om-draw-rounded-rect x y 22 h :round (box-draw-roundness self) :color (om-def-color :black) :fill t)

(let* ((font (or (font self) (om-def-font :font1b)))
          (text-h (cadr (multiple-value-list (om-string-size "A" font))))
          (text-y-pos (if (>= text-h h) h (* .5 (+ h text-h 1)))))

      (om-with-font font (om-draw-string (+ x 30) text-y-pos (format nil "~A" (nth (selection self) (items self)))))))

;; =======================

(defmethod interfacebox-action ((self PD-Patches) frame pos)
  
  (when (or (om-action-key-down)
            (container-frames-locked (om-view-container frame)))

    (when (< (om-point-x pos) 30)

      (let ((menu (om-make-menu "list items"
                                (loop for item in (items self)
                                      for i from 0
                                      collect (let ((sel i))
                                                (om-make-menu-item
                                                 (format nil "~A" item)
                                                 #'(lambda ()
                                                     (store-current-state-for-undo (editor (container self)))
                                                     (setf (selection self) sel)
                                                     (update-value-from-selection self)
                                                     (when (reactive (car (outputs self))) (self-notify self))
                                                     (om-invalidate-view frame))
                                                 :selected (= i (selection self))
                                                 ))))))

        (om-open-pop-up-menu menu (om-view-container frame))

        ))))
