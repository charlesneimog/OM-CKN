(in-package :om)


;;;===============================================================
;;; PATCH SELECTION, FILE BY JEAN BRESSON
;;;===============================================================

(defclass PD-Patches (OMInterfaceBox)
  ((items :accessor items :initarg :items :initform (pd-patches-list))
   (selection :accessor selection :initarg :selection :initform nil)
   (multiple-selection :accessor multiple-selection :initarg :multiple-selection :initform nil)
   (cell-height :accessor cell-height :initarg :cell-height :initform 18)
   (cell-font :accessor cell-font :initarg :cell-font :initform #S(graphics-ports:font-description :attributes (:family "Segoe UI" :size 7 :slant :roman :weight :bold :charset :ansi)))
   (output-mode :accessor output-mode :initarg :output-mode :initform :value))
  (:documentation "An interface box to graphically select among different items in a list.
Use the optional inputs to set the list of items.
Click with CMD or when the patch is locked to change the selected item.
Returns the selected item, or the selected index depending on how this is set in the box properties.
Can return a list of selected items if 'multiple selection' is enabled in the box properties."
   ))

; ======================================================

(defmethod special-box-p ((self (eql 'pd-list-patches))) t)
(defmethod special-item-reference-class ((item (eql 'pd-list-patches))) 'PD-Patches)

(defmethod default-size ((self PD-Patches)) (omp 240 60))
(defmethod maximum-size ((self PD-Patches))
  (omp nil (max 60 (+ 16 (* (+ 2 (cell-height self)) (length (items self)))))))

;; =================================================================

(defmethod get-all-keywords ((self PD-Patches))
  '((:items)))

;; =================================================================

(defmethod get-properties-list ((self PD-Patches))
  (add-properties (call-next-method)
                  "List selection display"
                  `((:multiple-selection "Multiple selection" :bool multiple-selection)
                    (:cell-height "Cell size (px)" :number cell-height (10 40))
                    (:cell-font "Cell font" :font cell-font)
                    (:output-mode "Output mode" (:value :index) output-mode-accessor))))

; =================================================================

(defmethod update-value-from-selection ((self PD-Patches))
  (set-value self (pd-define-patch 
             (if (multiple-selection self)

                (if (equal (output-mode self) :value) ;; multiselection true 
                    (list (posn-match (items self) (selection self)))
                    (list (selection self)))

                (if (equal (output-mode self) :value) ;; multiselection false 
                    (and (selection self) (list (nth (car (selection self)) (items self))))
                    (list (car (selection self))))))))

; =================================================================

(defmethod output-mode-accessor ((self PD-Patches) &optional value)
  (when value
    (setf (output-mode self) value) ;; não é aqui que define o output
    (update-value-from-selection self))
    (output-mode self))

;; =================================================================

(defmethod apply-box-attributes ((self PD-Patches) attributes)
  (when attributes
    (let ((newlist (getf attributes :items)))
      (unless (equal newlist (items self))
        (setf (selection self) nil)
        (set-value self nil))

      (let ((min-size (+ 8 (* (+ 2 (cell-height self)) (length newlist)))))
        (when (< (box-h self) min-size)
          (omng-resize self (omp (box-w self) min-size))
          (reset-frame-size (frame self)))
        )
      ))
  (call-next-method))

;; =================================================================

(defmethod omng-save ((self PD-Patches))
  (append (call-next-method)
          `((:items ,(omng-save (items self)))
            (:selection ,(omng-save (selection self))))))

;; =================================================================

(defmethod load-box-attributes ((box PD-Patches) data)
  (setf (items box) (omng-load (find-value-in-kv-list data :items)))
  (setf (selection box) (omng-load (find-value-in-kv-list data :selection)))
  box)

;; =================================================================

(defmethod omNG-make-special-box ((reference (eql 'pd-list-patches)) pos &optional init-args)
  (let* ((box (make-instance 'PD-Patches
                             :name "pd-list-patches"
                             :reference 'pd-list-patches)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))


;; =================================================================

(defmethod draw-interface-component ((self PD-Patches) x y w h)
  (om-with-clip-rect (frame self) x y w h

    (let* ((text-h (cadr (multiple-value-list (om-string-size "A" (cell-font self)))))
           (text-pos (if (>= text-h (cell-height self)) (cell-height self) (* .5 (+ (cell-height self) text-h)))))

    (om-with-font
       (cell-font self)
       (loop for i = 0 then (+ i 1)
             for yy = y then (+ yy (cell-height self))
             while (< yy h)
             while (< i (length (items self)))
             do
             (when (member i (selection self))
                (om-draw-rect 3 (+ yy 2) (- w 6) (cell-height self) :fill t :color (om-def-color :dark-gray)))
                (om-draw-string 5 (+ yy text-pos) (format nil "~A" (nth i (items self)))
                             :color (if (member i (selection self)) (om-def-color :white) (om-def-color :black)))
             )))

    (when (> (* (cell-height self) (length (items self))) h)
      (om-draw-rect (- w 24) (- h 14) 16 10 :fill t :color (om-def-color :white))
      (om-draw-string (- w 18) (- h 10) "...")))) ;;; desenha o "..." quando não caber todos os itens

;; ===============================================================

(defmethod interfacebox-action ((self PD-Patches) frame pos)

  (when (or (om-action-key-down) (container-frames-locked (om-view-container frame)))

    (let* ((y (- (om-point-y pos) 4))
           (n (floor y (cell-height self))))
      (when (and (> (om-point-x pos) 5)
                 (< (om-point-x pos) (- (w frame) 10))
                 (< n (length (items self))))

        (store-current-state-for-undo (editor (container self)))

        (if (member n (selection self))

            (setf (selection self) (remove n (selection self)))

          (setf (selection self)
                (if (multiple-selection self)
                    (sort (cons n (selection self)) '<)
                  (list n))))

        (update-value-from-selection self) ;; aqui printa quando eu seleciono
        (when (reactive (car (outputs self))) (self-notify self))
        (om-invalidate-view frame)))))                                                              

