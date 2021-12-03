;============================================================================
; om#: visual programming language for computer-assisted music composition
;============================================================================
;
;   This program is free software. For information on usage
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
;============================================================================
; File author: J. Bresson
;============================================================================

(in-package :om)

;; ================= Some Functions =====================

(defun lisp-list_2_python-list (list)
      "Transform a list in lisp to a list in Python."
(let* (
      (list2string (mapcar (lambda (x) (ckn-int2string x)) list)))
      (loop :for x :in list2string :collect (string+ x ", "))))


;; ================ Python Code Editor Inside OM =================

(defclass! py ()
    ((py-om :initform nil :initarg :py-om :accessor py-om)))


(defclass OMPYFunction (OMProgrammingObject)
  ((text :initarg :text :initform "" :accessor text)
   (error-flag :initform nil :accessor error-flag)
   ))

;; ======

(defmethod get-object-type-name ((self OMPYFunction)) "Py")

;; ======

(defmethod default-compiled-gensym  ((self OMPYFunction)) (gensym "pyfun-"))

;; ======

(defclass OMPyFunctionInternal (OMPYFunction) ()
  (:default-initargs :icon :py-f)
  (:metaclass omstandardclass))

;; ======

(defparameter *default-py-function-text*

  '(";;; edit a valid python code,"
    ";;; changing the variables you want to use "
    ";;; inside om-sharp to {til}d."
    "(lambda () (format nil
\"
# Here you are work with Python code.
# PUT_YOUR_CODE_HERE (leave the quotes). For example

sum = 2 + 2 
print(sum) # If you want to use something inside OM, you need to print it.

\"  

     ))"))

;; ======

(defmethod omNG-make-special-box ((reference (eql 'py)) pos &optional init-args)
  (omNG-make-new-boxcall
   (make-instance 'OMPyFunctionInternal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "    py    ")
                  :text *default-py-function-text*)
   pos init-args))

;; ======

(defmethod decapsulable ((self OMPYFunction)) nil)

;; ======

(defmethod update-lisp-fun ((self OMPYFunction))
  (compile-patch self)
  (loop :for item :in (references-to self) do
        (update-from-reference item)))

;; ======

(defmethod copy-contents ((from OMPYFunction) (to OMPYFunction))
  (setf (text to) (text from)) to)

;; ======

(defmethod compile-patch ((self OMPYFunction))
  "Compilation of a py function"
    (setf (error-flag self) nil)
    (let* (
      (lambda-expression (read-from-string (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2)) (text self)) nil))
      (function-def
            (if (and lambda-expression (lambda-expression-p lambda-expression))
                  (progn (setf (compiled? self) t)
                        (let* (
                              (var (car (cdr lambda-expression)))
                              (code (list (flat (x-append (list (second (cdr lambda-expression))) var))))
                              (py-code (list `(make-value (quote py) (list (list :py-om ,@code))))))
                              (print `(defun ,(intern (string (compiled-fun-name self)) :om) 
                                              ,var ;;variaveis 
                                              ,@py-code
                                              ))))                                                      
                          (progn (om-beep-msg "ERROR IN LISP FORMAT!!")
                              (setf (error-flag self) t)
                              `(defun ,(intern (string (compiled-fun-name self)) :om) () nil)))))     
    (compile (eval function-def))))


;;;===================
;;; py FUNCTION BOX
;;;===================

(defmethod special-box-p ((name (eql 'py))) t)

;; ======

(defclass OMBoxpy (OMBoxAbstraction) ())

;; ======

(defmethod get-box-class ((self OMpyFunction)) 'OMBoxpy)

;; ======

(defmethod draw-patch-icon :after ((self OMBoxpy) &optional (offset-x 0) (offset-y 0))
  (when (error-flag (reference self))
    (om-with-fg-color (om-def-color :darkgreen)
      (om-with-font (om-make-font "Arial" 14 :style '(:bold))
                    (om-draw-string (+ offset-x 2) (+ offset-y (- (box-h self) 8)) "Error !!")))))

;; ======

(defmethod create-box-inputs ((self OMBoxpy))
  (compile-if-needed (reference self))
  (let ((fname (intern (string (compiled-fun-name (reference self))) :om)))
    (when (fboundp fname)
      (let ((args (function-arg-list fname)))
        (loop :for a :in args collect
              (make-instance 'box-input :name (string a)
                             :box self :reference nil)))
      )))

;; ======

(defmethod create-box-outputs ((self OMBoxpy))
  (list
   (make-instance 'box-output :reference nil
                  :name "out"
                  :box self)))

;; ======

(defmethod update-from-reference ((self OMBoxpy))

  (let ((new-inputs (loop :for i :in (create-box-inputs self)
                          :for ni from 0 collect
                          (if (nth ni (inputs self))
                              (let ((ci (copy-io (nth ni (inputs self))))) ;;keep connections, reactivity etc.
                                (setf (name ci) (name i)) ;; just get the new name
                                ci)
                            i)))
        (new-outputs (loop :for o :in (create-box-outputs self)
                           :for no :from 0 collect
                           (if (nth no (outputs self))
                               (copy-io (nth no (outputs self)))
                             o))))

;; ======

    ;;; remove orphan connections
    (loop :for in :in (nthcdr (length new-inputs) (inputs self)) do
          (mapc #'(lambda (c) (omng-remove-element (container self) c)) (connections in)))

    (set-box-inputs self new-inputs)
    (set-box-outputs self new-outputs)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    t))

;; ======

(defmethod display-modes-for-object ((self OMpyFunction)) '(:hidden :mini-view :value))

;; ======

(defmethod draw-mini-view ((self OMpyFunction) box x y w h &optional time)
  (let ((di 12))
    (om-with-font
     (om-def-font :font1 :size 10)
     (loop :for line :in (text self)
           :for i = (+ y 18) then (+ i di)
           while (< i (- h 18)) do
           (om-draw-string (+ x 12) i line)))))

;;;===================
;;; EDITOR
;;;===================

(defclass py-function-editor (OMDocumentEditor) ())

(defmethod object-has-editor ((self OMpyFunction)) t)
(defmethod get-editor-class ((self OMpyFunction)) 'py-function-editor)

;;; nothing, e.g. to close when the editor is closed
(defmethod delete-internal-elements ((self OMpyFunction)) nil)

;;; maybe interesting to make this inherit from OMEditorWindow..

(defclass py-function-editor-window (om-lisp::om-text-editor-window)
  ((editor :initarg :editor :initform nil :accessor editor)))

(defmethod window-name-from-object ((self OMpyFunctionInternal))
  (format nil "~A  [internal py function]" (name self)))

(defmethod om-lisp::type-filter-for-text-editor ((self py-function-editor-window))
  '("py function" "*.olsp"))

;;; this will disable the default save/persistent behaviours of the text editor
;;; these will be handled following the model of OMPatch
(defmethod om-lisp::save-operation-enabled ((self py-function-editor-window)) nil)

(defmethod open-editor-window ((self py-function-editor))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (let* ((pyf (object self))
           (edwin (om-lisp::om-open-text-editor
                   :contents (text pyf)
                   :lisp t
                   :class 'py-function-editor-window
                   :title (window-name-from-object pyf)
                   :x (and (window-pos pyf) (om-point-x (window-pos pyf)))
                   :y (and (window-pos pyf) (om-point-y (window-pos pyf)))
                   :w (and (window-size pyf) (om-point-x (window-size pyf)))
                   :h (and (window-size pyf) (om-point-y (window-size pyf)))
                   )))
      (setf (editor edwin) self)
      (setf (window self) edwin)
      (om-lisp::text-edit-window-activate-callback edwin t) ;; will (re)set the menus with the editor in place
      edwin)))

(defmethod om-lisp::text-editor-window-menus ((self py-function-editor-window))
  (om-menu-items (editor self)))

(defmethod om-lisp::om-text-editor-modified ((self py-function-editor-window))
  (touch (object (editor self)))
  (setf (text (object (editor self)))
        (om-lisp::om-get-text-editor-text self))
  (report-modifications (editor self))
  (call-next-method))

(defmethod om-lisp::update-window-title ((self py-function-editor-window) &optional (modified nil modified-supplied-p))
  (om-lisp::om-text-editor-window-set-title self (window-name-from-object (object (editor self)))))

(defmethod om-lisp::om-text-editor-check-before-close ((self py-function-editor-window))
  (ask-save-before-close (object (editor self))))

(defmethod om-lisp::om-text-editor-resized ((win py-function-editor-window) w h)
  (when (editor win)
    (setf (window-size (object (editor win))) (omp w h))))

(defmethod om-lisp::om-text-editor-moved ((win py-function-editor-window) x y)
  (when (editor win)
    (setf (window-pos (object (editor win))) (omp x y))))

;;; update-py-fun at closing the window
(defmethod om-lisp::om-text-editor-destroy-callback ((win py-function-editor-window))
  (let ((ed (editor win)))
    (editor-close ed)
    (update-lisp-fun (object ed))
    (setf (window ed) nil)
    (setf (g-components ed) nil)
    (unless (references-to (object ed))
      (unregister-document (object ed)))
    (call-next-method)))

;;; update-py-fun at loosing focus
(defmethod om-lisp::om-text-editor-activate-callback ((win py-function-editor-window) activate)
  (when (editor win)
    (when (equal activate nil)
      (update-lisp-fun (object (editor win))))
    ))

;;; called from menu
(defmethod copy-command ((self py-function-editor))
  #'(lambda () (om-lisp::text-edit-copy (window self))))

(defmethod cut-command ((self py-function-editor))
  #'(lambda () (om-lisp::text-edit-cut (window self))))

(defmethod paste-command ((self py-function-editor))
  #'(lambda () (om-lisp::text-edit-paste (window self))))

(defmethod select-all-command ((self py-function-editor))
  #'(lambda () (om-lisp::text-select-all (window self))))

(defmethod undo-command ((self py-function-editor))
  #'(lambda () (om-lisp::text-edit-undo (window self))))

(defmethod font-command ((self py-function-editor))
  #'(lambda () (om-lisp::change-text-edit-font (window self))))

;============================================================================
; om#: visual programming language for computer-assisted music composition
;============================================================================
;
;   This program is free software. For information on usage
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
;============================================================================
; File author: J. Bresson , Python functions for om by Charles K. Neimog
;============================================================================

(in-package :om)

;; ==========================================================================

(defvar *alphabet* (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))


;; ========================================================================== 

(defun read_from_python (x) 
    (loop :for y :in (list! x) :collect (read-from-string y)))

;; ================ Python Code Editor Inside OM =================

(defclass run-py-f (OMProgrammingObject)
  ((text :initarg :text :initform "" :accessor text)
   (error-flag :initform nil :accessor error-flag)
   ))

;; ======

(defmethod get-object-type-name ((self run-py-f)) "run")

;; ======

(defmethod default-compiled-gensym  ((self run-py-f)) (gensym "py-run-"))

;; ======

(defclass run-py-f-internal (run-py-f) ()
  (:default-initargs :icon :py-f)
  (:metaclass omstandardclass))

;; ======

(defparameter *default-py-run-function-text*

  '(";;; edit a valid python code, It will just run it."
    ";;; changing the variables you want to use "
    ";;; inside om-sharp to {til}d."
    ";;; The name 'LIST' CANNOT be used as a variable name."
    "(lambda () (format nil
\"
from om_ckn import to_om

list_of_numbers = []

for x in range(10):
    list_of_numbers.append(x)

to_om(list_of_numbers)

\"  

     ))"))

;; ======

(defmethod omNG-make-special-box ((reference (eql 'py-run)) pos &optional init-args)
  (omNG-make-new-boxcall
   (make-instance 'run-py-f-internal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "    py    ")
                  :text *default-py-run-function-text*)
   pos init-args))

;; ======

(defmethod decapsulable ((self run-py-f)) nil)

;; ======

(defmethod update-lisp-fun ((self run-py-f))
  (compile-patch self)
  (loop for item in (references-to self) do
        (update-from-reference item)))

;; ======

(defmethod copy-contents ((from run-py-f) (to run-py-f))
  (setf (text to) (text from)) to)

;; ======

(defmethod compile-patch ((self run-py-f))
  "Compilation of a py function"
    (setf (error-flag self) nil)
    (let* (
      (lambda-expression (read-from-string (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2)) (text self)) nil))
      (var (car (cdr lambda-expression)))
      (length-var (length var))
      (code (list (flat (x-append (list (second (cdr lambda-expression))) var))))
      (py-code `(read_from_python (run-py (make-value (quote py) (list (list :py-om ,@code))))))
      (function-def
            (if (and lambda-expression (lambda-expression-p lambda-expression))

                  (progn (setf (compiled? self) t)
                        `(defun ,(intern (string (compiled-fun-name self)) :om) 
                                              ,var ;;variaveis 
                                              ,py-code))                                                        

                  (progn (om-beep-msg "ERROR IN LISP FORMAT!!")
                        (setf (error-flag self) t)
                       `(defun ,(intern (string (compiled-fun-name self)) :om) () nil)))))


; (py-add-var (lambda (#:g24586 #:g24587) (funcall '|pyfun-18529| #:g24586 #:g24587)) (list 2 3 4 65) (list 2 3 4 65))
    
(compile (eval (print function-def)))))


;;;===================
;;; py FUNCTION BOX
;;;===================

(defmethod special-box-p ((name (eql 'py-run))) t)

;; ======

(defclass OMBox-run-py (OMBoxAbstraction) ())

;; ======

(defmethod get-box-class ((self run-py-f)) 'OMBox-run-py)

;; ======

(defmethod draw-patch-icon :after ((self OMBox-run-py) &optional (offset-x 0) (offset-y 0))
  (when (error-flag (reference self))
    (om-with-fg-color (om-def-color :darkgreen)
      (om-with-font (om-make-font "Arial" 14 :style '(:bold))
                    (om-draw-string (+ offset-x 2) (+ offset-y (- (box-h self) 12)) "Error !!")))))

;; ======

(defmethod create-box-inputs ((self OMBox-run-py))
  (compile-if-needed (reference self))
  (let ((fname (intern (string (compiled-fun-name (reference self))) :om)))
    (when (fboundp fname)
      (let ((args (function-arg-list fname)))
        (loop for a in args collect
              (make-instance 'box-input :name (string a)
                             :box self :reference nil)))
      )))

;; ======

(defmethod create-box-outputs ((self OMBox-run-py))
  (list
   (make-instance 'box-output :reference nil
                  :name "out"
                  :box self)))

;; ======

(defmethod update-from-reference ((self OMBox-run-py))

  (let ((new-inputs (loop for i in (create-box-inputs self)
                          for ni from 0 collect
                          (if (nth ni (inputs self))
                              (let ((ci (copy-io (nth ni (inputs self))))) ;;keep connections, reactivity etc.
                                (setf (name ci) (name i)) ;; just get the new name
                                ci)
                            i)))
        (new-outputs (loop for o in (create-box-outputs self)
                           for no from 0 collect
                           (if (nth no (outputs self))
                               (copy-io (nth no (outputs self)))
                             o))))

;; ======

    ;;; remove orphan connections
    (loop for in in (nthcdr (length new-inputs) (inputs self)) do
          (mapc #'(lambda (c) (omng-remove-element (container self) c)) (connections in)))

    (set-box-inputs self new-inputs)
    (set-box-outputs self new-outputs)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    t))

;; ======

(defmethod display-modes-for-object ((self run-py-f)) '(:hidden :mini-view :value))

;; ======

(defmethod draw-mini-view ((self run-py-f) box x y w h &optional time)
  (let ((di 12))
    (om-with-font
     (om-def-font :font1 :size 10)
     (loop for line in (text self)
           for i = (+ y 18) then (+ i di)
           while (< i (- h 18)) do
           (om-draw-string (+ x 12) i line)))))

;;;===================
;;; EDITOR
;;;===================

(defclass run-py-function-editor (OMDocumentEditor) ())

(defmethod object-has-editor ((self run-py-f)) t)
(defmethod get-editor-class ((self run-py-f)) 'run-py-function-editor)

;;; nothing, e.g. to close when the editor is closed
(defmethod delete-internal-elements ((self run-py-f)) nil)

;;; maybe interesting to make this inherit from OMEditorWindow..

(defclass run-py-function-editor-window (om-lisp::om-text-editor-window)
  ((editor :initarg :editor :initform nil :accessor editor)))

(defmethod window-name-from-object ((self run-py-f-Internal))
  (format nil "~A  [internal py function]" (name self)))

(defmethod om-lisp::type-filter-for-text-editor ((self run-py-function-editor-window))
  '("py function" "*.olsp"))

;;; this will disable the default save/persistent behaviours of the text editor
;;; these will be handled following the model of OMPatch
(defmethod om-lisp::save-operation-enabled ((self run-py-function-editor-window)) nil)

(defmethod open-editor-window ((self run-py-function-editor))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (let* ((pyf (object self))
           (edwin (om-lisp::om-open-text-editor
                   :contents (text pyf)
                   :lisp t
                   :class 'run-py-function-editor-window
                   :title (window-name-from-object pyf)
                   :x (and (window-pos pyf) (om-point-x (window-pos pyf)))
                   :y (and (window-pos pyf) (om-point-y (window-pos pyf)))
                   :w (and (window-size pyf) (om-point-x (window-size pyf)))
                   :h (and (window-size pyf) (om-point-y (window-size pyf)))
                   )))
      (setf (editor edwin) self)
      (setf (window self) edwin)
      (om-lisp::text-edit-window-activate-callback edwin t) ;; will (re)set the menus with the editor in place
      edwin)))

(defmethod om-lisp::text-editor-window-menus ((self run-py-function-editor-window))
  (om-menu-items (editor self)))

(defmethod om-lisp::om-text-editor-modified ((self run-py-function-editor-window))
  (touch (object (editor self)))
  (setf (text (object (editor self)))
        (om-lisp::om-get-text-editor-text self))
  (report-modifications (editor self))
  (call-next-method))

(defmethod om-lisp::update-window-title ((self run-py-function-editor-window) &optional (modified nil modified-supplied-p))
  (om-lisp::om-text-editor-window-set-title self (window-name-from-object (object (editor self)))))

(defmethod om-lisp::om-text-editor-check-before-close ((self run-py-function-editor-window))
  (ask-save-before-close (object (editor self))))

(defmethod om-lisp::om-text-editor-resized ((win run-py-function-editor-window) w h)
  (when (editor win)
    (setf (window-size (object (editor win))) (omp w h))))

(defmethod om-lisp::om-text-editor-moved ((win run-py-function-editor-window) x y)
  (when (editor win)
    (setf (window-pos (object (editor win))) (omp x y))))

;;; update-py-fun at closing the window
(defmethod om-lisp::om-text-editor-destroy-callback ((win run-py-function-editor-window))
  (let ((ed (editor win)))
    (editor-close ed)
    (update-lisp-fun (object ed))
    (setf (window ed) nil)
    (setf (g-components ed) nil)
    (unless (references-to (object ed))
      (unregister-document (object ed)))
    (call-next-method)))

;;; update-py-fun at loosing focus
(defmethod om-lisp::om-text-editor-activate-callback ((win run-py-function-editor-window) activate)
  (when (editor win)
    (when (equal activate nil)
      (update-lisp-fun (object (editor win))))
    ))

;;; called from menu
(defmethod copy-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::text-edit-copy (window self))))

(defmethod cut-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::text-edit-cut (window self))))

(defmethod paste-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::text-edit-paste (window self))))

(defmethod select-all-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::text-select-all (window self))))

(defmethod undo-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::text-edit-undo (window self))))

(defmethod font-command ((self run-py-function-editor))
  #'(lambda () (om-lisp::change-text-edit-font (window self))))

; =======================================================
; Functions to be used in the patches
; =======================================================


(defmethod! run-py ((code string) &optional (cabecario nil))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc "With this object you can see the index parameters of some VST2 plugin."

(let* (
      (python-code (x-append cabecario " 

" code))
      (python-name (string+ "om-ckn-code" (ckn-int2string (om-random 10000 999999)) ".py"))
      (save-python-code (om::save-as-text python-code (om::tmpfile python-name :subdirs "om-ckn")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om-cmd-line (string+ "python " prepare-cmd-code))
      (mp:process-run-function "del-py-code" () (lambda (x) (ckn-clear-the-file x)) (om::tmpfile python-name :subdirs "om-ckn"))
      'end))

;; ================================================

(defmethod! run-py ((code py) &optional (cabecario nil))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc "With this object you can see the index parameters of some VST2 plugin."

(run-py (make-value 'to-om (list (list :py-inside-om (py-om code)))) cabecario))

;; ================================================

(defmethod! run-py ((code list) &optional (cabecario nil))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc "With this object you can see the index parameters of some VST2 plugin."

(let* (
      (python-code (x-append cabecario " 

" (concatstring code)))
      (save-python-code (om::save-as-text python-code (om::tmpfile (string+ "om-ckn-code" (ckn-int2string (om-random 10000 999999)) ".py") :subdirs "om-ckn")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om-cmd-line (string+ "python " prepare-cmd-code))
      (mp:process-run-function "del-py-code" () (lambda (x) (ckn-clear-the-file x)) (om::tmpfile "om-ckn-code.py"))
      'end))

;; ================================================ BRING TO OM ===========

(defclass! to-om ()
    ((py-inside-om :initform nil :initarg :py-inside-om :accessor py-inside-om)))

;; ========================

(defmethod! py-add-var ((function function) &rest rest)
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc ""
(let* (
      (check-all-rest (loop :for type :in (om::list! rest) :collect (format2python type)))
      (py (apply 'mapcar function check-all-rest)))
      (make-value 'py (list (list :py-om (concatstring (mapcar (lambda (x) (py-om x)) py)))))))



;; ========================

(defmethod! bring-to-om ((code string))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc ""

(make-value 'to-om (list (list :py-inside-om code))))

;; ========================

(defmethod! bring-to-om ((code list))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc ""

(make-value 'to-om (list (list :py-inside-om code))))


;; ========================

(defmethod! run-py ((code to-om) &optional (cabecario nil))
:initvals '(nil)
:indoc '("run py") 
:icon 'py-f
:doc ""

(let* (
      (python-code (x-append cabecario " 

" (py-inside-om code)))
      (python-name (string+ "om-ckn-code" (ckn-int2string (om-random 10000 999999)) ".py"))
      (data-name (string+ "data" (ckn-int2string (om-random 10000 999999)) ".txt"))
      (save-python-code (om::save-as-text python-code (om::tmpfile python-name :subdirs "om-ckn")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om-cmd-line (string+ "python " prepare-cmd-code " > " (list->string-fun (list (namestring (tmpfile data-name :subdirs "om-ckn"))))))
      (let* (
            (data (make-value-from-model 'textbuffer (tmpfile data-name :subdirs "om-ckn") nil)))
            (mp:process-run-function "del-py-code" () (lambda (x) (ckn-clear-the-file x)) (om::tmpfile python-name :subdirs "om-ckn"))
            (mp:process-run-function "del-data-code" () (lambda (x) (ckn-clear-the-file x)) (om::tmpfile data-name :subdirs "om-ckn"))
            (contents data))))

;; ========================

(defmethod! py->lisp ((result list))
:initvals '(nil)
:indoc '("result from py.") 
:icon 'py-f
:doc ""

(loop :for lista :in result
      :collect (remove nil (mapcar (lambda (y) (ignore-errors (parse-float y))) (string-to-list lista " ")))))

;; ========================

(defmethod! py-list ((result list))
:initvals '(nil)
:indoc '("result from py.") 
:icon 'py-f
:doc ""

(list (lisp-list_2_python-list result)))

;==================================
(defun format2python (type)

(case (type-of type)
        (lispworks:simple-text-string (list type))
        (sound (if (not (file-pathname type)) 
                    (list (ckn-temp-sounds type (string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-")))
                    (list (file-pathname type))))
        (fixnum (list (ckn-int2string type)))
        (float (ckn-int2string type))
        (cons (flat (mapcar (lambda (x) (list (format2python x))) type))) 
        (single-float (list (ckn-int2string type)))
        (pathname   (list (namestring type)))))

;; ============== VISUAL THINGS =================

(defun bpf-python-fun (X Y Z color)
      "Build a BPF using Python."
(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 10000
X = ~d
Y = ~d
plt.figure(figsize=(20, 10), dpi=100)
plt.plot(X, Y, lw=~d, color='~d')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.show()
" x y z color))
      (save-python-code (om::save-as-text python-code (om::outfile "bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ============
(defun save-bpf-python-fun (X Y thickness color outfile blackback dpi)
      "Save a BPF using Python."

(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 1000
~d.style.use('dark_background')
X = ~d
Y = ~d
plt.figure(figsize=(20, 10), dpi=~d)
plt.plot(X, Y, lw=~d, color='~d')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.rcParams['agg.path.chunksize'] = 10000
plt.savefig(~d)
plt.show()
print('Documento Salvo em ~d')
" blackback x y dpi thickness  color outfile outfile))
      (save-python-code (om::save-as-text python-code (om::outfile "save-bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ============
(defun bpf-python-om (X Y Z)
(let* (
      (python-code (format nil
                    "
import matplotlib.pyplot as plt
X = ~d
Y = ~d
plt.figure(figsize=(14, 7))
plt.plot(X, Y, lw=~d, color='black')
plt.subplots_adjust(left=0.02, right=0.986, top=0.986, bottom=0.029)
plt.show()
" x y z))
      (save-python-code (ckn-save-as-text python-code (om::outfile "bpf.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ============
(defun 3dc-python-fun (X Y Z A color)
(let* (
      (python-code (format nil
                    "
from mpl_toolkits import mplot3d
import numpy as np
import matplotlib.pyplot as plt
plt.rcParams['agg.path.chunksize'] = 10000
plt.figure(figsize=(5, 5), dpi=100)
ax = plt.axes(projection='3d')
ax.xaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.yaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.zaxis.set_pane_color((1.0, 1.0, 1.0, 1.0))
ax.xaxis._axinfo['grid']['color'] =  (1,0,1,0)
ax.yaxis._axinfo['grid']['color'] =  (1,0,1,0)
ax.zaxis._axinfo['grid']['color'] =  (1,0,0,0)
zline = ~d
xline = ~d
yline = ~d
plt.subplots_adjust(left=0.0, right=1, top=1, bottom=0.0)
ax.plot3D(xline, yline, zline,  lw=~d, color='~d')
plt.show()
" x y z a color))
      (save-python-code (om::save-as-text python-code (om::outfile "3dc.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))))

;; ================================================
;;;; Working with Vamp Plugins        =============
;; ================================================

(defmethod! vamp-list-plugins nil
:icon 'py-f
:doc "
This object will output a list of all the available Vamp plugins.
"

(let* (
      (python-code (format nil
                    "
from om_ckn import to_om
import vamp
to_om(vamp.list_plugins())"
))
      (run (read_from_python (run-py (make-value (quote py) (list (list :py-om python-code)))))))
      (flat run 1)))

;; ================================================

(defmethod! vamp-process ((sound string) (vamp_key string))
(let* (
      (python-code (format nil
                    "
import vamp
import librosa
from om_ckn import to_om

# from om_ckn import to_om

data, rate = librosa.load(r'~d')
output = vamp.collect(data, rate, '~d')
to_om(output)

" sound vamp_key))
      (run (read_from_python (run-py (make-value (quote py) (list (list :py-om python-code)))))))
      run))
      
;; ================================================
(defmethod! vamp-process ((sound pathname) (vamp_key string))
:icon 'py-f
:doc "
This object will process some audio using Vamp plugins.
"
(vamp-process (namestring sound) vamp_key))


;; ================================================

(defmethod! vamp-filter-by-prefix ((vamp_plugins list) (vamp_prefix string))
:icon 'py-f
:doc "
Filters plugins using the prefix."

(let* (
      (filter-function (lambda (x) (if (equal vamp_prefix (first (string-to-list x ":"))) x nil))))
      (remove nil (mapcar filter-function vamp_plugins))))

      
; ====================== Add the functions in OM-Menu =======================

(omNG-make-package
        "Python"
        :doc "Visual program manipulation"
        :container-pack *om-package-tree*
        :special-symbols '(py py-run)
        :subpackages
                (list (omNG-make-package
                            "Visual Functions"
                            :doc "Visual Functions with Python"
                            :functions '(bpf-python 3dc-python))
                      (omNG-make-package
                            "Work with py Special Box"
                            :doc "Functions to organize and run python code efficiently."
                            :functions '(run-py py-add-var bring-to-om py->lisp))
                      (omNG-make-package
                            "Implementation of vamp plugins in OM"
                            :doc "Functions to use vamp plugins inside OM."
                            :functions '(vamp-list-plugins vamp-process vamp-filter-by-prefix)
                                    )))

; ====================== Update-Menu if the library will be loaded with the opened patches  =======================

(update-preference-window-module :libraries)
