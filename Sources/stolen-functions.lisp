(in-package :om)

(defmethod! list-dll-plugins (&key (type nil) (unix nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil) (path nil))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the dll plugins. Connect it to a LIST-SELECTION object."

            (let* ((thepath (get-pref-value :externals :plugins))
                  (thefilelist (om-directory thepath 
                                             :type "dll" :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files)))
              (mapcar (lambda (x) (get-filename x)) thefilelist)))      

;; ======================================

(defmethod! list-fxp-presets (&key (type nil) (unix nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil) (path nil))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the fxp Presets. Connect it to a LIST-SELECTION object."

            (let* ((thepath (get-pref-value :externals :fxp-presets))
                  (thefilelist (om-directory thepath 
                                             :type "fxp" :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files)))
            (mapcar (lambda (x) (get-filename x)) thefilelist)))


;; ======================================

(defun get-filename (p)
  (let ((path (and p (pathname p))))
  (when (pathnamep path)
    (string+ (pathname-name path) 
             (if (and (pathname-type path) (stringp (pathname-type path)))
                 (string+ "." (pathname-type path)) 
               "")))))

;; ======================================

(defun find-num (arr num)
  (cond ((null arr) nil)              
        ((= (car arr) num) t)         
        (t (find-num (cdr arr) num))))

        ;; https://stackoverflow.com/questions/67101950/problem-about-finding-the-same-number-of-a-given-number-in-list-lisp

;; ======================================