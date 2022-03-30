(in-package :om)


;; ======================================
(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))
        
      ;; https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-string


;; ======================================

(defun find-num (arr num)
  (cond ((null arr) nil)              
        ((om::om= (car arr) num) t)         
        (t (find-num (cdr arr) num))))

        ;; https://stackoverflow.com/questions/67101950/problem-about-finding-the-same-number-of-a-given-number-in-list-lisp

;; ======================================