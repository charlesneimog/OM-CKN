;;;-*- Mode: LISP; Package: :SAPA; Syntax: COMMON-LISP -*-
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;  utilities.lisp
;
;  a collection of Lisp functions for certain basic operations ...
;  Note:  before compiling and loading utilities.lisp,
;         you should compile and load
;            sapa-package.lisp
;
;  6/3/93
;
;  SAPA, Version 1.0; Copyright 1993, Donald B. Percival, All Rights Reserved
;
;  Use and copying of this software and preparation of derivative works
;  based upon this software are permitted.  Any distribution of this
;  software or derivative works must comply with all applicable United
;  States export control laws.
; 
;  This software is made available AS IS, and no warranty -- about the
;  software, its performance, or its conformity to any
;  specification -- is given or implied. 
;
;  Comments about this software can be addressed to dbp@apl.washington.edu
;-------------------------------------------------------------------------------
;;; (compile-file "ccl:SAPA;utilities.lisp")
;;; (load "ccl:SAPA;utilities.fasl")
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The macros  multf
;;;              divf
;;;  act like incf and decf in Common Lisp, but use multiplication
;;;  and division rather than addition and subtraction:
;;;    (multf x a)   is  somewhat like  (setf x (* x a))
;;;    (divf  x a)         ...          (setf x (/ x a))
;;;  These are the ONLY macros used in SAPA.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(define-modify-macro multf (&optional (delta 1)) * "Like <incf>")

(define-modify-macro divf (&optional (delta 1)) / "Like <incf>") 

;-------------------------------------------------------------------------------
;;; The following constants are used with the next collection of functions:

(defconstant +sapa-10-over-log-10+ (/ 10.0d0 (log 10.0)))
(defconstant +sapa-2-pi+ (* 2 pi))
(defconstant +sapa-minus-2-pi+ (* -2 pi))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  convert-to-dB
;;;                 convert-from-dB
;;;                 log10
;;;                 power-of-2
;;;                 next-power-of-2
;;;  all take a single number and perform some sort of a ``log or exponent''
;;;  operation on it:
;;;  convert-to-dB converts the number to decibels;
;;;  convert-from-dB converts the number back from its decibel formulation;
;;;  log10 returns the log base 10 of the number
;;;  power-of-2 tests the number to see if it is a power of 2; and
;;;  next-power-of-2 returns a power of 2 greater than or equal to the number.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun convert-to-dB (num)
  "given
   [1] num (required)
       ==> a number
returns
   [1] 10 log_10(num)
---
Note: num is intended to be a positive number,
but this is not checked"
  (* (log num) +sapa-10-over-log-10+))

;;; (convert-to-dB 0.0001)  ;==> -39.999999999999986

;-------------------------------------------------------------------------------
(defun careful-convert-to-dB (num &optional (zero-mapping -100.0))
  "given
   [1] num (required)
       ==> a number
   [1] zero-mapping (required)
       ==> a number
returns
   [1] 10 log_10(num) if num > 0 or
       zero-mapping   if num <= 0"
  (if (plusp num)
    (* (log num) +sapa-10-over-log-10+)
    zero-mapping))

#|
(careful-convert-to-dB 0.0001)
;==> -39.999999999999986
(careful-convert-to-dB 0.0)
;==> -100.0
(careful-convert-to-dB 0.0 -1000.0)
;==> -1000.0
(careful-convert-to-dB 0.0 nil)
;==> nil
|#

;-------------------------------------------------------------------------------
(defun convert-from-dB (num)
  "given
   [1] num (required)
       ==> a number
returns
   [1] 10^(x/10)"
  (exp (/ num +sapa-10-over-log-10+)))

;;; (convert-from-dB 20.0)  ;==> 100.00000000000004

;-------------------------------------------------------------------------------
(defun log10 (num)
  "given
   [1] num (required)
       ==> a number
returns
   [1] log_10(num)
---
Note: shorthand for (log num 10)"
  (log num 10))

;;; (log10 100.0)  ;==> 2.0

;-------------------------------------------------------------------------------
#| (defun power-of-2 (n)
  "given
   [1] n (required)
       ==> an integer
returns
   [1] nil if n is not a power of 2 
           -- or --
       m   if n = 2^m for a positive integer m"
  (let ((answer+1 (power-of-2+1 n)))
    (if (numberp answer+1)
      (1- answer+1))))

;;; (power-of-2 1023)  ;==> nil
;;; (power-of-2 32)    ;==> 5 |#

;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
(defun a*x (a x)
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
returns
   [1] a vector of numbers obtained by multiplying
       each element of x by a
----
Note: corresponds to SSCAL in LINPACK's bla."
  (map 'vector #'(lambda (y) (* a y)) x))

#|
(setf *a* (vector 10.0 20.0 30.0))
(a*x pi *a*)  ;==> #(31.41592653589793 62.83185307179586 94.24777960769379)
*a*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun a*x! (a x &key (result x))
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
   [2] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by multiplying
       each element of x by a
----
Note: corresponds to SSCAL in LINPACK's bla."
  (map-into result #'(lambda (y) (* a y)) x))

#|
(setf *a* (vector 10.0 20.0 30.0))
(a*x! pi *a*)  ;==> #(31.41592653589793 62.83185307179586 94.24777960769379)
*a*  ;==> #(31.41592653589793 62.83185307179586 94.24777960769379)
(setf *a* (vector 10.0 20.0 30.0))
(setf *b* `(,nil ,nil ,nil))
(a*x! pi *a* :result *b*)
*a*  ;==> #(10.0 20.0 30.0)
*b*  ;==> (31.41592653589793 62.83185307179586 94.24777960769379)
|#

;-------------------------------------------------------------------------------
(defun a*x+b (a x b)
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
   [3] b (required)
       ==> a number
returns
   [1] a vector of numbers obtained by multiplying
       each element of x by a and adding b"
  (map 'vector #'(lambda (y) (+ b (* a y))) x))

#|
(setf *x* (vector  1.0  2.0  3.0))
(a*x+b pi *x* 10.0)
;==> #(13.141592653589793 16.283185307179586 19.42477796076938)
*x*  ;==> #(1.0 2.0 3.0)
|#

;-------------------------------------------------------------------------------
(defun a*x+b! (a x b &key (result x))
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
   [3] b (required)
       ==> a number
   [4] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by multiplying
       each element of x by a and adding b"
  (map-into result #'(lambda (y) (+ b (* a y))) x))

#|
(setf *x* (vector  1.0  2.0  3.0))
(a*x+b! pi *x* 10.0)
;==> #(13.141592653589793 16.283185307179586 19.42477796076938)
*x*  ;==> #(13.141592653589793 16.283185307179586 19.42477796076938)
|#

;-------------------------------------------------------------------------------
(defun a*x+y (a x y)
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
   [3] y (required)
       ==> a sequence of numbers
returns
   [1] a vector of numbers obtained by multiplying
       each element of x by a and adding the
       corresponding element of y
----
Note: corresponds to SAXPY in LINPACK's bla."
  (map 'vector #'(lambda (u v) (+ (* a u) v)) x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(a*x+y pi *x* *y*)
;==> #(13.141592653589793 26.283185307179586 39.424777960769376)
*x*  ;==> #(1.0 2.0 3.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun a*x+y! (a x y &key (result y))
  "given
   [1] a (required)
       ==> a number
   [2] x (required)
       ==> a sequence of numbers
   [3] y (required)
       ==> a sequence of numbers
   [4] result (keyword; y)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by multiplying
       each element of x by a and adding the
       corresponding element of y
----
Note: corresponds to SAXPY in LINPACK's bla."
  (map-into result #'(lambda (u v) (+ (* a u) v)) x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(a*x+y! pi *x* *y*)
;==> #(13.141592653589793 26.283185307179586 39.424777960769376)
*x*  ;==> #(1.0 2.0 3.0)
*y*  ;==> #(13.141592653589793 26.283185307179586 39.424777960769376)
|#

;-------------------------------------------------------------------------------
(defun x+y (x y)
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
returns
   [1] a vector of numbers obtained by adding
       x and y together element by element"
  (map 'vector #'+ x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x+y *x* *y*)
;==> #(11.0 22.0 33.0)
*x*  ;==> #(1.0 2.0 3.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun x+y! (x y &key (result x))
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
   [3] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by adding
       x and y together element by element"
  (map-into result #'+ x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x+y! *x* *y*)
;==> #(11.0 22.0 33.0)
*x*  ;==> #(11.0 22.0 33.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun x+b (x b)
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] b (required)
       ==> a number
returns
   [1] a vector of numbers obtained by adding
       b to each element of x"
  (map 'vector #'(lambda (y) (+ b y)) x))

#|
(setf *x* (vector  1.0  2.0  3.0))
(x+b *x* 10.0)
;==> #(11.0 12.0 13.0)
*x*  ;==> #(1.0 2.0 3.0)
|#

;-------------------------------------------------------------------------------
(defun x+b! (x b &key (result x))
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] b (required)
       ==> a number
   [3] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by adding
       b to each element of x"
  (map-into result #'(lambda (y) (+ b y)) x))

#|
(setf *x* (vector  1.0  2.0  3.0))
(x+b! *x* 10.0)
;==> #(11.0 12.0 13.0)
*x*  ;==> #(11.0 12.0 13.0)
|#

;-------------------------------------------------------------------------------
(defun x-y (x y)
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
returns
   [1] a vector of numbers obtained by subtracting
       elements of y from corresponding elements of x"
  (map 'vector #'- x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x-y *x* *y*)
;==> #(-9.0 -18.0 -27.0)
*x*  ;==> #(1.0 2.0 3.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun x-y! (x y &key (result x))
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
   [3] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by subtracting
       elements of y from corresponding elements of x"
  (map-into result #'- x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x-y! *x* *y*)
;==> #(-9.0 -18.0 -27.0)
*x*  ;==> #(-9.0 -18.0 -27.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun x*y (x y)
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
returns
   [1] a vector of numbers obtained by multiplying
       corresponding elements of x and y"
  (map 'vector #'* x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x*y *x* *y*)
;==> #(10.0 40.0 90.0)
*x*  ;==> #(1.0 2.0 3.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun x*y! (x y &key (result x))
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
   [3] result (keyword; x)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by multiplying
       corresponding elements of x and y"
  (map-into result #'* x y))

#|
(setf *x* (vector  1.0  2.0  3.0))
(setf *y* (vector 10.0 20.0 30.0))
(x*y! *x* *y*)
;==> #(10.0 40.0 90.0)
*x*  ;==> #(10.0 40.0 90.0)
*y*  ;==> #(10.0 20.0 30.0)
|#

;-------------------------------------------------------------------------------
(defun add-sequences (&rest seqs)
  "given
   [1] a arbitrary number of sequences of numbers,
       all of the same size
returns
   [1] a new sequence formed by adding the sequences
       together on an element by element basis"
  (apply #'map (type-of (car seqs)) #'+ seqs))

;;; (add-sequences '(1 2 3) '(4 5 6) '(7 8 9))  ;==>  (12 15 18)
;;; (add-sequences #(1 2 3) #(4 5 6) #(7 8 9))  ;==> #(12 15 18)

;-------------------------------------------------------------------------------
(defun multiply-sequences (&rest seqs)
  "given
   [1] a arbitrary number of sequences of numbers,
       all of the same size
returns
   [1] a new sequence formed by multiplying the sequences
       together on an element by element basis"
  (apply #'map (type-of (car seqs)) #'* seqs))

;;; (multiply-sequences '(1 2 3) '(4 5 6) '(7 8 9))  ;==>  (28 80 162)
;;; (multiply-sequences #(1 2 3) #(4 5 6) #(7 8 9))  ;==> #(28 80 162)

;-------------------------------------------------------------------------------
(defun circular-shift-sequence
       (a-seq
        &key
        (result
         (make-array (length a-seq))))
  "given
   [1] a-seq (required)
       ==> a sequence
   [2] result (keyword; vector of same length as a-seq)
       <== a sequence
returns
   [1] a `circularly' left-shifted sequence; i.e.,
       maps   x(0),   x(1), x(2), ..., x(n-2), x(n-1)
       to     x(n-1), x(0), x(1), ..., x(n-3), x(n-2)
----
Note: result can be the same sequence as result"
  (let* ((n (length a-seq))
         (n-1 (1- n))
         (temp (elt a-seq n-1)))
    (dotimes (i (1- n))
      (setf (elt result n-1)
            (elt a-seq (decf n-1))))
    (setf (elt result 0) temp)
    result))

#|
(setf *a* (vector 10.0 20.0 30.0 40.0 50.0 60.0 70.0))
(circular-shift-sequence *a* :result *a*)
;==> #(70.0 10.0 20.0 30.0 40.0 50.0 60.0)
(circular-shift-sequence *a*)
;==> #(60.0 70.0 10.0 20.0 30.0 40.0 50.0)
*a*
;==> #(70.0 10.0 20.0 30.0 40.0 50.0 60.0)
|#

;-------------------------------------------------------------------------------
;;; The name of this function is a misleading in that it can accept
;;; any type of sequences (not just vectors).



#|
(copy-vector #(1 2 3) (make-array 3))
;==> #(1 2 3)
(copy-vector #(1 2 3) (make-array 6 :initial-element 0) :start 1)
;==> #(2 3 0 0 0 0)
(copy-vector #(1 2 3) (make-array 6 :initial-element 0) :end 2)
;==> #(1 2 0 0 0 0)
|#

;-------------------------------------------------------------------------------
(defun cumulative-sums
       (the-seq
        &key
        (start 0)
        (end (length the-seq))
        (result (make-array (- end start))))
  "given
   [1] the-seq (required)
       ==> a sequence of numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sequence with cumulative sum
       of elements in specified subsequence"
  (let ((N (- end start)))
    (if (plusp N) (setf (elt result 0) (elt the-seq start)))
    (dotimes (i (1- N) result)
      (setf (elt result (1+ i)) (+ (elt result i)
                                   (elt the-seq (incf start)))))))

;;; (cumulative-sums #(10 20 30 40 55))  ;==> #(10 30 60 100 155)
;;; (cumulative-sums #(10 20 30 40 55) :start 2)  ;==> #(30 70 125)

;-------------------------------------------------------------------------------
(defun difference
       (sequence-of-numbers
        &key
        (start 0)
        (end (length sequence-of-numbers))
        (lag 1)
        (result (make-array (- end start lag))))
  "given
   [1] sequence-of-numbers (required)
       ==> a sequence of numbers
   [2] start (keyword; 0)
       ==> start index
   [3] end (keyword; length of sequence-of-numbers)
       ==> end index plus one
   [4] lag (keyword; 1)
       ==> lag for differencing
   [5] result (keyword; vector of length (- end start lag))
       <== results
return
   [1] a sequence of length (- end start lag)
       with lag-th order difference of sequence
       from index start to index end-1
---
Note: See Newton, 1988, page 31."
  (if (or (not (= start 0))
          (not (= end (length sequence-of-numbers))))
    (setf sequence-of-numbers (subseq sequence-of-numbers start end)))
  (let ((k lag))
    (dotimes (j (- end start lag) result)
      (setf (elt result j) (- (elt sequence-of-numbers k)
                              (elt sequence-of-numbers j)))
      (incf k))))

;;; (difference #(1 2 4 5 7 9 11))  ;==> #(1 2 1 2 2 2)

;-------------------------------------------------------------------------------
(defun transform-a-sequence (a-function the-seq)
  "given
   [1] a-function (required)
       ==> a function with one argument
   [2] the-seq (required)
       ==> a sequence of numbers
returns
   [1] a sequence of numbers obtained by applying
       a-function to each element of the-seq"
  (map (type-of the-seq) a-function the-seq))

;;; (transform-a-sequence #'convert-to-dB '(1 10 100)) ;==>  (0.0 10.0 20.0)
;;; (transform-a-sequence #'convert-to-dB #(1 10 100)) ;==> #(0.0 10.0 20.0)

;-------------------------------------------------------------------------------
(defun transform-a-sequence!
       (a-function
        the-seq
        &key
        (result the-seq))
  "given
   [1] a-function (required)
       ==> a function with one argument
   [2] the-seq (required)
       ==> a sequence of numbers
   [3] result (keyword; the-seq)
       <== a sequence of numbers
returns
   [1] a sequence of numbers obtained by applying
       a-function to each element of the-seq"
  (map-into result a-function the-seq))

#|
(setf *a* #(1 10 100))
(transform-a-sequence! #'convert-to-dB *a*)  ;==> #(0.0 10.0 20.0)
*a*  ;==> #(0.0 10.0 20.0)
|#

;-------------------------------------------------------------------------------
(defun linear-interpolation!
       (a-sequence
        &key
        (interpolation-predicate #'zerop))
  "given
   [1] a-sequence (required)
       <=> a sequence of numbers
   [2] interpolation-predicate (keyword; #'zerop)
       ==> a unitary predicate which, when true,
           indicates that a value in a-sequence
           is to be replaced by a linear interpolation
returns
   [1] a-sequence, modified with linear interpolates
       over specified values"
  (let ((n (length a-sequence)))
    ;;; check to make sure that the beginning and the end of the sequence
    ;;; are well-defined and that the sequence is long enough to do something
    ;;; with
    (assert (and
             (> n 2)
             (not (funcall interpolation-predicate (elt a-sequence 0)))
             (not (funcall interpolation-predicate (elt a-sequence (1- n))))))
    (do ((i-start 0)
         (i-stop (1- n))
         i-first-bad i-last-bad
         i-good-before-first-bad
         i-good-after-last-bad
         rise run)
        ((>= i-start i-stop) a-sequence)   ;;; exit test
      (block this-block
        (dotimes (i (1+ (- i-stop i-start)) (setf i-start (1+ i-stop)))
          (when (funcall interpolation-predicate
                         (elt a-sequence (+ i i-start)))
            (setf i-first-bad (+ i i-start)
                  i-good-before-first-bad (1- i-first-bad))
            (dotimes (j (- i-stop i-first-bad)
                        (error "algorithm failure!!! --- evacuate building!!!"))
              (when (not (funcall interpolation-predicate
                                  (elt a-sequence (+ j i-first-bad 1))))
                (setf i-last-bad (+ j i-first-bad)
                      i-good-after-last-bad (1+ i-last-bad)
                      rise (- (elt a-sequence i-good-after-last-bad)
                              (elt a-sequence i-good-before-first-bad))
                      run (- i-good-after-last-bad i-good-before-first-bad))
                (dotimes (k (- i-good-after-last-bad i-first-bad))
                  (setf (elt a-sequence (+ i-first-bad k))
                        (+ (elt a-sequence i-good-before-first-bad)
                           (* (/ (1+ k) run) rise))))
                (setf i-start (1+ i-good-after-last-bad))
                (return-from this-block)))))))))

#|
(setf *a* #(1 10 0 100))
(linear-interpolation! *a*)  ;==> #(1 10 55 100)
*a*  ;==> #(1 10 55 100)
(setf *b* #(1 10 nil 100))
(linear-interpolation!
 *b*
 :interpolation-predicate #'null)  ;==> #(1 10 55 100)
*b*  ;==> #(1 10 55 100)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  dot-product
;;;                 euclidean-norm
;;;                 sum-of-squares
;;;                 sum
;;;                 min-and-max-of-seq
;;;                 min-of-seq
;;;                 max-of-seq
;;;                 binary-search
;;;                 compare-seqs
;;;  all take sequence(s) of numbers as input and return various scalar(s)
;;;  of interest.  For example, the dot-product between two sequences
;;;  of the same length.
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun dot-product (x y)
  "given
   [1] x (required)
       ==> a sequence of numbers
   [2] y (required)
       ==> a sequence of numbers
returns
   [1] the dot product of x and y
----
Note: corresponds to SDOT in LINPACK's bla"
  (let ((sum 0.0))
    (dotimes (i (length x) sum)
      (incf sum (* (elt x i) (elt y i))))))

;;; (dot-product #(1 2 3) #(4 5 6))  ;==> 32.0

;-------------------------------------------------------------------------------
(defun euclidean-norm (x)
  "given
   [1] x (required)
       ==> a sequence of numbers
returns
   [1] Euclidean norm of x
----
Note: corresponds to LINPACK's SNRM2,
      but this is POOR implementation!"
  (sqrt (dot-product x x)))

;;; (euclidean-norm #(4 3))  ;==> 5.0

;-------------------------------------------------------------------------------
(defun sum-of-squares
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sum of squares of elements
       in specified subsequence"
  #-allegro
  (reduce #'+ the-seq :start start :end end :key #'(lambda (x) (* x x)))
  #+allegro
  (let ((SS 0.0)
        (j start))
    (dotimes (i (- end start) SS)
      (incf SS (expt (elt the-seq j) 2))
      (incf j)
      ))
  )

#|
(sum-of-squares #(4 3))                               ;==> 25
(sum-of-squares #(1.0 2.0 3.0))                       ;==> 14.0
(sum-of-squares #(1.0 2.0 3.0 4.0))                   ;==> 30.0
(sum-of-squares #(1.0 2.0 3.0 4.0) :end 3)            ;==> 14.0
(sum-of-squares #(1.0 2.0 3.0 4.0) :start 1 :end 3)   ;==> 13.0
|#

;-------------------------------------------------------------------------------
(defun sum
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] sum of elements in specified subsequence"
  (reduce #'+ the-seq :start start :end end))

;;; (sum #(4 3))  ;==> 7
;;; (sum #(7 8 3 4 3 10 11))  ;==> 46
;;; (sum #(7 8 3 4 3 10 11) :start 3 :end 5)  ;==> 7

;-------------------------------------------------------------------------------
(defun min-and-max-of-seq
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] minimum value in sequence
   [2] maximum value in sequence"
  (values  (reduce #'min the-seq :start start :end end)
           (reduce #'max the-seq :start start :end end)))

;;; (min-and-max-of-seq #(7 8 3 4 3 10 11))  ;==> 3 and 11

;-------------------------------------------------------------------------------
(defun min-of-seq
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] minimum value in sequence
   [2] index of minimum value"
  (let ((the-min (reduce #'min the-seq :start start :end end)))
    (values the-min (position the-min the-seq :start start :end end))))

;;; (min-of-seq #(7 8 3 4 3 10 11))  ;==> 3 and 2

;-------------------------------------------------------------------------------
(defun max-of-seq
       (the-seq
        &key
        (start 0)
        (end (length the-seq)))
  "given
   [1] the-seq (required)
       ==> a sequence of real-valued numbers
   [2] start (keyword; 0)
       ==> start index of sequence to be used
   [3] end (keyword; length of the-seq)
       ==> 1 + end index of sequence to be used
returns
   [1] maximum value in sequence
   [2] index of maximum value"
  (let ((the-max (reduce #'max the-seq :start start :end end)))
    (values the-max (position the-max the-seq :start start :end end))))

;;; (max-of-seq #(7 8 3 4 3 10 11))  ;==> 11 and 6

;-------------------------------------------------------------------------------
(defun binary-search
       (value
        ordered-seq
        &key
        (lower-test #'<=)
        (upper-test #'<=))
  "given
   [1] value (required)
       ==> a real-valued number
   [2] ordered-seq (required)
       ==> an ordered sequence of real-valued numbers
   [3] lower-test (keyword; #'<=)
       ==> a predicate of two arguments
           that defines the lower test
   [4] upper-test (keyword; #'<=)
       ==> a predicate of two arguments
           that defines the upper test
returns
   [1] if lower-test and upper-test are set
       to their default values, this function
       returns an index i such that v[i] <= value <= v[i+1],
       where 0 <= i <= n-2 (if there is no such i, nil is returned);
       if nondefault values are supplied for lower-test and upper-test,
       then the condition `v[i] <= value <= v[i+1]'
       gets modified in an obvious way
---
Note: value can be an arbitrary object, and ordered-seq
can be a list of arbitrary objects if the binary predicates
lower-test and upper-test are appropriate set"
  (binary-search-internal
   value ordered-seq 0 (1- (length ordered-seq)) lower-test upper-test))

;;; (binary-search 8 #(1 2 4 5 7 9 11))  ;==> 4

;-------------------------------------------------------------------------------
(defun compare-seqs (one-seq another-seq)
  "given
   [1] one-seq (required)
       ==> any sequence
   [2] another-seq (required)
       ==> any sequence (must be same length as one-seq)
returns
   [1] maximum absolute difference between corresponding
       elements of the two sequences
   [2] average absolute difference
---
Note: useful for comparing results that in theory
should be identical"
  (assert (= (length one-seq) (length another-seq)))
  (let ((max-abs-diff 0.0)
        (ave-abs-diff 0.0)
        (n (length one-seq))
        abs-diff)
    (dotimes (i n (values (/ ave-abs-diff n) max-abs-diff))
      (setf abs-diff (abs (- (elt one-seq i) (elt another-seq i))))
      (when (> abs-diff max-abs-diff) (setf max-abs-diff abs-diff))
      (incf ave-abs-diff abs-diff))))

#|
(compare-seqs
 #(1 2 3 4 5 6   7 8 9 10)
 #(1 2 3 4 5 6.1 7 8 9 10))
;==>
0.009999999999999964
0.09999999999999964
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  iota
;;;                 sample-from-a-function
;;;  both generate a sequence of numbers.  In the case of iota, the sequence
;;;  of numbers is equally spaced; in the case of sample-from-a-function,
;;;  the sequence is obtained by sampling a function at a specified set
;;;  of points
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun iota
       (start
        end
        &key
        (type 'vector)
        (float-p nil)
        (skip 1)
        (result (make-sequence type (1+ (/ (- end start) skip)))))
  "given
   [1] start (required)
       ==> an integer
   [2] end (required)
       ==> an integer >= start
   [3] type (keyword; 'vector)
       ==> type of sequence desired
   [4] float-p (keyword; nil)
       ==> if true, sequence consists of floats
   [5] skip (keyword; 1)
       ==> desired increment between integers in sequence
   [6] result (keyword; sequence of length (1+ (- start end)))
       ==> storage space for result
returns
   [1] a sequence of length (1+ (/ (- end start) skip))
       with numbers start, start + skip, ..., end - skip, end
       (these are stored in whatever result points to)"
  (if float-p
    (dotimes (i (1+ (/ (- end start) skip)) result)
      (setf (elt result i) (float (+ start (* i skip)))))
    (dotimes (i (1+ (/ (- end start) skip)) result)
      (setf (elt result i) (+ start (* i skip))))))

;;; (iota 7 10 :type 'vector)  ;==> #(7 8 9 10)

;-------------------------------------------------------------------------------
(defun sample-from-a-function
       (a-function
        &key
        (seq-of-x-values '())
        (x0 0.0)
        (delta-x 1.0)
        (n (length seq-of-x-values)))
  "given
   [1] a-function (required)
       ==> a function to be sampled from
   [2] seq-of-x-values (keyword; '())
       ==> sequence of values at which a-function
           is to be sampled (default '())
   [3] x0 (keyword; 0.0)
       ==> point of first value to be sampled
   [4] delta-x (keyword; 1.0)
       ==> increment between points
   [5] n (keyword; length of seq-of-x-values)
       ==> number of samples
returns
   [1] an array with the values of a-function
       at a specified set of points; and
   [2] an array with the specified set of points.
---
Note that the specified set of points either is in seq-of-x-values
 -- if it is non-nil -- or is given by
x0, x0 + delta-x, x0 + 2*delta-x, ..., x0 + (n-1)*delta-x"
  (let ((the-samples (make-array n))
        (x (if seq-of-x-values
             (if (arrayp seq-of-x-values)
               seq-of-x-values
               (make-array n :initial-contents seq-of-x-values))
             (make-array n))))
    (cond
     (seq-of-x-values
      (dotimes (i n (values the-samples x))
        (setf (aref the-samples i)
              (funcall a-function (elt seq-of-x-values i)))))
     (t
      (dotimes (i n (values the-samples x))
        (setf (aref x i) x0)
        (setf (aref the-samples i) (funcall a-function x0))
        (incf x0 delta-x))))))

#|
(sample-from-a-function #'cos
                        :delta-x (/ (* 2 pi) 4)
                        :n 5)
;==>
#(1.0 6.123031769111886E-17 -1.0 -1.836909530733566E-16 1.0)
#(0.0 1.5707963267948966 3.141592653589793 4.71238898038469 6.283185307179586)
|#

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  The functions  sign
;;;                 sampling-time->Nyquist-frequency
;;;                 Nyquist-frequency->sampling-time
;;;  are, respectively, a Lisp implementation of the Fortran SIGN function
;;;  (useful because it differs from the Common Lisp function signum);
;;;  a simple function that takes the sampling time (called delta t
;;;  in the SAPA book) and returns the Nyquist frequency; and another
;;;  simple function for going from the Nyquist frequency to the
;;;  sampling time
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
(defun sign (a1 a2)
  "implementation of Fortran SIGN function"
  (if (minusp a2)
    (- (abs a1))
    (abs a1)))

;-------------------------------------------------------------------------------
(defun sampling-time->Nyquist-frequency (sampling-time)
  "given a sampling time, returns the associated Nyquist frequency
---
Note: see page 98 of the SAPA book"
  (/ (* 2 sampling-time)))

;-------------------------------------------------------------------------------
(defun Nyquist-frequency->sampling-time (Nyquist-frequency)
  "given a Nyquist frequency, returns the associated sampling-time
---
Note: see page 98 of the SAPA book"
  (/ (* 2 Nyquist-frequency)))

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;;;  Everything below here consists of internal symbols in the SAPA package
;;;  and should be regarded as "dirty laundry" ...
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;;; NOTE: used by binary-search.  This arrangement was done because the way
;;;       that this function is recursively called makes the lambda list
;;;       bulky as a user interface.  Originally, we had &optional instead of
;;;       &key in binary-search, and there was no binary-search-internal.
;;;       There is probably a slick way of getting around this is Lisp,
;;;       but I (dbp) don't know what it is as of 7/29/91!
;;; NOTE: `end' here is one less than the usual Lisp parameter of that name
(defun binary-search-internal
       (value
        ordered-seq
        start
        end
        lower-test
        upper-test)
  (cond
   ((= (- end start) 1)
    (if (and (funcall lower-test (elt ordered-seq start) value)
             (funcall upper-test value (elt ordered-seq end)))
      start))   ;returns nil if termination condition fails
   (t
    (let ((middle (truncate (+ start end) 2)))
      (if (funcall upper-test value (elt ordered-seq middle))
        (binary-search-internal
         value ordered-seq start middle lower-test upper-test)
        (binary-search-internal
         value ordered-seq middle end lower-test upper-test)
        )))))
