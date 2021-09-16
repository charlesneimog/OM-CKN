;;; -*- Mode: Lisp; Package: CLOUD.SP -*-

;;;  (c) copyright 2005-2009 by
;;;           Martin Raspaud <martin.raspaud@gmail.com>

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file provides implementations of various windowing functions.

(in-package :om-ckn)

(deftype window ()
  `(member :rectangular 
	   :hann
	   :hanning
	   :hamming
	   :blackman 
	   :barlett
	   :triangular))
	   
(defun rectangular-value (x)
  (if (and (<= 0 x) (< x 1))
      1.0d0
      0.0d0))

(defun rectangular (N)
  "Computes the N size rectangular window."
  (make-array N :initial-element 1.0d0))

(defun hann-value (x)
  (if (<= 0 x 1)
      (* 0.5d0 (- 1d0 (cos (* 2d0 pi x))))
      0.0d0))

(defun hann (N)
  "Computes the N size periodic Hann window."
  (let ((w (make-array N)))
    (loop for i from 0 to (1- N)
	  do (setf (aref w i) (hann-value (/ (1+ i) N))))
    w))

(defun hamming-value (x)
  (if (<= 0 x 1)
      (- 0.54d0 (* 0.46d0 (cos (* 2d0 pi x))))
      0.0d0))

(defun hamming (N)
  "Computes the N size Hamming window."
  (let ((w (make-array N)))
    (loop for i from 0 to (1- N)
	  do (setf (aref w i) (hamming-value (/ i N))))
    w))

(defun blackman-value (x)
  (if (<= 0 x 1)
      (+ 0.42d0
	 (* -0.5d0 (cos (* 2d0 pi x)))
	 (* -0.08d0 (cos (* 4d0 pi x))))
      0.0d0))
  
(defun blackman (N)
  "Computes the N size Blackman window."
  (let ((w (make-array N)))
    (loop for i from 0 to (1- N)
	  do (setf (aref w i) (blackman-value (/ i N))))
    w))

(defun barlett-value (x)
  (if (<= 0 x 1)
      (if (<= x (/ 1 2))
	  (* 2.0d0 x)
	  (- 2.0d0 (* 2.0d0 x)))
      0.0d0))

(defun barlett (N)
  "Computes the N size Blackman window."
  (let ((w (make-array N)))
    (loop for i from 0 to (1- N)
	  do (setf (aref w i) (barlett-value (/ i N))))
    w))

(defun triangular (N)
  (barlett N))

(defun triangular-value (x)
  (barlett-value x))

(defun hanning (N)
  (hann N))

(defun hanning-value (x)
  (hann-value x))


(defun make-window (type size)
  "Creates a new window."
  (case type
    ((:hann :hanning) (hann size))
    (:hamming (hamming size))
    (:blackman (blackman size))
    ((:barlett :triangular) (barlett size))
    ((:rectangular t) (rectangular size))))

(defun window-value (x type)
  "Computes the value of a given window-type at position x."
  (case type
    ((:hann :hanning) (hann-value x))
    (:hamming (hamming-value x))
    (:blackman (blackman-value x))
    ((:barlett :triangular) (barlett-value x))
    ((:rectangular t) (rectangular-value x))))


(let ((size 0)
      (defined-type :hann)
      (window))
  
  (defun n-fill-window (in-array type)
    "Fill a given array destructively with a 'type window."
    (unless (and (= size (length in-array))
		 (eq type defined-type))
      (setf size (length in-array)
	    defined-type type
	    window (make-window defined-type size)))
    (iterate (for i index-of-vector in-array)
	     (for j in-vector window)
	     (setf (aref in-array i) j))
    in-array)

  (defun n-apply-window (in-array type)
    "Multiplies destructively an array by a 'type window."
    (unless (and (= size (length in-array))
		 (eq type defined-type))
      (setf size (length in-array)
	    defined-type type
	    window (make-window defined-type size)))
    (iterate (for i index-of-vector in-array)
	     (for j in-vector window)
	     (setf (aref in-array i) (* (aref in-array i) 
					j)))
    in-array)

  (defun apply-window (in-array type)
    "Multiplies an array by a 'type window."
    (unless (and (= size (length in-array))
		 (eq type defined-type))
      (setf size (length in-array)
	    defined-type type
	    window (make-window defined-type size)))
    (let ((out-array (make-array size
		      :element-type (array-element-type in-array))))
      (om-ckn::iterate (om-ckn::for i in-vector window)
	       (om-ckn::for j in-vector in-array)
	       (om-ckn::for k index-of-vector out-array)
	     (setf (aref out-array k) (* i j)))
      out-array))


  (defun apply-window! (in-array out-array type)
    "Multiplies an array by a 'type window into another array."
    (unless (and (= size (length in-array))
		 (eq type defined-type))
      (setf size (length in-array)
	    defined-type type
	    window (make-window defined-type size)))
    (iterate (for i index-of-vector in-array)
	     (for j in-vector window)
	     (setf (aref out-array i) (* (aref in-array i)
					 j)))
    out-array))

; ==================================================

(compile 'rectangular-value)
(compile 'rectangular)
(compile 'hann-value)
(compile 'hann)
(compile 'hamming-value)
(compile 'hamming)
(compile 'blackman-value)
(compile 'blackman)
(compile 'barlett-value)
(compile 'barlett)
(compile 'triangular)
(compile 'triangular-value)
(compile 'hanning)
(compile 'hanning-value)
(compile 'make-window)
(compile 'window-value)
