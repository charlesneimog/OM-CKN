(in-package :om)

; ===========================================

(defmethod! neimog-fft ((samples list))
:indoc '("It calculates the FFT using a list of numbers.")
:icon 'omckn-sound
:outdoc '("A list of complex-numbers")
:doc "This is a simple implementation of the FFT using Lisp. This is the exact copy of the patch"

(if (> (list-depth samples) 1)
    (mapcar 'neimog-fft samples) ;; If it is a list of lists, then we apply the FFT to each list
    (let ((n (length samples)))
        (if (< n 2)
            samples
            (let* (
                    (even (neimog-fft (loop :for i :below n :by 2 :collect (elt samples i))))
                    (odd (neimog-fft (loop :for i :from 1 :below n :by 2 :collect (elt samples i))))
                    (DFT (loop :for i :from 0 :below (/ n 2) :collect (* (nth i odd) (exp (/ (* #C(0 -2) pi i) n)))))
                    (new-even (loop :for i :below (/ n 2) :collect (nth i even))))
                    (loop   :with left := nil
                            :with right := nil
                            :for i :below (/ n 2)
                            :do (setf left (cons (+ (nth i new-even) (nth i DFT)) left))
                            :do (setf right (cons (- (nth i new-even) (nth i DFT)) right))
                            :finally (return (append (reverse left) (reverse right)))))))))

(compile 'neimog-fft)

; ===========================================

(defmethod! neimog-ifft ((samples list))
:indoc ' ("It calculates the IFFT using a list of complex numbers.")
:icon 'omckn-sound
:outdoc '("A list of complex-numbers")
:doc "This is a simple implementation of the IFFT using Lisp."
(if (> (list-depth samples) 1)
    (mapcar #'neimog-ifft samples) ;; If it is a list of lists, then we apply the FFT to each list
    (let* (
            (change-imag-signal (mapcar 'conjugate samples))
            (fft (neimog-fft change-imag-signal))
            (n (length samples)))
            (mapcar (lambda (x) (/ (conjugate x) n)) fft))))

(compile 'neimog-ifft)

; ===========================================
(defmethod! neimog-rifft ((samples list))
:indoc ' ("It calculates the real part of IFFT using a list of complex numbers.")
:icon 'omckn-sound
:outdoc '("A list of complex-numbers")
:doc "It returns just the real part of the IFFT using the IFFT function."
(if (> (list-depth samples) 1)
    (mapcar #'neimog-rifft samples) ;; If it is a list of lists, then we apply the FFT to each list
    (let* (
            (change-imag-signal (mapcar 'conjugate samples))
            (fft (neimog-fft change-imag-signal))
            (n (length samples)))
            (mapcar (lambda (x) (realpart (/ (conjugate x) n))) fft))))

(compile 'neimog-rifft)




               