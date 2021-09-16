
(in-package :om)

;=====================================================================

(defun partial-track (fft->max-coll nota index frequency-threshold)

      (let* (

            (action2 (lambda (fft-coll) (if     
                                    (om< (om-abs (om- nota (f->mc (first fft-coll)))) frequency-threshold) ; 50 é o limite  
                                    (x-append index fft-coll) ; 0 é o index
                                    nil)))
            (action3 (loop :for loop-fft->max-coll :in fft->max-coll :collect 
                        (let* (
                              (action1-1 (mapcar action2 (flat (cdr loop-fft->max-coll) 1))))
                              (remove nil (x-append (first loop-fft->max-coll) action1-1))))))

            (loop :for loop-fft->max-coll :in fft->max-coll
                  :for loop-action3 :in action3 :collect
                        (let* (
                              (action4-1 (remove (last-n (flat (cdr loop-action3)) 3) (flat (cdr loop-fft->max-coll) 1) :test #'equal))
                              (action4-2 (first loop-fft->max-coll)))
                  (list action4-2 action4-1)))
      action3))

; ============================================================

(defun exclude-partial-track (fft->max-coll nota frequency-threshold)

(let* (
      (action1 (sort-list (remove nil (flat (remove-dup (f->mc (mapcar (lambda (x) (first (mat-trans (flat (cdr x) 1)))) fft->max-coll)) 'eq 1)))))
      (action2 (lambda (fft-coll) (if 
                                          (om< (om-abs (om- nota (f->mc (first fft-coll)))) frequency-threshold) ; 50 é o limite  
                                          (x-append (position nota action1) fft-coll) ; 0 é o index
                                          nil)))
      (action3 (loop :for loop-fft->max-coll :in fft->max-coll :collect 
                      (let* (
                            (action1-1 (mapcar action2 (flat (cdr loop-fft->max-coll) 1))))
                        (remove nil (x-append (first loop-fft->max-coll) action1-1)))))

      (action4 
      (loop :for loop-fft->max-coll :in fft->max-coll
            :for loop-action3 :in action3 :collect
            (let* (
            (action4-1 (remove (last-n (flat (cdr loop-action3)) 3) (flat (cdr loop-fft->max-coll) 1) :test #'equal))
            (action4-2 (first loop-fft->max-coll)))
            (list action4-2 action4-1)))))
action4))

;=====================================================================

(defun all-freq-fft (fft->max-coll) 
        (sort-list (flat (remove-dup 
              (flat (om::f->mc (mapcar (lambda (x) (first (om::mat-trans (flat (cdr x) 1)))) fft->max-coll)) 1) 'eq 1))))

;=====================================================================

(compile 'partial-track)
(compile 'exclude-partial-track)
(compile 'all-freq-fft)