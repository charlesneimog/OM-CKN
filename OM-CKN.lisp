;;            OM-CKN
;;
;;      by Charles K. Neimog 
;;  Universidade de São Paulo (2021)
           
(in-package :om)


(defun lib-src-file (file)
  (merge-pathnames file (om-make-pathname :directory *load-pathname*)))

(mapcar #'(lambda (file) (compile&load (lib-src-file file) t t))
      '(             
      "Sources/Sapa-FFT/fft"
      "Sources/Sapa-FFT/utilities"  
      "Sources/quicklisp-packages/package" 
      "Sources/quicklisp-packages/iterate"
      "Sources/Cloud/FFT-window"
      "Sources/Samples/samples-control-om6"
      "Sources/Samples/ircam-instruments-om6"
      "Sources/sinusoidal-modeling"
      "Sources/om6-methods"
      "Sources/utilities"
      "Sources/stolen-functions"
      
            ))


(fill-library  '(("FFT" nil (ckn-fft-instance) (do-fft fft->amplitude fft->phrase sound->bytes) nil)
                  ("SDIF" nil nil (sdif->list sdif-envelope save-spear-sdif))
               
                ))


(print 
 "
                                              OM-CKN   "
)
                    

