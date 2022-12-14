;;            OM-CKN
;;
;;      by Charles K. Neimog 
;; University of São Paulo (2021-2022)
           

(in-package :om)

(defun lib-src-file (file)
  (merge-pathnames file (om-make-pathname :directory *load-pathname*)))

(mapcar #'(lambda (file) (compile&load (lib-src-file file) t t))
      '(
            "Sources/Sapa-FFT/fft"
            "Sources/Klingbeil/partial-tracking"
            "Sources/quicklisp-packages/package" 
            "Sources/quicklisp-packages/iterate"
            "Sources/Cloud/FFT-window"
            "Sources/om-methods" 
      	"Sources/Utilities/utilities"
            "Sources/Samples/FULLSOL"
            "Sources/Sapa-FFT/utilities" 
            "Sources/Samples/samples-control"
            ;"Sources/Samples/VST2 and VST3"
            ;"Sources/Utilities/sinusoidal-modeling"
            "Sources/Utilities/stolen-functions"

            "Sources/Score/score"
            ;"Sources/Score/functions"
            ;"Sources/Score/om2music21"

            "Sources/Samples/OrchideaSOL2020"
            ;"Sources/Python/py"
            ;"Sources/Python/vamp"
            ;"Sources/loristrck/loristrck"
            "Sources/Neimog-FFT/fft"
            ))

(om::fill-library '((nil nil nil () nil)))


(print 
 "
 
                  OM-CKN

      by Charles K. Neimog | charlesneimog.com   
      University of São Paulo (2021-2)
"
)
                    

