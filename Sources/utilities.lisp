(in-package :om)



;==================================== IMPORT NUMBERS ===================

(defconstant CKN-euler-number (exp 1))


(defconstant CKN-pi  (coerce pi 'single-float))

;; SAMPLES AND DAW =================

(defun search-inside-some-folder (folder extension)                                                                         
      (let* (
            (thepath folder)
            (thefilelist (om-directory thepath 
                              :type extension
                              :directories t 
                              :files t 
                              :resolve-aliases nil 
                              :hidden-files nil))
            (more-folders? (mapcar (lambda (x) (if 
                                                      (system::directory-pathname-p x)
                                                      (search-inside-some-folder x extension)
                                                      x)) thefilelist)))
            more-folders?))


; =============

(defun search-plugins (extension)

      (let* (
            (thepath (get-pref-value :externals :plugins))
            (thefilelist (om-directory thepath 
                                                :type extension
                                                :directories t 
                                                :files t 
                                                :resolve-aliases nil 
                                                :hidden-files nil))

            (action1 
                  (loop :for loop-files :in thefilelist 
                        :collect (if 
                                          (system::directory-pathname-p loop-files)
                                          (search-inside-some-folder loop-files extension)
                                          loop-files))))
            (remove nil (flat action1))))

;==============================================================

(defun names-to-mix (in1)
(reduce (lambda (z y) (string+ z y))
          (flat (loop for x :in in1 :collect  
                      (flat (om::x-append " -v 1 "(list->string-fun (list (string+ (namestring x) " "))) " "))))))



;==============================================================


;==================================== FUNCTIONS ===================

(defun remove-nth-element-fun (n list)
  "Remove the nth element of a list."
(if (not n)
list  
(if (> (1- n) (length list))
      list
    (append (subseq list 0 (1- n))
            (subseq list n)))))

;; ============

(defun create-pure-tone (frequency time)
      "It will create a pure-tone using complex coordenates."
(let* (
      (angle (om::om* (* -2 (coerce pi 'single-float) frequency) time))
      (cos-angle (mapcar (lambda (X) (cos x)) angle))
      (sin-angle (mapcar (lambda (X) (sin x)) angle)))
  (om::om+ cos-angle (om::om* (sqrt -1) sin-angle))))


;==================================== Python Functions ===========

(defun lisp-list_2_python-list (list)
      "Transform a list in lisp to a list in Python."
(let* (
      (list2string (mapcar (lambda (x) (ckn-int2string x)) list)))
      (loop :for x :in list2string :collect (string+ x ", "))))

;; ============
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
plt.figure(figsize=(20, 10), dpi=300)
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

;; =========================================

(defmethod* ckn-save-as-text ((self t) &optional (path "data") (type "txt"))
  :icon 908
  :initvals '(nil "data")
  :indoc '("data (list, BPF, or TextBuffer)" "a file location")
  :doc "Saves the data from <self> as a text file in <path>."
  (let ((file (cond
               ((null path) (om::om-choose-new-file-dialog :directory (def-save-directory) :types '("Text files" "*.txt" "All files" "*.*")))
               ((pathnamep path) path)
               ((stringp path) (if (pathname-directory path) (pathname (string+ path type)) (outfile path :type type))))))
    (if file
        (progn
          (with-open-file (out file :direction :output :if-does-not-exist :create :if-exists :supersede)
            (write-data self out))
          file)
      (om::om-abort))))

;; =========================================

(defmethod write-data ((self t) out)
  (format out "~A~%" self))

;==================================== 
(defun list->string-fun (ckn-list)
  (when ckn-list
    (concatenate 'string 
                 (write-to-string (car ckn-list)) (list->string-fun (cdr ckn-list)))))

;====================================

(defun list-dimensions (list depth)
  (loop :repeat depth
        :collect (length list)
        :do (setf list (car list))))

;=====================================

(defun list-to-array-fun (list depth)
  (make-array (list-dimensions list depth)
              :initial-contents list))

;=====================================

(defun array-to-list-fun (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop :for j :below (nth n dimensions)
                     :do (setf (nth n indices) j)
                     :collect (if (= n depth)
                                (apply #'aref array indices)
                                (recurse (1+ n))))))
      (recurse 0))))

;=====================================

(defun by-N-fun (list n fun) 
  (loop for tail on list by (lambda (l) (nthcdr n l)) 
    :collect (funcall fun (subseq tail 0 (min (length tail) n)))))

;=====================================

(defun fft->phrase-fun (fft)
    (let* (   
        (fft-list (array-to-list-fun fft))
        (i-n (mapcar (lambda (x) (imagpart x)) fft-list))
        (r-n (mapcar (lambda (y) (realpart y)) fft-list)))
    (mapcar (lambda (x y) (atan x y)) i-n r-n)))

;=====================================

(defun fft->amplitude-fun (fft)
 (let* (   
       (fft-list (array-to-list-fun fft))
       (i-n (mapcar (lambda (x) (imagpart x)) fft-list)) ;; FILTRA A PARTE IMAGINÁRIA DO FFT
       (r-n (mapcar (lambda (y) (realpart y)) fft-list))) ;; FILTRA A PARTE REAL DO FFT
       
   (mapcar (lambda (x y) (sqrt (om::om+ (om::om^ x 2) (om::om^ y 2)))) i-n r-n)))

;======================================

(deftype complex-sample-array (&optional size)
  `(simple-array complex-sample (,size)))

;======================================
(defun energy (x)
  (let ((acc 0d0))
    (declare (type double-float acc))
    (map nil (lambda (x)
               (let ((r (realpart x))
                     (i (imagpart x)))
                 (incf acc (+ (* r r) (* i i))))) x) acc))

;======================================

(defvar *optimization-policy* '(optimize speed (safety 0))) ;; Otimização de processamento

;=====================================

(defun real-samplify (vec &optional (size (length vec)))
  (etypecase vec
    (real-sample-array vec)
    ((simple-array single-float 1)
     (map-into (make-array size
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))
    (sequence
     (map-into (make-array size
                           :element-type 'real-sample)
               (lambda (x)
                 (coerce x 'real-sample))
               vec))))

;=====================================

(deftype real-sample-array (&optional size)
  `(simple-array real-sample (,size)))

; ============================

(deftype real-sample ()
  'double-float)

; ============================

(defun lb (n)
  (integer-length (1- n)))

; ============================

(defun half-fun (in-array)
  (make-array (/ (length in-array) 2)
	      :element-type (array-element-type in-array)
	      :displaced-to in-array))

(compile 'half-fun)
; ============================

(defun sound-window (sound-bytes-window window hop-size windows-type &optional result)

(let* (
      (action1 (if (equal nil windows-type) ;; checa se o(a) usuario selecionou ou nao um tipo de janela para a analise.

                (list-to-array (first-n sound-bytes-window window) 1) ; se nao selecionou, o algoritmo simpleste 
                                                                      ; organiza os samples de acordo com a hop-size e
                                                                      ; os transforma em arrays (estruturas de dados que 
                                                                      ; agilizam o processamento).

                ;; First-n seleciona os primeiros n bytes de todos os bytes do sample em analise. 
                ;; Sendo que n e o hopsize.
                ;; O trecho acima transforma os bytes de um sample em arrays em list (list-to-array).

                (om-ckn::apply-window (list-to-array (first-n sound-bytes-window window) 1) windows-type)))  
                                                           ; se o(a) usuario(a) selecionou, organizamos os samples
                                                           ; e entao aplicamos o calculo da janela atraves da funcao 
                                                           ; om-ckn::apply-window

;; ================================================================================================

      (action2 (last-n sound-bytes-window (let* ((number (- (length sound-bytes-window) hop-size)))
                                            (if (plusp number) number 1)))))

                ;; Esta acao exclui os samples que ja passaram pelo organizacao e a transformacao para arrays.
                ;; Em seguida checa se ha samples suficientes para fazer o calculo da ACTION1 novamente.

;; ================================================================================================


(if (< (length (remove nil action2)) window) ;; Esta acao checa o resultado da ACTION2 e ve se o algoritmo deve finalizar o calculo
                                             ;; ou fazer a ACTION1 e ACTION2 novamente. 

;; ============
    (reverse (om::x-append (list action1) result)) ;; Caso nao ha mais samples para organizar e fazer os calculos, aqui recolhemos todos
                                                   ;; os resultados e direcionamos para o output. 
;; ============

    (setf sound-bytes-window (sound-window action2 window hop-size windows-type (push action1 result))))))
                                                   ;; Caso seja necessario fazer o calculo novamente aqui salvamos os resultados na 
                                                   ;; na variavel RESULT e entao fazemos o calculo novamente. 

(compile 'sound-window)
;=====================================

(defun sound-window-list (sound-bytes-window window hop-size &optional result)

(let* (
      (action1 (first-n sound-bytes-window window))
      (action2 (last-n sound-bytes-window (let* ((number (- (length sound-bytes-window) hop-size)))
                                            (if (plusp number) number 1)))))
(if (< (length (remove nil action2)) window) 
    (om::x-append result (list action1)) 
  (setf sound-bytes-window (sound-window-list action2 window hop-size (push action1 result))))))

(compile 'sound-window-list)
;=====================================

(defun loop-in-parts (sound-bytes-window window hop-size &optional result)

(if (< (length sound-bytes-window) hop-size)
(list sound-bytes-window)

(let* (
      (action1 (first-n sound-bytes-window window))
      (action2 (let* ((number (- (length sound-bytes-window) hop-size)))
                      (if (plusp number)      
                        (last-n sound-bytes-window number)
                        sound-bytes-window))))
(if (or (< (length (remove nil action2)) window) (equal action1 action2))
    (if (equal action1 action2) 
        (reverse (om::x-append (list action2) (list action1) result)) 
        (reverse (om::x-append (list action1) result)))
  (setf sound-bytes-window (loop-in-parts action2 window hop-size (push action1 result)))))))

(compile 'loop-in-parts)
;===================================== SDIF TRACKING ============
(defun prepare-fft2sdif (list-fft-instance)
(let* (
            (all-frequencias (mapcar (lambda (x) (frequencias x)) list-fft-instance))
;;          (all-frequencias (mc->f (f->mc (mapcar (lambda (x) (frequencias x)) list-fft-instance)))) 
            (all-amplitudes (mapcar (lambda (x) (amplitudes x)) list-fft-instance))
            (all-phrase (mapcar (lambda (x) (phrase x)) list-fft-instance))
            (all-peaks (mapcar (lambda (a b c) (list a b c)) all-frequencias all-amplitudes all-phrase)))
            (mapcar (lambda (x) (om::mat-trans x)) all-peaks)))

(compile 'prepare-fft2sdif)
;=====================================

(defun index-of-individual-partial-tranking (frequencia cents spectro)
      (let* (
            (inside-remove-if-lambda (lambda 
                                    (remove-if-x)
                                          (not (and 
                                                (om::om> frequencia (om::om- remove-if-x cents)) 
                                                (om::om< frequencia (om::om+ remove-if-x cents))))))
            (remove-if-lambda (lambda (x) (remove-if inside-remove-if-lambda x)))
            (partial-tracking (mapcar remove-if-lambda spectro))
            (filtro 
                  (loop :for inside-partial-tracking :in partial-tracking 
                        :collect 
                              (if 
                                    (om::om= (length inside-partial-tracking) 1)
                                    inside-partial-tracking 
                                    (let* (
                                          (math-frequencia-closest (mapcar (lambda (x) (om::om- frequencia x)) inside-partial-tracking))
                                          (frequencia-closest (om::list-min math-frequencia-closest))
                                          (position-closest-frequency (ckn-position math-frequencia-closest frequencia-closest)))
                                          (choose inside-partial-tracking position-closest-frequency)))))
            (took-the-peak (mapcar (lambda (x y) (ckn-position x y)) spectro (flat filtro))))
      took-the-peak))

(compile 'index-of-individual-partial-tranking)
;=====================================

(defun tracking-partial-from-spectro (individual-partial-tranking spectro-total index)
"Remove os parciais que ja foram rastreados."                  
(let* (
      (organize (mapcar (lambda (x y) (choose x y)) spectro-total individual-partial-tranking))
      (add-index-to-partial (mapcar (lambda (x y) (if (not x) nil (om::flat (om::x-append y x)))) organize index)))
      add-index-to-partial))

(compile 'tracking-partial-from-spectro)
;=====================================
(defun remove-tracking-partial-from-spectro (index-of-individual-partial-tranking prepare-fft)
(mapcar (lambda (x y) (remove-nth-element-fun x y)) (om::flat index-of-individual-partial-tranking) prepare-fft))

(compile 'remove-tracking-partial-from-spectro)
;=====================================
(defun fft-freqs (fft-list)

(mapcar (lambda (x) (frequencias x)) fft-list))

(compile 'fft-freqs)
;=====================================

(defun frobnicate (list)
  (loop with counter = 1
        for (prev next) on list
        when (and next (null prev))
          do (incf counter)
        when prev
          collect counter
        else
          collect prev))

(compile 'frobnicate)
;=====================================
(defun freq-cents-wrong-inside (list-fft-instance)
(mapcar (lambda (x) (mapcar (lambda (y) (first y)) x)) list-fft-instance))

(compile 'freq-cents-wrong-inside)
;;(mc->f (f->mc (mapcar (lambda (x) (mapcar (lambda (y) (first y)) x)) list-fft-instance))))
;======================================

(defun partial-tracking-fun (prepare-fft todas-frequencias cents-threshold index-inicial &optional result)

(let* (
      (frequencia-da-vez  (first todas-frequencias))
      (sem-a-da-vez (cdr todas-frequencias))
      (partial-tracking   (index-of-individual-partial-tranking frequencia-da-vez cents-threshold (freq-cents-wrong-inside prepare-fft)))
      (remocao-do-parcial (remove-tracking-partial-from-spectro partial-tracking prepare-fft))
      (como-farei-o-index (1+ index-inicial))
      (traking (tracking-partial-from-spectro partial-tracking prepare-fft 
                                              (mapcar (lambda (x) (list x)) (om::om+ como-farei-o-index (frobnicate (flat partial-tracking))))))
      (last-index (let* (
                              (action1 (caar (last (remove nil traking)))))
                              (if (not action1) como-farei-o-index action1))))
      (if 
            (not sem-a-da-vez)
            (om::x-append (list traking) result)
            (setf prepare-fft 
                  (partial-tracking-fun remocao-do-parcial sem-a-da-vez cents-threshold last-index (push traking result))))))

(compile 'partial-tracking-fun)
;======================================
(defun fft->sdif-fun (fft-instances-list freq-threshold db)
(let* (
      (sin-model (fft->sin-model fft-instances-list db))
      (tempo (ms->sec (mapcar (lambda (x) (ckn-tempo x)) sin-model)))
      (all-freqs (sort-list (remove-dup (om::om-round (remove nil (flat (fft-freqs sin-model)))) 'eq 1)))
      (freqs2sdif (prepare-fft2sdif sin-model))
      (partial-tracking (mapcar (lambda (x) (remove nil x)) (mat-trans (partial-tracking-fun freqs2sdif all-freqs freq-threshold 0))))
      (2matrix (mapcar (lambda (x) (make-value 'sdifmatrix (list (list :matrixtype "1TRC") (list :data (mat-trans x))))) partial-tracking))
      (make-sdif 
            (loop :for tempo-loop :in tempo    
                  :for matrix-loop :in 2matrix
                  :collect 
            (make-value 'sdifframe (list (list :frametype "1TRC") (list :ftime tempo-loop) (list :streamid 0) (list :lmatrix matrix-loop))))))
      (make-value 'ckn-sdif (list (list :ckn-matrix make-sdif)))))
      
(compile 'fft->sdif-fun)
;=====================================
(defun ckn-cmd-line (str)
  (oa::om-command-line str))

;=====================================

(defun sound->bytes-smart (self)

(let* (
        (pontos
          (audio-io::om-get-sound-buffer (filename self) :float t))
        (sound-markers (om::om-round (sec->samples (markers self) 44100)))
        (numbers (if (not (equal (length (markers self)) 2))
                      (loop :for i :from 0 :to (om::om-sound-n-samples self) :by 1 :collect (om::om-read-ptr pontos i :float))
                      (loop :for i :from (first sound-markers) :to (second sound-markers) :by 1 :collect (om::om-read-ptr pontos i :float)))))
(make-instance 'sound-bytes :bytes numbers)))

;=====================================

(defun sound->bytes-fun (self)
(format nil "Read bytes of the sound.")
(with-audio-buffer (b self)
          (let* ((sound-markers (ms->samples (markers self) (sample-rate self)))
                (2-markers (equal (length sound-markers) 2))
                (channel-ptr (om::om-read-ptr (om::om-sound-buffer-ptr b) (1- (n-channels self)) :pointer)))
      (if 2-markers
          (loop :for i :from (first sound-markers) :to (second sound-markers)
                :by 1 :collect (om::om-read-ptr channel-ptr i :float))
          (loop :for i :from 0 :to (n-samples self) :by 1 :collect (om::om-read-ptr channel-ptr i :float))))))

;=====================================

(defun sound->bytes-om (self)

  (let* ((pontos
          (audio-io::om-get-sound-buffer (filename self) :float t)))
(loop :for i :from 0 :to (om::om-sound-n-samples self) :by 1 :collect (om::om-read-ptr pontos i :float))))


;=====================================

(defun sound->bytes-om-class (self)

  (let* ((pontos
          (audio-io::om-get-sound-buffer (filename self) :float t))
(numbers (loop :for i :from 0 :to (om::om-sound-n-samples self) :by 1 :collect (om::om-read-ptr pontos i :float))))
(make-instance 'sound-bytes :bytes numbers)))


; =====================================

(defun bytes->sound-fun (onda quantos-canais qual-canal)

(let* ((nbsamples (length onda)))
      (with-sound-output (mysound :nch quantos-canais :size nbsamples :sr 44100 :type :float)
            (loop :for x :from 0 :to (1- nbsamples)
                  :for onda-loop :in onda
                  :do
            (write-in-sound mysound (om::om- qual-canal 1) x onda-loop)))))

; =====================================

(defun ITD-Sound (sound place-of-sound)
(let* (
      (s-bytes (bytes (sound->bytes sound)))
      (channel-1 (bytes->sound-fun (om::x-append (om::repeat-n 0.0 (first place-of-sound)) s-bytes) 2 1))
      (channel-2 (bytes->sound-fun (om::x-append (om::repeat-n 0.0 (second place-of-sound)) s-bytes) 2 2)))
      (om::sound-mix channel-1 channel-2)))

 ; ============================ OM-SYNTH ========================================================

(defun do-senoide (dur freq gain envelope)

  (let* (
         (sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop :for f :in (list (car freqs)) :collect (/ f sr)))
         (glissando (loop :for z :in (nth 2 (multiple-value-list (om::om-sample freqs nbsamples))) :collect (/ z sr)))
         (sampled-envelope envelope))

   (loop :for x :from 0 :to (1- nbsamples)
         :for y-list := (make-list (length steps) :initial-element 0) :then (om+ y-list (nth x glissando))
         :for amp :in sampled-envelope
         :collect
            (om::om* 
             (om::om* gain amp)
             (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor (car y-list))))))))))

;=========================
;; This is a code stolen from Jean Bresson OM-Sharp

(defun synth (dur freq gain envelope)

  (let* ((sr 44100)
         (nbsamples (round (* dur sr)))
         (freqs (list! freq))
         (steps (loop for f in freqs collect (/ f sr)))
         (sampled-envelope (om-scale (nth 2 (multiple-value-list (om-sample envelope nbsamples))) 0.0 1.0)))

    (with-sound-output (mysound :nch 1 :size nbsamples :sr 44100 :type :float)

      (loop for x from 0 to (1- nbsamples)
            for y-list = (make-list (length steps) :initial-element 0) then (om+ y-list steps)
            for amp in sampled-envelope
            do
            (write-in-sound mysound 0 x
                            (* gain amp
                               (apply '+
                                      (loop for y in y-list collect
                                            (sin (* 2 (coerce pi 'single-float) (cadr (multiple-value-list (floor y))))))

                                      ))
                            )
            )
      )))
;=========================


(defun fft-multiple-thread (fft-chunks mail-box nomes chunks sample-rate hop-size)

(let* ()

(loop 
    :for ckn-fft-chunks :in fft-chunks 
    :for create-mailbox :in mail-box
    :for names-process :in nomes
    :for chunks-number :in chunks
    :do 
        (mp:process-run-function names-process
                 () 
                  (lambda (x w z) (mp:mailbox-send w 
                      (let* (
                              (fft (half-fun (sapa-fft! x)))
                              (amp (fft->amplitude fft))
                              (phrase (fft->phrase fft)))
                             ;(length-amp (length amp))) ;; colocar a sequencia fft                                                               
                              (make-instance 'ckn-fft-instance 
                                 :ckn-complex-numbers (make-instance 'fft-complex-numbers :complex-numbers fft)
                                 :fft-window (* 2 (length amp))
                                 :fft-chunks z
                                 :ckn-hop-size hop-size
                                 :sound-sample-rate sample-rate
                                 :ckn-tempo (om::sec->ms (om::samples->sec (om::om* hop-size (1- z)) 44100))
                                 :amplitudes amp
                                 :phrase phrase
                                 :frequencias nil))))
                   ckn-fft-chunks create-mailbox chunks-number))
(loop-until-finish-process mail-box)
(mapcar (lambda (x) (mp:mailbox-peek x)) mail-box)))

;========================= 

(defun do-fft-chunks (fft-chunks)

(let* ()
      (om::om-print "Aguarde" "Verbose :: ")
      (loop :for chunks-number :in (arithm-ser 1 (length fft-chunks) 1)
            :collect (list->string-fun (list 'fft- chunks-number)))))

;========================= Multithreading with lispwork ====================

(defun ckn-mailbox-name (list-of-something)

(let* ()
      (loop :for chunks-number :in (arithm-ser 1 (length list-of-something) 1)
            :collect (list->string-fun (list 'mailbox- (om::om-random 100 999) chunks-number)))))

;========================= 

(defun ckn-mailbox-peek (mail-box)
(mapcar (lambda (x) (mp:mailbox-peek x)) mail-box))

;================================================

(defun ckn-make-mail-box (names-of-all-process)
(loop :for name-process :in names-of-all-process
      :collect (mp:make-mailbox :lock-name name-process)))

;===============================================

(defun fft-ckn (sound-self fft-size hop-size windows-type)
(om::om-print "Aguarde!" "OM-CKN - Verbose ::")
  (let* (
        (sound (sound->bytes-fun sound-self))
        (zero-padding (om::x-append sound 
                                (loop :for i :from 1 :to (om::om- (om::om* (ceiling (om::om/ (length sound) fft-size)) fft-size) (length sound))
                                      :collect 0)))
        (sound-windows (sound-window zero-padding fft-size hop-size windows-type))
        (sound-windows-parts (loop-in-parts sound-windows 128 128))
        (sound-windows-length (length sound-windows))
        (fft-chunk-to-ms (om::arithm-ser 1 sound-windows-length 1))
        (fft-chunk-to-ms-parts (loop-in-parts fft-chunk-to-ms 128 128))
        (boolean-window-size (om::om> sound-windows-length 128)))

(if boolean-window-size
        (flat (loop :for loop-sound-windows-parts :in sound-windows-parts 
          :for loop-fft-chunk-to-ms-parts :in fft-chunk-to-ms-parts
          :collect (let* (
                         (action1 (do-fft-chunks loop-sound-windows-parts))
                         (action2 (ckn-make-mail-box action1)))
                     (fft-multiple-thread loop-sound-windows-parts action2 action1 loop-fft-chunk-to-ms-parts (sample-rate sound-self) hop-size))) 1)

   
  (let* (
                         
                         (action1 sound-windows)
                         (action2 fft-chunk-to-ms)                         
                         (action3 (do-fft-chunks sound-windows))
                         (action4 (ckn-make-mail-box action3)))
                     (fft-multiple-thread action1 action4 action3 action2 (sample-rate sound-self) hop-size)))))
 

(compile 'fft-ckn)
;=====================================================================

(defun fft-ckn-om (sound-self fft-size hop-size windows-type)
(om::om-print "Aguarde!" "OM-CKN - Verbose ::")
  (let* (
        (sound-selection (bytes (sound->bytes-smart sound-self)))
        (zero-padding (om::x-append sound-selection
                                (loop :for i :from 1 :to (om::om- (om::om* (ceiling (om/ (length sound-selection) fft-size)) fft-size) (length sound-selection))
                                      :collect (let* () 0))))
        (sound-windows (sound-window zero-padding fft-size hop-size windows-type))
        (sound-windows-parts (loop-in-parts sound-windows 128 128))
        (sound-windows-length (length sound-windows))
        (fft-chunk-to-ms (om::arithm-ser 1 sound-windows-length 1))
        (fft-chunk-to-ms-parts (loop-in-parts fft-chunk-to-ms 128 128))
        (boolean-window-size (om::om> sound-windows-length 129)))
(om::om-print "Conversao para Bytes concluida, aguarde o FFT." "OM-CKN - Verbose ::")
(if boolean-window-size
    (flat (loop :for loop-sound-windows-parts :in sound-windows-parts 
          :for loop-fft-chunk-to-ms-parts :in fft-chunk-to-ms-parts
          :collect (let* (
                         (action1 (do-fft-chunks loop-sound-windows-parts))
                         (action2 (ckn-make-mail-box action1)))
                     (fft-multiple-thread loop-sound-windows-parts action2 action1 loop-fft-chunk-to-ms-parts (sample-rate sound-self) hop-size))) 1)
  (let* (
                         
                         (action1 sound-windows)
                         (action2 fft-chunk-to-ms)                         
                         (action3 (do-fft-chunks sound-windows))
                         (action4 (ckn-make-mail-box action3)))
                     (fft-multiple-thread action1 action4 action3 action2 (sample-rate sound-self) hop-size)))))


;=====================================================================

(defun fft->Sin-model-fun (ckn-fft-instance filtro)

(loop 
        :for x :in ckn-fft-instance 
        :collect 
            (let* (
                  (FFT-SIZE (FFT-WINDOW x))
                  (TEMPO (ckn-tempo x))
                  (AMPLITUDES (amplitudes x))
                  (PHRASE (phrase x))
                  (CORRECTION-FOR-DB (case FFT-SIZE 
                                          (512 124.53343)
                                          (1024 250.19278749034922D0)
                                          (2048 501.4303903221932D0)
                                          (4096 1026.685)))
                  (MAG->DB 
                        (let* (
                                (action1 (om::om/ AMPLITUDES CORRECTION-FOR-DB))
                                (action2 (mapcar (lambda (x) (if (plusp x) (log x 10) -150.0)) action1)))
                          (om::om* 20 action2)))

                  (SPEAR-CORRECTION 
                        (om::mat-trans (spear-approach MAG->DB filtro FFT-SIZE PHRASE (sound-sample-rate x)))))
                                                                        ;; COLOCAR SAMPLE-RATE NA CKN-FFT-INSTANCE
(make-instance 'ckn-fft-instance 
                :fft-window FFT-SIZE
                :ckn-complex-numbers nil
                :ckn-hop-size (ckn-hop-size x)
                :fft-chunks nil
                :sound-sample-rate (sound-sample-rate x)
                :phrase (third SPEAR-CORRECTION)
                :ckn-tempo TEMPO 
                :frequencias (first SPEAR-CORRECTION)
                :amplitudes (second SPEAR-CORRECTION)))))


;;; ============== isso é o principal ================== ESBOCO

(defun spear-approach (deb filtro fft-size phrase sample-rate)
  
(let* (
  (action1 
      (loop   
            :for loop-amplitudes :on deb
            :for loop-number-bin :in (om:arithm-ser 1 (length deb) 1)
                                      ;; (om:arithm-ser 0 (1- (length deb)) 1) 

                                      ;; Isso é um ótimo exemplo de erro comum, o código 
                                      ;; (om:arithm-ser 0 (1- (length deb)) 1) pode transformar o resultado final das 
                                      ;; frequencias em certa de 2 ou três Hertz.
            :for phrase-loop :in phrase
            :while (om::om< 3 (length loop-amplitudes))
          :collect 
              (if  (let* (
                          (first-amp (first loop-amplitudes))
                          (second-amp (second loop-amplitudes))
                          (third-amp (third loop-amplitudes)))
                    
                    (and 
                          (om::om< first-amp second-amp)
                          (om::om> second-amp third-amp)
                          (om::om< filtro second-amp)))
                                      ;;; Este pedaço de código é responsável por criar o Local Maxima
                                      ;; É necessário cumprir 3 coisas
                                      ;; 1ª = A amplitude do bin (x-1) precisa ser menor que a amplitude de x 
                                      ;; 2ª = A amplitude do bin (x+1) precisa ser menor que a amplitude de x
                                      ;; 3ª = A amplitude de x precisa ser maior que a amplitude do filtro
                  (let* (
                    (Local-Maxima (list (1- loop-number-bin)  loop-number-bin (1+ loop-number-bin)))
                    (Local-Maxima-is-positive 
                              (if (equal (om::om-abs Local-Maxima) Local-Maxima)
                                  Local-Maxima 
                                  '(1 2 3)))
                    (amplitudes-de-Local-Maxima (om::posn-match deb Local-Maxima-is-positive))
                    (BIN-CORRECTION      
                                (if (equal 3 (length amplitudes-de-Local-Maxima))
                                      (let* (
                                            (a (first amplitudes-de-Local-Maxima))
                                            (b (second amplitudes-de-Local-Maxima))
                                            (c (third amplitudes-de-Local-Maxima)))
                                            (* 1/2 (/ (- a c) (+ (- a (* 2 b)) c))))))
                    (bin-para-frequencia 
                            (om::om-round (first (bin->freq (list (om::om+ loop-number-bin BIN-CORRECTION)) sample-rate fft-size)) 2))
                    (correcao_de_amplitude 
                                          (if (equal 3 (length amplitudes-de-Local-Maxima))
                                              (let* (
                                                    (a (first amplitudes-de-Local-Maxima))
                                                    (b (second amplitudes-de-Local-Maxima))
                                                    (c (third amplitudes-de-Local-Maxima)))
                                                (- b (* 1/4 (- a c) BIN-CORRECTION))))))
                    (list bin-para-frequencia (om::db->lin correcao_de_amplitude) phrase-loop))))))

    (remove nil action1)))

;===================================================================== Files control =====================================

;=====================================

(defun name-of-file (p)
  (let ((path (and p (pathname p))))
  (when (pathnamep path)
    (string+ (pathname-name path) 
             (if (and (pathname-type path) (stringp (pathname-type path)))
                 (string+ "." (pathname-type path)) 
               "")))))
               
;  ========================

(defun ckn-string-name (list-name)

(if (< (length list-name) 2)
    (car list-name)
(let*  (
    (action1 (string+ (first list-name) (second list-name)))
    (action2 (if 
                (>  (length (x-append action1 list-name)) 2)
                (x-append action1 (last-n list-name (- (length list-name) 2)))
                action1)))
    
    (if (< (length action2) 2)
            (first action2)
            (setf list-name (ckn-string-name action2))))))
            
(compile 'ckn-string-name)
;; ============

(defun ckn-int2string (int)
      "Number to string."
  (write-to-string int))

;; ============

(defun ckn-clear-temp-files ()

(let* ()
(ckn-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))
(ckn-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.wav" (outfile ""))))))))))))
;; ================================

(defun ckn-clear-the-file (x)

(if (atom x) 
    (ckn-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring x))))))))
    (loop :for x-file :in x
          :do (ckn-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring x-file)))))))))))

;; ================================

(defun ckn-copy2outfile (x)
(let* (
      (action1 (string+ "copy " (list->string-fun (list x)) " " (list->string-fun (list (namestring (outfile "")))))))
      (ckn-cmd-line action1)
      (ckn-clear-temp-files)
      (outfile (name-of-file x))))

;; ================================

(defun ckn-rename-file (x)
(let* (
      (action1 (string+ "copy " (list->string-fun (list x)) " " (list->string-fun (list (namestring (outfile "")))))))
      (ckn-cmd-line action1)
      (ckn-clear-temp-files)
      (outfile (name-of-file x))))

;; ================================

(defun ckn-list-to-string (lst)
    (format nil "~{~A ~}" lst))

;; ================================

(defmethod! ckn-temp-folder ((string string))
:initvals '(nil)
:indoc '("Temp folder") 
:icon '17359
:doc "Temp folder, not in OneDrive Folder."

(merge-pathnames (def-temp-folder) string))

;; ================================

(ensure-directories-exist (outfile " " :subdirs "\om-ckn"))

;================================== WAIT PROCESS =================
(defun loop-until-probe-file (my-file)
        (loop :with file = nil 
              :while (equal nil (setf file (probe-file my-file)))
        :collect file)
        
(probe-file my-file))

;==========================

(defun loop-until-finish-process (mailbox)
      (loop :with mailbox-empty = nil :while 
            (setf mailbox-empty (remove nil (mapcar (lambda (x) (mp:mailbox-empty-p x)) mailbox)))
            :do (let* ()
            mailbox-empty)))


;================================== Antescofo =================

(defun ckn-antescofo-score (voice variance local)
(let* (
    (voice-tempo (tempo voice))
    (voice-tree (om::om-abs (ms->sec (om6-true-durations (make-value 'voice (list (list :tree (mktree (tree2ratio (tree voice)) '(4 4))) (list :tempo voice-tempo)))))))
    (voice-midi (lmidic voice))
    (voice-to-rest (choose-to-rest voice))
    (SCORE
            (loop :for voice-tree-loop :in voice-tree 
            :for rest-loop :in voice-to-rest
            :collect
                    (om::x-append (if (equal (length (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))) 1) 
                    "NOTE"
                    "CHORD")

                        (if (equal (length (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))) 1)
                            (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))
                            (list (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop))))

                        voice-tree-loop
                        (if (equal (if (equal (choose voice-midi rest-loop) nil) (list 0) (choose voice-midi rest-loop)) (list 0))
                            nil
                            "@pizz"))))

    (Score-acabada (om::x-append (list (om::x-append "BPM" voice-tempo) (om::x-append "Variance" variance)) SCORE)))
(save-as-text Score-acabada local)))

; ========================================== OSC-PLAY =======================
(defun chord->voice (lista-de-notas)
(mktree (loop :for i :from 1 to (length lista-de-notas) :collect (let* () 1/4)) (list 4 4)))

;; ==================

(defun normalize-chord-seq (chrdseq)
  (let* ((xdx (om::x->dx (om::lonset chrdseq)))
         (filt-durs1 (mapcar 'list-min (om::ldur chrdseq)))
         (lst-durs (mapcar 'list xdx filt-durs1))
         (filt-durs2 (mapcar 'list-min lst-durs))
         (newdurs (loop 
                   :for pt :in (om::lmidic chrdseq)
                   :for drs :in filt-durs2
                   collect (repeat-n drs (length pt)))))
    (make-instance 'chord-seq 
                   :lmidic (om::lmidic chrdseq)
                   :lonset (om::lonset chrdseq)
                   :ldur newdurs)))

;; ======================================================================

(defun voice->coll (ckn number-2)
(let* (
  (ckn-action1  (loop 
                      :for ckn-plus :in (om6-true-durations ckn) 
                      :collect (if (plusp ckn-plus) 0 1)))
  (ckn-action2  (loop 
                      :for cknloop :in ckn-action1 
                      :collect (if 
                                    (om::om= 0 cknloop)
                                    (setq number-2 (om::om+ number-2 1)))))
  (ckn-action3 
            (let* ((ckn-action3-1 
                          (if 
                              (equal nil (first ckn-action2))
                              0 
                              (first ckn-action2))))             
                  (if 
                      (equal nil (first ckn-action2)) 
                      (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) 
                      (om::om+ (om::om- ckn-action2 ckn-action3-1) 1)))))
(loop 
      :for cknloop-1 :in ckn-action3 
      :for cknloop-2 :in (om::dx->x 0 (loop :for y :in (om6-true-durations ckn) :collect (abs y))) 
      :for cknloop-3 :in (om6-true-durations ckn) 
      :collect          
      (if (plusp cknloop-3) 
            (om::x-append 
               (if (plusp cknloop-3) cknloop-2 nil)
                  (om::x-append  
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "LMIDIC") cknloop-1) 
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "lvel") cknloop-1)
                  (choose (om::get-slot-val (om::make-value-from-model 'voice ckn nil) "lchan") cknloop-1)
                    (if (plusp cknloop-3) cknloop-3 nil) 
                       ))))))

;; ======================================================================

(defun choose-ratio-of-note (voice note-number)
  (let* (
        (action1 (remove nil (mapcar (lambda (x) (if (plusp x) x nil)) (tree2ratio (tree voice)))))
        (action2 (mktree (list (choose action1 note-number)) (list 4 4)))
        (action3 (make-instance 'voice :tree action2 :tempo (tempo voice))))
    (car (om6-true-durations action3))))

;================================================ ckn-gc-all ==================

(defun ckn-gc-all (x)

(nth 1 (om::multiple-value-list (om::seq (om::gc-all) x))))


;; ===================================== Information =========================== 

(defun read-dur-informations (filename)
      (let* (  
            (action1 
            (with-open-file (stream filename)
                  (loop :for line := (read-line stream nil)
                        :while line 
                        :collect line)))
            (action2 (second action1)))
       (read-from-string (car (last (string-to-list action2))))))

;; =================================================== OM6 functions

(defun om6-true-durations (ckn)

 (let* ((newchrdseq (if (typep ckn 'note) 
                           (om::Objfromobjs (om::Objfromobjs ckn (make-instance 'chord)) (make-instance 'chord-seq))
                           (om::Objfromobjs ckn (make-instance 'chord-seq))))

         (newcs (normalize-chord-seq newchrdseq))
         (onsets (om::Lonset newcs))
         (dur (om::Ldur newcs))
         (newonsets (if (= 2 (length onsets)) (om::x->dx  onsets) (butlast (om::x->dx onsets))))
         (newdurs (mapcar 'first dur))
         (resultat1 
          (om::x-append 
          (flat
           (list (mapcar #'(lambda (x y) (if (= 0 (- x y)) x 
                                             (list x (- x y))))
                         newdurs newonsets)
                 (last newdurs)))
          (last-elem newdurs)))
         (resultat2 (butlast
                     (if (= 0 (first onsets)) resultat1 (cons (om::om* -1 (first onsets)) resultat1)))))
    
   (let ((result (remove nil (mapcar #'(lambda (x) (if (not (or (= x 1) (= x -1))) x ))
          resultat2))))
         (if (= 2 (length onsets)) (list (car result) (second result)) result))))

;===================================================================== Compile in OM-SHARP =================================

(compile 'ckn-gc-all)
(compile 'loop-in-parts)
(compile 'sound-window-list)
(compile 'sound-window)
(compile 'real-samplify)
(compile 'name-of-file)
(compile 'energy)
(compile 'fft->amplitude-fun)
(compile 'fft->phrase-fun)
(compile 'by-N-fun)
(compile 'array-to-list-fun)
(compile 'list-to-array-fun)
(compile 'list-dimensions)
(compile 'list->string-fun)
(compile 'fft-multiple-thread)
(compile 'do-fft-chunks)
(compile 'ckn-make-mail-box)
(compile 'ckn-clear-temp-files)
(compile 'spear-approach )
(compile 'fft->sin-model-fun)
(compile 'do-senoide)
(compile 'ITD-Sound)
(compile 'bytes->sound-fun)
