
(in-package :om)


(require-library "om-py")
(load-om-library "om-py")

;; Preferencias ========================

(if (equal *app-name* "om-sharp")
  (let* ()
          (add-preference-section :externals "OM-CKN" nil '(:sox-exe :PureData :Pd-Patches :ircam-instruments :OrchideaSOL :plugins :Sonic-visualizer))
          (add-preference :externals :sox-exe "Sox Path" :file (merge-pathnames "executables/SOX/windows/sox.exe" (lib-resources-folder (find-library "OM-CKN"))))
          (add-preference :externals :PureData "Pure Data executable" :file " ")
          (add-preference :externals :Sonic-visualizer "Sonic-Visualizer executable" :file " ")
          (add-preference :externals :Pd-Patches "Pure Data Patches" :folder (merge-pathnames "Pd-Patches/" (lib-resources-folder (find-library "OM-CKN"))))
          (add-preference :externals :ircam-instruments "Ircam Instruments Path" :folder "Your Ircam Instruments Folder")
          (add-preference :externals :OrchideaSOL "SOL Samples Library" :folder "SOL folder")
          (add-preference :externals :plugins "Plugins DLL" :folder "Your Plugins VTS2 Folder")))

;; =======================================================================
;; ================= CLASSES =============================================
;; =======================================================================

; ================================================== Sdif-Class =====================

(defclass! ckn-sdif ()
    ((ckn-matrix :initform nil :initarg :ckn-matrix :accessor ckn-matrix)))

; ================================================== FFT-Class =====================

(defclass! ckn-fft-instance ()
( 
  (ckn-complex-numbers :initform '#(-6.1035157E-5 0.0) :initarg :ckn-complex-numbers :accessor ckn-complex-numbers)
  (sound-sample-rate :initform nil :initarg :sound-sample-rate :accessor sound-sample-rate)
  (fft-window :initform nil :initarg :fft-window :accessor fft-window)
  (ckn-hop-size :initform nil :initarg :ckn-hop-size :accessor ckn-hop-size)
  (fft-chunks :initform nil :initarg :fft-chunks :accessor fft-chunks)
  (ckn-tempo :initform nil :initarg :ckn-tempo :accessor ckn-tempo)
  (frequencias :initform nil :initarg :frequencias :accessor frequencias)
  (amplitudes :initform nil :initarg :amplitudes :accessor amplitudes)
  (phrase :initform nil :initarg :phrase :accessor phrase)
                                    )
  (:icon 17359))

;==================================================================================

(defclass! fft-complex-numbers ()
((complex-numbers :initform '#(-6.1035157E-5 0.0) :initarg :complex-numbers :accessor complex-numbers))
(:icon 17359))

; ================================================== Bytes-Class =====================

(defclass! sound-bytes ()
((bytes :initform nil :initarg :bytes :accessor bytes))
(:icon 17359))
                              

; ============ Facilitar minha vida!!! =================

(defmethod! nth-repeated-random ((list list) (times number))
:initvals ' (NIL)
:indoc ' ("one list")
:icon 'combinatory
:doc ""

(om::repeat-n (om::nth-random list) times))

; ======================================== Methods and Functions ==================================================

(defmethod! complex-numbers-parts ((list list))
:initvals ' (NIL)
:indoc ' ("list of complex-numbers")
:icon '17359
:numouts 2
:outdoc '("imagpart" "realpart")
:doc ""
(values 
  (mapcar (lambda (x) (imagpart x)) list)
  (mapcar (lambda (x) (realpart x)) list)))

; ================================ Python Methods ==================================


(defmethod! python-colors nil
:icon 'py-f
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.
For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'." 


'("b"  "g"  "r"  "c"  "m"  "y"  "k"  "w"   "tab:blue"  "tab:orange"  "tab:green"  "tab:red"  "tab:purple"  "tab:brown"  "tab:pink"  "tab:gray"  "tab:olive"  "tab:cyan" 

 "xkcd:black"  "xkcd:white"  "xkcd:dull red"  "xkcd:dried blood"  "xkcd:dark red"  "xkcd:red"  "xkcd:deep red"  "xkcd:mahogany"  "xkcd:pastel red"  "xkcd:reddish"  "xkcd:grapefruit"  "xkcd:deep brown"  "xkcd:dark coral"  "xkcd:pale red"  "xkcd:coral"  "xkcd:dark salmon"  "xkcd:brownish pink"  "xkcd:very dark brown"  "xkcd:indian red"  "xkcd:salmon"  "xkcd:pinkish grey"  "xkcd:reddy brown"  "xkcd:reddish grey"  "xkcd:brick red"  "xkcd:tomato"  "xkcd:peachy pink"  "xkcd:orangey red"  "xkcd:brick"  "xkcd:very light pink"  "xkcd:brownish red"  "xkcd:orange red"  "xkcd:blush"  "xkcd:vermillion"  "xkcd:orange pink"  "xkcd:tomato red"  "xkcd:burnt red"  "xkcd:reddish orange"  "xkcd:orangish red"  "xkcd:red brown"  "xkcd:light salmon"  "xkcd:melon"  "xkcd:rusty red"  "xkcd:rust red"  "xkcd:pinkish orange"  "xkcd:pinkish brown"  "xkcd:orangered"  "xkcd:red orange"  "xkcd:pale salmon"  "xkcd:clay"  "xkcd:dark peach"  "xkcd:brown red"  "xkcd:terracotta"  "xkcd:terracota"  "xkcd:reddish brown"  "xkcd:blood orange"  "xkcd:pinkish tan"  "xkcd:terra cotta"  "xkcd:auburn"  "xkcd:adobe"  "xkcd:orangish"  "xkcd:warm grey"  "xkcd:brownish"  "xkcd:rust"  "xkcd:russet"  "xkcd:chestnut"  "xkcd:rust brown"  "xkcd:deep orange"  "xkcd:brick orange"  "xkcd:bright orange"  "xkcd:burnt umber"  "xkcd:orangeish"  "xkcd:chocolate brown"  "xkcd:earth"  "xkcd:burnt sienna"  "xkcd:peach"  "xkcd:dusty orange"  "xkcd:sienna"  "xkcd:dark orange"  "xkcd:burnt orange"  "xkcd:pastel orange"  "xkcd:rusty orange"  "xkcd:rust orange"  "xkcd:cocoa"  "xkcd:copper"  "xkcd:faded orange"  "xkcd:burnt siena"  "xkcd:cinnamon"  "xkcd:mushroom"  "xkcd:chocolate"  "xkcd:clay brown"  "xkcd:orange"  "xkcd:apricot"  "xkcd:sepia"  "xkcd:dull orange"  "xkcd:pale orange"  "xkcd:pumpkin orange"  "xkcd:mocha"  "xkcd:milk chocolate"  "xkcd:light peach"  "xkcd:brownish orange"  "xkcd:warm brown"  "xkcd:dark brown"  "xkcd:pale brown"  "xkcd:browny orange"  "xkcd:orangish brown"  "xkcd:orange brown"  "xkcd:tan brown"  "xkcd:pumpkin"  "xkcd:light brown"  "xkcd:puce"  "xkcd:dark taupe"  "xkcd:leather"  "xkcd:orangey brown"  "xkcd:raw umber"  "xkcd:light orange"  "xkcd:brown"  "xkcd:umber"  "xkcd:brown orange"  "xkcd:tangerine"  "xkcd:dirty orange"  "xkcd:medium brown"  "xkcd:mango"  "xkcd:butterscotch"  "xkcd:dull brown"  "xkcd:coffee"  "xkcd:taupe"  "xkcd:dirt"  "xkcd:dirt brown"  "xkcd:dark tan"  "xkcd:caramel"  "xkcd:brownish grey"  "xkcd:fawn"  "xkcd:greyish brown"  "xkcd:dust"  "xkcd:toupe"  "xkcd:raw sienna"  "xkcd:very light brown"  "xkcd:camel"  "xkcd:sand brown"  "xkcd:yellowish orange"  "xkcd:grey brown"  "xkcd:dark beige"  "xkcd:orange yellow"  "xkcd:squash"  "xkcd:mud brown"  "xkcd:sandstone"  "xkcd:macaroni and cheese"  "xkcd:pale peach"  "xkcd:dark sand"  "xkcd:golden brown"  "xkcd:tan"  "xkcd:saffron"  "xkcd:putty"  "xkcd:amber"  "xkcd:poo brown"  "xkcd:sandy brown"  "xkcd:yellow orange"  "xkcd:shit brown"  "xkcd:orangey yellow"  "xkcd:desert"  "xkcd:bronze"  "xkcd:mustard brown"  "xkcd:poop brown"  "xkcd:poop"  "xkcd:golden rod"  "xkcd:ochre"  "xkcd:shit"  "xkcd:muddy brown"  "xkcd:sunflower"  "xkcd:marigold"  "xkcd:brown grey"  "xkcd:golden yellow"  "xkcd:wheat"  "xkcd:mud"  "xkcd:yellow ochre"  "xkcd:goldenrod"  "xkcd:light mustard"  "xkcd:maize"  "xkcd:golden"  "xkcd:sand"  "xkcd:ocre"  "xkcd:yellowy brown"  "xkcd:yellowish brown"  "xkcd:pale gold"  "xkcd:stone"  "xkcd:greyish"  "xkcd:burnt yellow"  "xkcd:light gold"  "xkcd:puke brown"  "xkcd:hazel"  "xkcd:ocher"  "xkcd:dark gold"  "xkcd:poo"  "xkcd:bland"  "xkcd:sandy"  "xkcd:yellow tan"  "xkcd:yellow brown"  "xkcd:dark mustard"  "xkcd:gold"  "xkcd:beige"  "xkcd:baby shit brown"  "xkcd:sand yellow"  "xkcd:diarrhea"  "xkcd:dark khaki"  "xkcd:olive brown"  "xkcd:light tan"  "xkcd:baby poo"  "xkcd:baby poop"  "xkcd:brown yellow"  "xkcd:dark yellow"  "xkcd:sunflower yellow"  "xkcd:sun yellow"  "xkcd:mustard"  "xkcd:pale"  "xkcd:brownish yellow"  "xkcd:dandelion"  "xkcd:dull yellow"  "xkcd:dark cream"  "xkcd:sandy yellow"  "xkcd:mustard yellow"  "xkcd:muddy yellow"  "xkcd:cement"  "xkcd:ugly brown"  "xkcd:greenish brown"  "xkcd:greeny brown"  "xkcd:buff"  "xkcd:yellowish"  "xkcd:green brown"  "xkcd:ugly yellow"  "xkcd:olive yellow"  "xkcd:khaki"  "xkcd:egg shell"  "xkcd:straw"  "xkcd:brown green"  "xkcd:manilla"  "xkcd:dirty yellow"  "xkcd:piss yellow"  "xkcd:vomit yellow"  "xkcd:browny green"  "xkcd:sunny yellow"  "xkcd:parchment"  "xkcd:puke yellow"  "xkcd:custard"  "xkcd:butter yellow"  "xkcd:light beige"  "xkcd:sunshine yellow"  "xkcd:bright yellow"  "xkcd:light yellow"  "xkcd:pastel yellow"  "xkcd:canary yellow"  "xkcd:off white"  "xkcd:eggshell"  "xkcd:ivory"  "xkcd:cream"  "xkcd:creme"  "xkcd:pale yellow"  "xkcd:yellowish tan"  "xkcd:butter"  "xkcd:banana"  "xkcd:yellow"  "xkcd:puke"  "xkcd:faded yellow"  "xkcd:lemon yellow"  "xkcd:off yellow"  "xkcd:lemon"  "xkcd:canary"  "xkcd:vomit"  "xkcd:drab"  "xkcd:ecru"  "xkcd:banana yellow"  "xkcd:brownish green"  "xkcd:pea soup"  "xkcd:mud green"  "xkcd:baby poop green"  "xkcd:olive"  "xkcd:mustard green"  "xkcd:baby puke green"  "xkcd:bile"  "xkcd:shit green"  "xkcd:snot"  "xkcd:greenish beige"  "xkcd:olive drab"  "xkcd:poop green"  "xkcd:sickly yellow"  "xkcd:dark olive"  "xkcd:baby shit green"  "xkcd:puke green"  "xkcd:pea soup green"  "xkcd:green/yellow"  "xkcd:swamp green"  "xkcd:murky green"  "xkcd:barf green"  "xkcd:light khaki"  "xkcd:vomit green"  "xkcd:olive green"  "xkcd:bright olive"  "xkcd:booger green"  "xkcd:pea"  "xkcd:gross green"  "xkcd:greenish tan"  "xkcd:snot green"  "xkcd:pea green"  "xkcd:neon yellow"  "xkcd:greenish yellow"  "xkcd:ugly green"  "xkcd:sick green"  "xkcd:sickly green"  "xkcd:lime yellow"  "xkcd:dark yellow green"  "xkcd:greeny yellow"  "xkcd:booger"  "xkcd:light olive"  "xkcd:icky green"  "xkcd:yellowish green"  "xkcd:muddy green"  "xkcd:dark olive green"  "xkcd:chartreuse"  "xkcd:camo"  "xkcd:yellowy green"  "xkcd:green yellow"  "xkcd:avocado green"  "xkcd:pale olive"  "xkcd:army green"  "xkcd:slime green"  "xkcd:khaki green"  "xkcd:avocado"  "xkcd:yellowgreen"  "xkcd:light olive green"  "xkcd:tan green"  "xkcd:yellow/green"  "xkcd:dark lime"  "xkcd:camouflage green"  "xkcd:yellow green"  "xkcd:dirty green"  "xkcd:pear"  "xkcd:lemon lime"  "xkcd:camo green"  "xkcd:lemon green"  "xkcd:dark lime green"  "xkcd:electric lime"  "xkcd:swamp"  "xkcd:military green"  "xkcd:pale olive green"  "xkcd:bright yellow green"  "xkcd:light yellow green"  "xkcd:sap green"  "xkcd:mossy green"  "xkcd:light moss green"  "xkcd:navy green"  "xkcd:lime"  "xkcd:acid green"  "xkcd:pale lime"  "xkcd:light lime green"  "xkcd:moss green"  "xkcd:leaf green"  "xkcd:light pea green"  "xkcd:lime green"  "xkcd:bright lime"  "xkcd:kiwi"  "xkcd:leaf"  "xkcd:kermit green"  "xkcd:drab green"  "xkcd:pale lime green"  "xkcd:light yellowish green"  "xkcd:apple green"  "xkcd:pistachio"  "xkcd:kiwi green"  "xkcd:moss"  "xkcd:light lime"  "xkcd:frog green"  "xkcd:key lime"  "xkcd:lawn green"  "xkcd:nasty green"  "xkcd:celery"  "xkcd:dark grass green"  "xkcd:spring green"  "xkcd:grassy green"  "xkcd:asparagus"  "xkcd:bright lime green"  "xkcd:grass"  "xkcd:light grass green"  "xkcd:turtle green"  "xkcd:grass green"  "xkcd:flat green"  "xkcd:apple"  "xkcd:light grey green"  "xkcd:lichen"  "xkcd:sage"  "xkcd:green apple"  "xkcd:medium grey"  "xkcd:light grey"  "xkcd:tea green"  "xkcd:toxic green"  "xkcd:light light green"  "xkcd:very light green"  "xkcd:off green"  "xkcd:very pale green"  "xkcd:washed out green"  "xkcd:greenish grey"  "xkcd:sage green"  "xkcd:dull green"  "xkcd:grey/green"  "xkcd:light sage"  "xkcd:pale green"  "xkcd:grey"  "xkcd:pale light green"  "xkcd:forrest green"  "xkcd:green grey"  "xkcd:fern green"  "xkcd:light green"  "xkcd:fern"  "xkcd:pastel green"  "xkcd:fresh green"  "xkcd:poison green"  "xkcd:leafy green"  "xkcd:tree green"  "xkcd:muted green"  "xkcd:light pastel green"  "xkcd:vivid green"  "xkcd:grey green"  "xkcd:greyish green"  "xkcd:lighter green"  "xkcd:faded green"  "xkcd:easter green"  "xkcd:greeny grey"  "xkcd:celadon"  "xkcd:mid green"  "xkcd:highlighter green"  "xkcd:electric green"  "xkcd:very dark green"  "xkcd:dark sage"  "xkcd:radioactive green"  "xkcd:dark green"  "xkcd:dusty green"  "xkcd:hunter green"  "xkcd:fluro green"  "xkcd:true green"  "xkcd:forest"  "xkcd:racing green"  "xkcd:vibrant green"  "xkcd:lightish green"  "xkcd:neon green"  "xkcd:fluorescent green"  "xkcd:dark pastel green"  "xkcd:bottle green"  "xkcd:hot green"  "xkcd:bright green"  "xkcd:boring green"  "xkcd:darkgreen"  "xkcd:green"  "xkcd:light neon green"  "xkcd:lightgreen"  "xkcd:light bright green"  "xkcd:light forest green"  "xkcd:light mint"  "xkcd:soft green"  "xkcd:dark forest green"  "xkcd:forest green"  "xkcd:british racing green"  "xkcd:medium green"  "xkcd:light mint green"  "xkcd:mint green"  "xkcd:deep green"  "xkcd:baby green"  "xkcd:light seafoam green"  "xkcd:darkish green"  "xkcd:mint"  "xkcd:pine"  "xkcd:bright light green"  "xkcd:emerald green"  "xkcd:slate green"  "xkcd:hospital green"  "xkcd:algae"  "xkcd:foam green"  "xkcd:light sea green"  "xkcd:kelly green"  "xkcd:irish green"))



(defmethod! bpf-python ((X list) (Y list) &optional (thickness 1) (color 'black))
:initvals '(nil nil 0.2 black)
:indoc ' ("X points" "Y points" "Thickness" "The color (for example black)")
:icon 'py-f
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.
For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (x_if (if (not x) (om::arithm-ser 1 (length y) 1) x))
      (y_if (if (not y) (om::arithm-ser 1 (length x) 1) y))
      (X-PYTHON (om-py::lisp-list_2_python-list (om::om-round x_if 10)))
      (Y-PYTHON (om-py::lisp-list_2_python-list (om::om-round y_if 10))))
(mp:process-run-function (string+ "BPF-PYTHON-" (write-to-string (om::om-random 1 1000)))
      () 
                  (lambda (x-axis y-axis) (if
                                              (equal *app-name* "om-sharp")
                                                     (bpf-python-fun x-axis y-axis thickness color)
                                                     (bpf-python-om x-axis y-axis thickness)
                                            )) X-PYTHON Y-PYTHON)))


; ==================================================================================

(defmethod! save-bpf-python ((X list) (Y list) &optional (thickness 0.4) (outfile nil) (color 'black) (blackbackgroud nil) (dpi 300))
:initvals ' (NIL)
:indoc ' ("Build a BPF in Python and save it.")
:icon 'py-f
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.

For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (outfile_in_python (list->string-fun (list (namestring outfile))))
      (black-backgroud (if blackbackgroud  "plt" "#plt")) 
      (x_if (if (not x) (om::arithm-ser 1 (length y) 1) x))
      (y_if (if (not y) (om::arithm-ser 1 (length x) 1) y))
      (X-PYTHON (lisp-list_2_python-list x_if))
      (Y-PYTHON (lisp-list_2_python-list y_if)))
(mp:process-run-function (string+ "Save-PYTHON-" (write-to-string (om::om-random 1 1000)))
      () 
                  (lambda (x-axis y-axis) (if
                                              (equal *app-name* "om-sharp")
                                                     (save-bpf-python-fun x-axis y-axis thickness color outfile_in_python black-backgroud dpi)))
                  X-PYTHON Y-PYTHON)))

; ==================================================================================

(defmethod! 3dc-python ((X list) (Y list) (Z list) &optional (thickness 1) (color 'black))
:initvals ' (NIL)
:indoc ' ("Build a 3DC in Python and save it.")
:icon 'py-f
:doc "This is a BPF like the BPF of OM-Sharp. But you can you more numbers.

For this work you need:
  1. Install Python and put it the Path variables.
  2. Install the pip (Google it!!)
  3. Install the matplotlib.pyplot with 'pip install matplotlib.pyplot'."

(let* (
      (X-PYTHON (om-py::lisp-list_2_python-list X))
      (Y-PYTHON (om-py::lisp-list_2_python-list Y))
      (Z-PYTHON (om-py::lisp-list_2_python-list Z)))
(mp:process-run-function (string+ "3DC-PYTHON" (write-to-string (om::om-random 1 1000)))
                 () 
                  (lambda (x-axis w-axis z-axis) (3dc-python-fun x-axis w-axis z-axis thickness color)) X-PYTHON Y-PYTHON Z-PYTHON)))
                                    
;==================================================

(defmethod! BORDEAUX-fft ((sound-array array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(time (fft-fun sound-array)))

;==================================================

(defmethod! ckn-fft ((sound sound) (fft-size number) (hop-size number) (window-type number))
:initvals '(nil 2048 512 nil)
:indoc '("Sound class" "FFT-size" "Hop-size" "Windows-type") 
:menuins '((3 (("hann" 1) ("blackman" 2) ("barlett" 3) ("hamming" 4) ("rectangular" 5) ("nenhuma" 6))))
:icon '17359
:doc "It does the FFT in a sound."

(if (compiled-function-p #'sapa-fft!) nil (compile 'sapa-fft!))

(if 
    (equal *app-name* "om-sharp")
        (fft-ckn sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))

        (fft-ckn-om sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))))

;==================================================

(defmethod! ckn-fft ((sound sound) (fft-size number) (hop-size number) (window-type null))
:initvals '(nil 2048 512 nil)
:indoc '("Sound class" "FFT-size" "Hop-size" "Windows-type") 
:menuins '((3 (("hann" 1) ("blackman" 2) ("barlett" 3) ("hamming" 4) ("rectangular" 5) ("nenhuma" 6))))
:icon '17359
:doc "It does the FFT in a sound."

(if (compiled-function-p #'sapa-fft!) nil (compile 'sapa-fft!))

(if (not window-type)
  (om-message-dialog "You need to define which window-type (fourth inlet) will be used for the fft analysis."))

(if 
    (equal *app-name* "om-sharp")
        (fft-ckn sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))

        (fft-ckn-om sound fft-size hop-size (case window-type 
            (1 :hanning)
            (2 :blackman)
            (3 :barlett)
            (4 :hamming)
            (5 :rectangular)
            (6 nil)))))



;==================================================

(defmethod! complex-senoide-h ((freq number) (sec number) (samples-rate number))
:initvals '(nil)
:indoc '("Frequency in Hz" "Durations in sec." "Sample-rate")
:icon '17359
:doc "It does one senoide in the complex plan."

(let* (
      (durations (x-append 0 sec))
      (sec-samples (round (om::sec->samples sec samples-rate)))
      (sampling (nth 2 (om::multiple-value-list (om::om-sample durations sec-samples)))))
  (create-pure-tone-h freq sampling)))

;==================================================

(defmethod! split-complex-numbers ((complex-number list))
:initvals '(nil)
:indoc '("List of complex numbers")
:icon '17359
:numouts 2
:outdoc '("Imag part" "Real Part")
:doc "It does one senoide in the complex plan."
(values 
 (mapcar (lambda (x) (imagpart x)) complex-number)
 (mapcar (lambda (x) (realpart x)) complex-number)))


;==================================================

(defmethod! partial-tracking ((ffts list) (db-limiter number) (cents_limiter number))
:initvals '(nil)
:indoc '("List of ckn-fft")
:icon '17359
:doc "It does the partial-tracking of some FFT."

(fft->sdif ffts cents_limiter db-limiter))

;==================================================

(defmethod! complex-senoide ((freq number) (sec number) (samples-rate number))
:initvals '(nil)
:indoc '("Frequency in Hz" "Durations in sec." "Sample-rate")
:icon '17359
:doc "It does one senoide in the complex plan."

(let* (
      (durations (x-append 0 sec))
      (sec-samples (round (om::sec->samples sec samples-rate)))
      (sampling (nth 2 (om::multiple-value-list (om::om-sample durations sec-samples)))))
  (create-pure-tone freq sampling)))

;==================================================

(defmethod! fft->amplitude ((fft array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->amplitude-fun fft))

;==================================================

(defmethod! fft->amplitude ((fft list))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->amplitude (list-to-array fft 1)))

;=============================

(defmethod! fft->phrase ((fft array))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->phrase-fun fft))

;=============================

(defmethod! fft->phrase ((fft list))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(fft->phrase (list-to-array fft 1)))

;==================================== ARRAY UTILITIES =======

(defmethod! list-to-array ((array-my list) (dimensions integer))
:initvals '((nil) (nil))
:indoc '("Sound class" "bla bla")
:icon '17359
:doc "It reads a wave file."

(list-to-array-fun array-my dimensions))

;=====================================

(defmethod! array-to-list ((my-array array))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359 
:doc "It reads a wave file."

(array-to-list-fun my-array))

;=====================================

(defmethod! by-N ((my-list list) (division-number number) (my-function function))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359 
:doc "It reads a wave file."

(by-N-fun my-list division-number my-function))

;=====================================

(defmethod! sound->bytes ((self sound))
:initvals '(nil)
:indoc '("Sound class")
:icon '17359
:doc "It reads a wave file."

(if (equal *app-name* "om-sharp")
  (make-instance 'sound-bytes :bytes (sound->bytes-fun self))
  (sound->bytes-om-class self)))

;=====================================

(defmethod! bytes->sound ((self list) &optional (quantos-canais 1) (qual-canal 1))
:initvals '(nil 1 1)
:indoc '("bytes 0 until 1 list" "number of channels" "where write?")
:icon '17359
:doc "It create a sound from list of bytes (0 until 1)."

(if (equal *app-name* "om-sharp")
    (bytes->sound-fun self quantos-canais qual-canal)))

;=====================================

(defmethod! cartopol ((fft cons))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:numouts 2
:outdoc ' ("phrase" "amplitude")
:icon '17359
:doc "Do the same thing that the cartopol of Max/MSP."

(values 
  (mapcar (lambda (x) (fft->phrase x)) fft)
  (mapcar (lambda (y) (fft->amplitude y)) fft)))


;=====================================

(defmethod! bin->freq ((fft-bin list) (sample-rate number) (fft-size number))
:initvals ' ((41 82 122 163 204) 44100 4096)
:indoc ' ("FFT bin number" "Sample-rate" "Fft-size")
:numouts 1
:outdoc ' ("Frequency")
:icon '17359
:doc ""

(bin->freq-fun fft-bin sample-rate fft-size))
;=================

(defun bin->freq-fun (fft-bin sample-rate fft-size)
  (om::om* fft-bin (float (om::om/ sample-rate fft-size))))

;==================================================

(defmethod! half ((fft-array array))
:doc "E necessaria somente metade do resultado do FFT."

(half-fun fft-array))

; =================================================


(defmethod! fft->dB ((amplitudes list))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
         (fft-size (* 2 (length amplitudes)))
				(action1 (om/ amplitudes (* 0.10519141 (print fft-size))))
				(action2 (mapcar (lambda (x) (log x 10)) action1)))
	(om* 20 action2)))

;; ============================================= SDIF ========================================

(defmethod! sdif->list ((sdif-file sdiffile))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(sdif->list-fun sdif-file))

;; =============================================

(defmethod! sdif->list ((sdif-file pathname))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(sdif->list (make-value-from-model 'sdiffile sdif-file nil)))

;; =============================================

(defun sdif->list-fun (sdif-file) 

(let* (
    (info (sdifinfo sdif-file nil))
    (action1 (second (multiple-value-list 
                (om::getsdifdata sdif-file 0 (second (car info)) (third (car info)) '(0 1 2) nil nil nil nil))))
    (action2 (getsdifframes sdif-file)))

        (loop 
            :for cknloop 
            :in (arithm-ser 1 (length action2) 1) 
                  :collect       
    
        (x-append 
              (get-slot-val (make-value-from-model 'sdifframe (posn-match action2 (1-  cknloop)) nil) "FTIME")
              (let* (

(action3-1 
        (posn-match 
                    (om::get-slot-val (make-value-from-model 'sdifmatrix 
                                          (first (om::get-slot-val 
                              (om::make-value-from-model 'sdifframe (posn-match action2 (1- cknloop)) nil)
                                            "LMATRIX")) nil) "DATA") '(0 1 2 3)))
(action3-2 (mat-trans (list (om::om-round (first action3-1)) (om::om-round (second action3-1) 2) (third action3-1) (fourth action3-1)))))
action3-2)))))

;; ====================================================

(defmethod! sdif-envelope ((sdif-file sdiffile))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(let* (
    (action1 (sdif->list-fun sdif-file))
    (action2 
        (mapcar (lambda (x)
          (reduce (lambda (xy xx) (+ xy xx))
              (mapcar
                  (lambda (xxx) (third xxx)) (last-n x (- (length x) 1))))) action1)))

(make-value 'bpf (list (list :x-points nil) (list :y-points action2)))))

;; ===================================

(defmethod! sdif->ckn-fft-instance ((sdif-file sdiffile))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc "This will translate each frame of the SDIF file for the ckn-fft-instance class."

(let* (
    (action1 (sdif->list-fun sdif-file))
    (partialtracking (mapcar (lambda (x) (cdr x)) action1))
    (tempo (sec->ms (mapcar (lambda (x) (car x)) action1)))
    (ckn-tempo (ms->samples (car (flat tempo)) 44100)))
    (loop :for looptempo :in tempo 
          :for looppartialtracking :in partialtracking
                        :collect
                    (make-value 'ckn-fft-instance 
                                (list 
                                    (list :ckn-tempo looptempo)
                                    (list :ckn-hop-size ckn-tempo)
                                    (list :frequencias (mapcar (lambda (x) (second x)) looppartialtracking))
                                    (list :amplitudes (mapcar (lambda (x) (third x)) looppartialtracking))
                                    (list :phrase (mapcar (lambda (x) (fourth x)) looppartialtracking))
                                    (list :sound-sample-rate 44100))))))

;; ===================================

(defmethod! sdif->ckn-fft-instance ((sdif-file pathname))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc "This will translate each frame of the SDIF file for the ckn-fft-instance class."

(sdif->ckn-fft-instance (make-value-from-model 'sdiffile sdif-file nil)))

;; ====================================================

(defmethod! save-spear-sdif ((sdif-frames list) (name string))
:initvals ' (NIL)
:indoc ' ("Sdif-File." "Name")
:icon '17359
:doc ""

(write-sdif-file sdif-frames :outpath
  (outfile (string+ name ".sdif"))
  :types
  (list (make-value 'sdiftype (list (list :struct 'f) (list :signature "1TRC")
     (list :description (list (list "XNFO" "InfoMat") (list "XMAT" "datamat"))))))))


;; ====================================================

(defmethod! save-spear-sdif ((sdif-frames ckn-sdif) (name string))
:initvals ' (NIL)
:indoc ' ("Sdif-File." "Name")
:icon '17359
:doc ""
(save-spear-sdif (ckn-matrix sdif-frames) name))

;; ====================================================

(defmethod! fft->sdif ((sdif-frames list) (freq-threshold number) (db-threshold number))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""
(fft->sdif-fun sdif-frames freq-threshold db-threshold))

;; ====================================================

(defmethod! sound-seq-list ((sound list) &optional (fade-bewteen-sound 0.001))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(om-print "Aguarde alguns segundos!" "Verbose")

(let* (
      (action1 (build-sound-sequence-fun sound fade-bewteen-sound))
      (action2 
        (om::om-cmd-line (string+ "powershell -command " 
                          (list->string-fun (list (string+ "del " 
                                            (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))))

  (save-sound action1 (merge-pathnames (string+ "om-ckn/temp-sound" (list->string-fun (list (om::om-random 0 9999999))) ".wav") (outfile "")))))

;; ====================================================

(defmethod! sound-mix-list ((sound list))
:initvals ' (NIL)
:indoc ' ("Sdif-File.")
:icon '17359
:doc ""

(build-sound-mix-fun sound))


;; ====================================================


(defmethod! sound-transpose-sox ((sound sound) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(if (equal 0 cents) sound (ckn-transpose-a-sound (namestring (file-pathname sound)) cents)))

;; ====================================================


(defmethod! sound-transpose-sox ((sound null) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(sound-silence-sox 1.0 1))

;; ====================================================

(defmethod! sound-transpose-sox ((sound string) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(if (equal 0 cents) sound (ckn-transpose-a-sound sound cents)))

;; ====================================================


(defmethod! sound-transpose-sox ((sound string) (cents null))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

sound)

;; ====================================================

(defmethod! sound-transpose-sox ((sound pathname) (cents number))
:initvals ' (NIL)
:indoc ' ("Pathname of a sound-file" "Tranposition in cents")
:icon '17359
:doc ""

(if (equal 0 cents) (namestring sound) (ckn-transpose-a-sound (namestring sound) cents)))

;; ====================================================

(defmethod! sound-mono-to-stereo-sox ((sound sound))
:initvals ' ("NIL")
:indoc ' ("One mono sound")
:icon '17359
:doc ""
(sound-mono-to-stereo-sox-fun (namestring (file-pathname sound))))

;; ===============


(defmethod! sound-mono-to-stereo-sox ((sound list))
:initvals ' ("NIL")
:indoc ' ("One mono sound")
:icon '17359
:doc ""
(loop :for x :in sound :collect (sound-mono-to-stereo-sox x)))

;; ===============

(defmethod! sound-mono-to-stereo-sox ((sound string))
:initvals ' ("NIL")
:indoc ' ("One mono sound")
:icon '17359
:doc ""
(sound-mono-to-stereo-sox-fun sound))

;; ===============

(defmethod! sound-mono-to-stereo-sox ((sound pathname))
:initvals ' ("NIL")
:indoc ' ("One mono sound")
:icon '17359
:doc ""
(sound-mono-to-stereo-sox-fun (namestring sound)))

;; ===============

(om::defmethod! choose ((notelist list) (chord-n list))
:initvals ' ((1 2 3 4 5 6 7 8 9 10) 2)
:indoc ' ("List or list of lists of anything" "What element(s) do you want?") 
:icon '17359
:doc "This object choose an element in a list; or a list in a list of lists. If you put more that one element in the second inlet this object will choose all the elements that you put in second inlet.
Inlet1: (7 8 9 10 458)
Inlet2: (1 3 5)
Result: (7 9 458)."

(if (equal nil chord-n) nil (posn-match notelist (om::om- chord-n 1))))

;; ====================================================

(om::defmethod! choose ((notelist list) (chord-n number))
:initvals ' ((1 2 3 4 5 6 7 8 9 10) 2)
:indoc ' ("List or list of lists of anything" "What element(s) do you want?") 
:icon '17359
:doc "This object choose an element in a list; or a list in a list of lists. If you put more that one element in the second inlet this object will choose all the elements that you put in second inlet.
Inlet1: (7 8 9 10 458)
Inlet2: (1 3 5)
Result: (7 9 458)."

(if (equal nil chord-n) nil (posn-match notelist (om::om- chord-n 1))))

;; ====================================================

(defmethod! choose-to-rest ((voice voice) &optional (number-2 1))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(ckn-action1  (loop :for ckn-plus :in (om6-true-durations voice) :collect (if (plusp ckn-plus) 0 1)))
(ckn-action2 (loop :for cknloop :in ckn-action1 :collect (if (= 0 cknloop) (setq number-2 (+ number-2 1)) nil))))
(let* (
     (ckn-action3-1 
     (if (equal nil (first ckn-action2)) 0 (first ckn-action2))))
     (if (equal nil (first ckn-action2)) (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) (om::om+ (om::om- ckn-action2 ckn-action3-1) 1))          
      )))


;; ====================================================

(defmethod! ckn-position ((list list) (my-number number))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "Check the ALL the position of one number in one list."

(let* (
(ckn-action1  (loop :for ckn-loop :in list 
                    :for my-position :in (om::arithm-ser 1 (length list) 1)
                    :collect 
                          (if (equal ckn-loop my-number) my-position nil))))
(remove nil ckn-action1)))


;; ====================================================

(defmethod! ckn-position ((list list) (my-number list))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "Check the ALL the position of one number in one list."

(let* (
(ckn-action1  (loop :for ckn-loop :in list 
                    :for my-position :in (om::arithm-ser 1 (length list) 1)
                    :collect 
                          (if (equal ckn-loop my-number) my-position nil))))
(remove nil ckn-action1)))

;; ====================================================

(defmethod! choose-to-rest ((list list) &optional (number-2 1))
:initvals '(nil nil)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(ckn-action1  (loop :for ckn-plus :in list :collect (if (plusp ckn-plus) 0 1)))

(ckn-action2 (loop :for cknloop :in ckn-action1 :collect (if (= 0 cknloop) (setq number-2 (+ number-2 1)) nil))))


(let* (
     (ckn-action3-1 
     (if (equal nil (first ckn-action2)) 0 (first ckn-action2))))
     (if (equal nil (first ckn-action2)) (om::om+ (om::om- ckn-action2 ckn-action3-1) -1) (om::om+ (om::om- ckn-action2 ckn-action3-1) 1))          
      )))

;; ====================================================

(defmethod! ckn-list->string ((list list))
:initvals '(nil nil)
:indoc '("A list of names?") 
:icon '17359
:doc "Transform a list in one string."

(list->string-fun list))


;; ====================================================
(defmethod! ckn-temp-sounds ((sounds sound) &optional (string string))
:initvals '(nil)
:indoc '("one string") 
:icon '17359
:doc "Copy some file to outfiles."

(car (save-temp-sounds (list! sounds) string)))

;; ====================================================
(defmethod! ckn-temp-sounds ((sounds list) &optional (string string))
:initvals '(nil)
:indoc '("one string") 
:icon '17359
:doc "Copy some file to outfiles."

(save-temp-sounds sounds string))


;; ====================================================
(defmethod! copy2outfile ((path string))
:initvals '(nil)
:indoc '("one string") 
:icon '17359
:doc "Copy some file to outfiles."

(ckn-copy2outfile path))


;; ====================================================
(defmethod! copy2outfile ((path pathname))
:initvals '(nil)
:indoc '("one pathname") 
:icon '17359
:doc "Copy some file to outfiles."

(ckn-copy2outfile (namestring path)))

;; ==============================================

(defmethod! remove-nth-element ((lists list) (numbers list))
:initvals '(nil nil)
:indoc '("A list of names?") 
:icon '17359
:doc "Transform a list in one string."

(let* (
      (action1 (remove-nth-element-fun (car numbers) lists))
      (action2 (cdr numbers)))
      (if (not (cdr numbers))
          action1
          (setf lists (remove-nth-element action1 action2)))))
            
;; ====================================================

(defmethod! voice->samples ((voice voice) &optional (pan nil) (temp-files t))
:initvals '(nil nil t)
:indoc '("a voice" "panoramic information - see the object sound-stereo-pan" "Clear temp files") 
:icon '17359
:doc "Imported from OM6. It can take."

(om-print "Aguarde!" "Verbose")

;; (clear-subdir-temp-files "om-ckn")

(if (equal (check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")

(if (om-print (equal *app-name* "om-sharp") "app-name")
    (voice->samples-sound-fun voice pan temp-files)
    (voice->samples-sound-om6-fun voice pan temp-files))

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))

;; ====================================================

(defmethod! voice->samples-ITD ((voice voice) &optional (pan nil) (temp-files t))
:initvals '(nil nil t)
:indoc '("a voice" "panoramic information - see the object sound-stereo-pan" "Clear temp files") 
:icon '17359
:doc "Imported from OM6. It can take."

(om-print "Aguarde!" "Verbose")

(clear-subdir-temp-files "om-ckn")

(if (equal (check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")

(if (om-print (equal *app-name* "om-sharp") "app-name")
    (voice->samples-sound-ITD-fun voice pan temp-files))

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))


;; ====================================================

(defmethod! sound-vol-sox ((sounds sound) (volume number))
:initvals '(nil)
:indoc '("sound" "volume in this format 0.9 that means 90% of the original sound.") 
:icon '17359
:doc "It does the same that sound-vol, but it uses the SoX."

(sound-vol-sox-fun (file-pathname sounds) volume))

;; ====================

(defmethod! sound-vol-sox ((sounds pathname) (volume number))
:initvals '(nil)
:indoc '("sound" "volume in this format 0.9 that means 90% of the original sound.") 
:icon '17359
:doc "It does the same that sound-vol, but it uses the SoX."

(sound-vol-sox-fun sounds volume))

;; ====================

(defmethod! sound-vol-sox ((sounds string) (volume number))
:initvals '(nil)
:indoc '("sound" "volume in this format 0.9 that means 90% of the original sound.") 
:icon '17359
:doc "It does the same that sound-vol, but it uses the SoX."

(sound-vol-sox-fun sounds volume))

;; ====================

(defmethod! sound-vol-sox ((sounds string) (volume null))
:initvals '(nil)
:indoc '("sound" "volume in this format 0.9 that means 90% of the original sound.") 
:icon '17359
:doc "It does the same that sound-vol, but it uses the SoX."

(sound-vol-sox-fun sounds (if (equal nil volume) 1 1)))

;; ====================================================
(defmethod! sound-mix-sox ((sounds list) (name string))
:initvals '("sound-mix.wav")
:indoc '("sounds") 
:icon '17359
:doc "It does the same that sound-mix and sound-mix-list."

(if (< (length (om::list! sounds)) 23)
    (sound-mix-sox-fun sounds name)
    (sound-mix-sox-responsive sounds (string+ name "responsive") 00001)))

;; ====================================================
(defmethod! sound-seq-sox ((sounds list) (name string))
:initvals '(nil "sox-seq.wav")
:indoc '("list of pathnames of sounds") 
:icon '17359
:doc "It does the same that sound-seq and sound-seq-list."

(if (< (length sounds) 21)
    (sound-seq-sox-fun sounds name)
    (sound-seq-sox-responsive sounds (string+ name "seq-res-") 00001)))

;; ====================================================
(defmethod! sound-dur-sox ((sounds string))
:initvals '(nil)
:indoc '("sounds" "list with fade-in and fade-out") 
:icon '17359
:doc "It does the same that sound-fade."

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (file-out-name (om::string+ "sound-dur-" (write-to-string (om::om-random 0 10000)) ".txt"))
  (line-command (string+ sox-path " " (list->string-fun (list (namestring sounds))) " -n stat " " 2>" (list->string-fun (list (namestring (tmpfile file-out-name :subdirs "om-ckn"))))))
  (the-command (ckn-cmd-line line-command))
  (loop-until-probe-file (tmpfile file-out-name :subdirs "om-ckn")))
  (let* (
          (ckn-sound-dur (read-dur-informations (tmpfile file-out-name :subdirs "om-ckn"))))
          (ckn-clear-the-file (tmpfile file-out-name :subdirs "om-ckn"))
          ckn-sound-dur)))

;; ====================================================
(defmethod! sound-dur-sox ((sounds pathname))
:initvals '(nil)
:indoc '("sounds" "list with fade-in and fade-out") 
:icon '17359
:doc "It does the same that sound-fade."

(sound-dur-sox (namestring sounds)))

;; ====================================================
(defmethod! sound-fade-sox ((sounds pathname) (fade list))
:initvals '(nil)
:indoc '("sounds" "list with fade-in and fade-out") 
:icon '17359
:doc "It does the same that sound-fade."
(sound-fade-sox-fun sounds fade))

;; ====================================================
(defmethod! sound-fade-sox ((sounds string) (fade list))
:initvals '(nil)
:indoc '("sounds" "list with fade-in and fade-out") 
:icon '17359
:doc "It does the same that sound-fade."
(sound-fade-sox-fun sounds fade))

;; ====================================================
(defmethod! sound-silence-sox ((sounds number) &optional (channels 1))
:initvals '(nil)
:indoc '("float number") 
:icon '17359
:doc "It does the same that sound-silence but using sox."

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (silence (tmpfile (string+ "silence-" (format nil "~d" sounds)   ".wav") :subdirs "om-ckn"))
  (sound-in-out (list (namestring silence)))
  (line-command 
    (string+ sox-path " " "-n " (format nil " -c ~d " channels) " -r 44100 " " " (list->string-fun sound-in-out) " trim 0 " (format nil "~d" (abs sounds)))))
  
(if (not (probe-file silence))
    (ckn-cmd-line line-command)
    silence)
  (loop-until-probe-file silence)
  (car sound-in-out)))

;; SOX -n "C:\Users\neimog\OneDrive - design.ufjf.br\Documentos\OM - Workspace\out-files\om-ckn\silence.wav" trim 0 10

;; ====================================================

(defmethod! sound-cut-sox ((sounds string) (in number) (out number))
:initvals '(nil)
:indoc '("float number") 
:icon '17359
:doc "It does the same that sound-silence but using sox."

(sound-cut-sox-fun sounds in out))

;; ====================================================

(defmethod! sound-cut-sox ((sounds string) (in string) (out number))
:initvals '(nil)
:indoc '("float number") 
:icon '17359
:doc "It does the same that sound-silence but using sox."

(sound-cut-sox-fun sounds in out))

;; ====================================================

(defmethod! sound-denoise-sox ((sound sound))
:initvals '(nil)
:indoc '("Sound") 
:icon '17359
:doc "It will denoise the sound using sox. You need to put two markers in the sound because sox need to do a probile of the noise. So mark the noise part of the sound."

(sound-denoise-sox-fun sound))


; ===========================================================================

(defmethod! sound-markers-cut ((sound sound))
:initvals ' ((nil) -60)       
:indoc ' ("A sound.")
:outdoc ' ("The new sound")
:icon '17359
:doc "It will cut one sound using the TWO FIRST markers."

(let* (
      (sound-markers (om::markers sound)))
  (if (null sound-markers)
      sound 
     (sound-fade (om::sound-cut sound (first sound-markers) (second sound-markers)) 0.01 0.01))))

;; ====================================================

(defmethod! sound-seq-multi ((sounds list) &optional (list-per-threading 30))
:initvals '(nil)
:indoc '("a list of sounds." "Among of sounds per threading.")
:icon '17359
:doc "Like sound-seq-list but multithreading (more fast)."
        (gc-all)
    (let* (
    (action1 (sound-seq-list-multi-threading (build-seq-of-sounds sounds list-per-threading))))
    (om::om-cmd-line (string+ "powershell -command " 
                              (list->string-fun (list (string+ "del " 
                                                (list->string-fun (list (namestring (merge-pathnames "om-ckn/*.aif" (outfile ""))))))))))
            (gc-all)
        action1))

;; ==================================================== NOTES AND SCORE ==================

(defmethod! ckn-add-extras ((voice voice))
:initvals '(nil)
:indoc '("Add some notehead in OM-Score") 
:icon '17359
:doc ""

(ckn-add-extras voice))

;; =======================================

(defmethod! ckn-voice ((tree list) (chords list) (lvel list) (lchan list) (tempo number))
:initvals '(nil)
:indoc '("voice without drawing?") 
:icon '17359
:doc "Build a voice."

(make-instance 'voice :tree tree :lmidic chords :lvel lvel :lchan lchan :tempo tempo))

; ====================================================== MICROTONAL PLAYER =================

(om::defmethod! ckn-voice->text ((voice voice))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."

(voice->coll voice 1))

; (compile 'voice->coll)
;(compile 'ckn-voice->text)
; ======================

(om::defmethod! osc-play ((voice voice))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."

(let* (
        (ckn-action1 (remove nil (voice->coll voice 1)))
        (x->dx (x->dx (mapcar (lambda (x) (car x)) ckn-action1))))
  (loop :for tocando :in ckn-action1
        :for x->dx_loop :in x->dx
        :do (let* () (om::osc-send (om::x-append '/real-time (print (cdr tocando))) "127.0.0.1" 3000)
                     (sleep (om::ms->sec x->dx_loop)))
    '("done"))))

; ===========================================================================

(om::defmethod! osc-play ((self chord-seq))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."


  (let* ( 
        (durations-of-the-chors (om::omquantify (om::x->dx (lonset self)) 1000 (list 4 4) 1024))
        (loop-notes (loop :for x :in (lmidic self) 
                          :for y :in (lonset self) 
                          :collect (if (not x) y (abs y))))
        (to-voice (make-instance 'voice :tree durations-of-the-chors :tempo 1000 :lmidic (remove nil (lmidic self)))))
(osc-play to-voice)))
       
; ===========================================================================

(om::defmethod! osc-play ((chord chord))
:initvals ' ((nil))       
:indoc ' ("A player for OM#")
:outdoc ' ("PLAY")
:icon '17359
:numouts 1
:doc "It is a player for OM#. You can download the Max/MSP patch in:  <https://bit.ly/32K0och>.

For the automatic work the folder out-files of OM# must be in the files preferences of the Max/MSP."

(let* (
    (chord-seq (make-instance 'chord-seq :lmidic (list chord))))
    (osc-play chord-seq)))

;; ==================================================== Utilitities

(defmethod* f->n ((freq list))
  :numouts 1
  :initvals (list 6000 nil)
  :indoc '("pitch or pitch list (midicents)" "frequency (Hz)")
  :icon 'conversion
  :doc "
Converts a (list of) freq pitch(es) to names of notes.
"

(mc->n (f->mc freq) 4))

;; ====================================================

(defmethod* ms->samples ((sec number) (sample-rate number))
  :numouts 1
  :initvals (list 6000 nil)
  :indoc '("numbers of seconds" "samples rate of the audio")
  :icon '141
  :doc "
Converts a (list of) seconds to milisseconds.
"
(om::om-round (sec->samples (om::om/ sec 1000) sample-rate)))

;; ==================================================== FFT APPROACH LIKE SPEAR =====================================

(defmethod! fft->chord-seq  ((ckn-fft-instance list) (down number) (up number) &optional (min-vel 10))
:initvals '(nil 3600 8400)
:indoc '("pitch or pitch list (midicents)" "frequency (Hz)")
:icon '17359
:doc "
Converts a (list of) freq pitch(es) to names of notes."


(let* (
    (fft->chords 
     (loop :for x 
           :in ckn-fft-instance 
           :collect (let* (
                        (amplitudes (amplitudes x))
                        (frequencias (frequencias x))
                        (freq-to-midicents (f->mc frequencias))
                        (lin->vel (om::om/ (om::om-scale amplitudes 10 127000 0 1) 1000)))
                        (make-instance 'chord
                                          :lmidic freq-to-midicents
                                          :lvel lin->vel))))
    (filter (lambda (x) 
              (let* (
                     (matrix-transformation (mat-trans (list (lmidic x) (lvel x))))
                     (lambda-filter (loop :for y :in matrix-transformation 
                                          :collect (let* (
                                                          (notas (first y))
                                                          (dinamicas (second y))
                                                          (boolean-if (and (om::om< notas up) (om::om> notas down) (om::om<= min-vel dinamicas))))
                                                     (if boolean-if (list notas dinamicas) nil)))))
                     (let* (
                            (remove-nil (remove nil lambda-filter))
                            (second-matrix-transformation (mat-trans remove-nil)))
                       (make-instance 'chord :lmidic (first second-matrix-transformation) :lvel (second second-matrix-transformation))))))
  (make-chords 
                (let* (
                    (action1 (mapcar filter fft->chords))
                    (action2 (om::om- (length action1) 2)))
                    (om::first-n action1 action2)))



  (make-chord-seq 
             (make-instance 'chord-seq 
                 :lmidic make-chords 
                 :lonset (list 0 (om::om-round (sec->ms (samples->sec (ckn-hop-size (first ckn-fft-instance)) (sound-sample-rate (first ckn-fft-instance))))))))
  (vel (flat (mapcar (lambda (x) (lvel x)) fft->chords))))
  (print (format nil "minimun velocity ~d" (om::list-min vel)))
  (print (format nil "maximum velocity ~d" (om::list-max vel)))
  make-chord-seq))

; ===========================================================================

(defmethod! fft->sin-model ((ckn-instances list) (db-filter number))
:initvals ' ((nil) -60)       
:indoc ' ("A list of ckn-fft-instance class." "Threshold in dB.")
:outdoc ' ("list of ckn-fft-instance with the approach of Spear software.")
:icon '17359
:doc ""

(fft->sin-model-fun ckn-instances db-filter))

;; ORCHIDEA INSTRUMENTS ===============================

(defmethod! o-voice2samples ((voice voice) &optional (pan nil) (temp-files t))
:initvals ' ((nil) '-60)       
:indoc ' ("A list of ckn-fft-instance class." "Threshold in dB.")
:outdoc ' ("list of ckn-fft-instance with the approach of Spear software.")
:icon '17360
:doc ""

(om-print "Aguarde!" "Verbose")
;; (clear-subdir-temp-files "om-ckn")
(if (equal (o-check-samples-in-voice voice) "Todas as alturas possuem samples correspondentes")
(if (equal *app-name* "om-sharp")
    (o-voice->samples-sharp voice pan temp-files)
    )

(let* ((action1 (print "Not able to find all the samples")))
                (om-abort))))


;; Multithreading ====================================================

(defmethod! ckn-loop-multi-prepare ((list list) (how_many_threading list))
:initvals ' (nil nil)       
:indoc ' ("one list" "how much threading for time.")
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."


(loop-in-parts list how_many_threading how_many_threading))

;; =============================================

(defmethod! ckn-loop-multi-prepare ((list list) (how_many_threading number))
:initvals ' (nil nil)       
:indoc ' ("one list" "how much threading for time.")
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."


(loop-in-parts list how_many_threading how_many_threading))

;; ====================================================

(defmethod! ckn-multi-1-var ((ckn-lambda function) (list list) &optional (loop-inside 0))
:initvals ' (nil nil)       
:indoc ' ("one function" "one list" "loop inside function?")
:menuins '((2 (("yes" 1) ("no" 0))))
:outdoc ' ("result")
:icon 'multithreading 
:doc "It does multithreading loops, do not use it if you REALLY do not need :) ."

(let* (
      (list-of-something (if (equal loop-inside 0) list (mapcar (lambda (x) (list x)) list)))
      (action1 (ckn-mailbox-name list-of-something))
      (action2 (ckn-make-mail-box action1)))
(loop 
    :for list-of-something-loop :in list-of-something 
    :for create-mailbox :in action2
    :for names-process :in action1
    :for index :from 1 :to (length list) 
    :do (om::om-print (format nil "Thread ~d of ~d" index (length list)) "OM-CKN")  
    :do 
        (mp:process-run-function names-process
                 () 
                  (lambda (x y) (mp:mailbox-send x (mapcar ckn-lambda (om::list! y)))) create-mailbox (list list-of-something-loop)))
(loop-until-finish-process action2)
(ckn-mailbox-peek action2)))


;; ==================================================

; === 

(defmethod! sonic-visualizer ((sound string))

(sonic-visualizer (probe-file sound)))

; === 

(defmethod! sonic-visualizer ((sound sound))

(sonic-visualizer (namestring (car (if (not (om::file-pathname sound)) 
                                        (om::list! (save-temp-sounds sound (om::string+ "format-" (format nil "~7,'0D" (om-random 0 999999)) "-")))
                                        (om::list! (om::file-pathname sound)))))))

;; =============================================

(defmethod! sonic-visualizer ((sound pathname))
:initvals ' (nil nil)       
:indoc ' ("Sound File")
:outdoc ' ("result")
:icon 'sonicvisualizer 
:doc "Open audio files with Sonic Visualizer."


(mp:process-run-function (string+ "Opening SonicVisualizer!") ;; Se não, a interface do OM trava
                                   () 
                                          (lambda () 
                                              (let* ()
                                                (om::om-cmd-line 
                                                  (om::string+ 
                                                      (list->string-fun (list (namestring (get-pref-value :externals :Sonic-visualizer)))) " " 
                                                      (list->string-fun (list (namestring sound))))))
                                              (print "Closing SonicVisualizer"))))
