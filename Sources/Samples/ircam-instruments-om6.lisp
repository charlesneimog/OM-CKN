(in-package :om)

;; ==================================================== Preferencias ==================================

(defmethod! ircam-samples-folder ((x string))
:initvals '(nil)
:indoc '("Name of the composer" "Name of the piece") 
:icon '17359
:doc "This object define the name of the composer and the name of the piece."

(defparameter *IRCAM-PATH* (namestring x))
(om-save-pathname (namestring *IRCAM-PATH*))
*IRCAM-PATH*)


;; ==================================================== Preferencias ==================================

#| 
(add-preference-section :externals "OM-CKN" nil '(:ircam-instruments :MrsWatson-exe :sox-exe :plugins ::fxp-presets))
(add-preference :externals :ircam-instruments "Ircam Instruments Path" 
                :path nil)

|#

;; ==================================================== Utilidades ==================================
(defun ircam-n->mc (x)

(let* (
       (action1 (string-to-list x "-"))
       (action2 (om::om- (length action1) 2))
       (action3 (om::last-n action1 action2))
       (action4 (om::first-n action3 (1- (length action3)))))

(loop :for ckn-action5 :in action4 
      :collect 
      (let* (
            (action5-1 (remove '#\+ ckn-action5))
            (action5-2 (om::n->mc action5-1 4))
            (action5-3 (if 
                           (equal action5-1 ckn-action5)
                           action5-2
                         (om::om+ action5-2 50))))
        action5-3))))

;; ==================================================== 

(defun ckn-multiphonics-notes (all-names notes)
      (let* (
            (action1 (mapcar (lambda (x) (ircam-n->mc x)) all-names))
            (action2 (mapcar (lambda (x) 
                              (loop :for loop-action2 :in action1 
                                    :collect (find-num loop-action2 x))) 
                                                                              (om::approx-m notes 4)))
            (action3 (mapcar (lambda (x) (remove nil x)) (om::mat-trans action2)))
            (action4 (mapcar (lambda (x) (if (atom x) 0 (length x))) action3))
            (action5 (car (om::sort-list (om::remove-dup action4 'eq 1) :test '>)))
            (action6 (ckn-position action4 action5))
            (action7 
                  (if (equal action6 nil)
                        (let* ()
                              (om-print "Nao ha nenhuma nota corresponde nos multifonicos disponiveis, escolhendo aleatoriamente" "WARNING!!!")
                              (om::nth-random all-names))
                        
                        (if (om::om< (length action6) 2)
                              (let* ()  
                                    (om-print 
                                          (string+ "O multifonico com mais notas em comum é esse" 
                                                            (first (choose all-names action6)))
                                          "Notice!")
                                    (first (choose all-names action6)))

                              (let* ()  
                                    (om-print "Ha alguns multifonicos com a mesma quantidade de notas em comum, escolhendo aleatoriamente entre eles." "Notice!")
                                    (choose all-names (om::nth-random action6)))))))
action7))
            
                        

;; ==================================================== 

(defmethod! check-samples-in-voice ((voice voice))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(midis-no-om6 (make-instance 'chord-seq :lmidic (chords voice)))
(notas (approx-m (flat (lmidic midis-no-om6)) 2))
(canais (flat (lchan midis-no-om6)))
(vel (flat (lvel midis-no-om6)))

(test 
 (loop :for loop-notas :in notas
      :for loop-canais :in canais
      :for loop-vel :in vel
      :collect 
      (equal nil (ircam-instruments loop-notas loop-canais loop-vel)))))

(if (equal nil (remove nil test)) "Todas as alturas possuem samples correspondentes" 
(format nil "A nota ~d nao possuem sample correspondente! :( " (1+ (position t test))))))

;; ==================================================== 

#|
(defmethod!  ircam-instruments ((notes list) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound."

(case number-of-the-instrument
  (14 (Fl-multi notes))
  (61 (Cl-multi notes))))


|#
;; ==================================================== 

(defmethod!  ircam-instruments ((note integer) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound.

============================  FLUTE ============================

||| 01 = Flute Aeolian   ||| 02 = Flute Aeolian+Ordinario ||| 03 = Flute Aeolian-to-ordinário 
||| 05 = Flute crescendo ||| 06 = Flute-cres-to-desc      ||| 07 = Flute-decr     
||| 08 = Flute Disc fing ||| 09 = Fl-flatt                ||| 10 = Fl-harm-fngr
||| 11 = Fl-harm-fngr    ||| 12 = Fl-jet-wh               ||| 13 = Fl-key-click    
||| 14 =  Fl-multi       ||| 16 = Fl-ord                  ||| 17 =  fl-ord-quarter-tone
||| 18 = Fl-ord_aeol     ||| 19 = Fl-ord_flatt            ||| 20 = Fl-pizz 21  Fl-ply+sng 
||| 22 = Fl-ply+sng-uni  ||| 23 = Fl-sfz                  ||| 24 = Fl-stacc 
||| 25 = Fl-tongue-ram   ||| 26 = Fl-trill-maj2           ||| 27 =  Fl-trill-min2 
||| 28 =  Fl-whst-tn     ||| 29 =  Fl-whst-tn-sw-slw      
_______________________________________________________________________

============================= OBOE ===============================

||| 30  = blow-without           ||| 31  = chromatic-scale            ||| 32  = crescendo 
||| 33  = crescendo-to-desc      ||| 34  = decrescendo                ||| 35  = discolored-fingering 
||| 36  = double-trill-major     ||| 37  = double-trill-minor         ||| 38  = flatt  
||| 39  = harmonic-fing          ||| 40  = Ob-key-click               ||| 41  = kiss 
||| 42  = clip-glissando         ||| 43  = multiphonics               ||| 44  = ob-multi        
||| 45  = note-lasting           ||| 46  = Ob-ord                     ||| 47  = ordinario-1q  
||| 48  = sforzando              ||| 49  = Ob-stacc                   ||| 50  =  trill-major-second  
||| 51  =  trill-major           ||| 52  =  vibrato 

"

(case number-of-the-instrument

;; Flute 

(1 (Fl-aeol note))
(2 (Fl-aeol+ord note velocity))
(3 (Fl-aeol-to-ord note))
(5 (Fl-cresc note))
(6 (Fl-cresc-to-decr note))
(7 (Fl-decr note))
(8 (Fl-disc-fing note))
(9 (Fl-flatt note velocity))
(10 (Fl-flatt-to-ord note))
(11 (Fl-harm-fngr note velocity))
(12 (Fl-jet-wh note))
(13 (Fl-key-click note))
(14 (Fl-multi (list note)))
(15 (print "I need to implement"))
(16 (Fl-ord note velocity))
(17 (fl-ord-1q note velocity))
(18 (Fl-ord_aeol note))
(19 (Fl-ord_flatt note))
(20 (Fl-pizz note))
(21 (Fl-ply+sng note))
(22 (Fl-ply+sng-uni note))
(23 (Fl-sfz note))
(24 (Fl-stacc note))
(25 (Fl-tongue-ram note))
(26 (Fl-trill-maj2 note))
(27 (Fl-trill-min2 note))
(28 (Fl-whst-tn note))
(29 (Fl-whst-tn-sw-slw note))

;; Oboé ================================================================

#| 
(30 (blow-without))
(31 (chromatic-scale))
(32 (crescendo))
(33 (crescendo-to-desc))
(34 (decrescendo))
(35 (discolored-fingering))
(36 (double-trill-major))
(37 (double-trill-minor))
(38 (flatt))
(39 (harmonic-fing))
|#
(40 (Ob-key-click note))
#| 
(41 (kiss))
(42 (lip-glissando))
(43 (multiphonics))
|#
(44 (ob-multi (list note)))
#| 
(45 (note-lasting))
|#
(46 (Ob-ord note velocity))
#| 
(47 (ordinario-1q))
(48 (sforzando))
|#
(49 (Ob-stacc note))
#| 

(50 (trill-major-second))
(51 (trill-major0))
(52 (vibrato))

;; Clarinete  ================================================================

(53 (aeoliand-and-ordinario))
(54 (crscendo))
(55 (cresecndo-to-de))
(56 (descr))
(57 (flatt))
(58 (flatt-high))
(59 (glisand))
(60 (key-click))
|#
(61 (Cl-multi (list note)))
#| 
(62 (note-lasting)) 

|#
(63 (Cl-ord note velocity))
#| 

(64 (ord-1q))
(65 (ordinario-high))
(66 (sforzando))
(67 (staccato))
(68 (trill-major))
(69 (trill-minor))

;; Basson  ================================================================

(70 (blow-with))
(71 (chromatic-scale))
(72 (crescendo))
(73 (crscendo-to-des))
(74 (descrecndo))
(75 (flatter))
(76 (glissando))
(77 (harmonic))
(78 (key-click))
(79 (multiphonis))
(80 (note-lasting))
(81 (ordinario))
(82 (ordinario-1q))
(83 (sforzando))
(84 (sordina))
(85 (staccato))
(86 (trill-major))
(87 (trill-minor))
(88 (vibrato))

;; Saxophone  ================================================================

(89 (aeolian))
(90 (backwards))
(91 (bisbigliando))
(92 (blow-without))
(93 (chromatic-scale))
(94 (crescendo))
(95 (crescendo-to-desc))
(96 (descrescendo))
(97 (discolored-fing))
(98 (double-tonguin))
(99 (exploding))
(100 (flatter))
(101 (flatter-to-ord))
(102 (glissando))
(103 (harmonic-fin))
(104 (harmonic-gliss))
(105 (key-click))
(106 (kiss))
(107 (move-bell-from-down-up))
(108 (move-bell-left-to-right))
(109 (multi))
(110 (ord))
(111 (ord-1q))
(112 (ord-high))
(113 (ord-to-flat))
(114 (play-and-sing-gli))
(115 (play-and-sing-m2-up))
(116 (play-and-sing-unison))
(117 (sforzando)) |#


(118 (Asax-slap note velocity))

#|
(119 (slap-unpitched))
(120 (staccato))
(121 (trill-major))
(122 (trill-minor))

;; 06 French Horn  ================================================================

(123 (brassy))
(124 (brassy-to-ordinario))
(125 (chromatic-scale))
(126 (crescendo)
(127 (crescendo-to-decresdenco))
(128 (decrescendo))
(129 (flatt))
(130 (flat-stopped))
(131 (flt-to-ord))
(132 (mute flt))
(133 (mute-ord))
(134 (note-last))
(135 (open-to-stopped))
(136 (ordinario))
(137 (ord-to-brassy))
(138 (ord-to-fltt-))
(139 (sforzando))
(140 (slap-pitched))
(141 (staccati))
(142 (stopped))
(143 (stiooed-to-open))
(144 (trill0major-seg))
(145 (trill-minor))

;; 07 trumpet ================================================================

|# 
(162 (CTp-ord note velocity)) 

#| 
145 + 33 = 178

;; 08 Trombone ================================================================

178 + 28 = 206

;; 09 Tuba ================================================================

206 + 43 = 249

;; 10 acordion ================================================================ 
249 + 13 = 262

;; 11 Guitar ================================================================

262 + 19 = 281
|#

(271 (Gtr-ord note velocity))
(274 (Gtr-pizz-bartok note))

;; 12 Harp  ================================================================

(300 (Hp-ord note velocity))

;; 13 Violin  ================================================================

(328 (Violin-pizz-secco note velocity))

;; 13 Viola  ================================================================

;; 349 - 394 

;; 14 Violoncello  ================================================================

;; 395 - 440 

(425 (Vc-pizz-lv note velocity))
(419 (Vc-ord note velocity))

;; 14 Contrabaixo  ================================================================


(497 (Cb-pizz-lv note velocity))



(nil nil)))


;; ==================================================== FLUTE ====================================================

; 001

(defmethod!  Fl-aeol ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute/aeolian/" "Fl-aeol-" (ckn-mc->n note) "-p" ".aif")))

; =======================

; 002
(defmethod!  Fl-aeol+ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/aeolian-and-ordinario/" "Fl-aeol+ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

; 003

(defmethod!  Fl-aeol-to-ord ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/aeolian-to-ordinario/" "Fl-aeol_ord-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod!  Fl-cresc ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/crescendo/" "Fl-cresc-" (ckn-mc->n note) "-ppff" ".aif")))


; =======================

; 005 

(defmethod!  Fl-cresc-to-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/crescendo-to-decrescendo/" "Fl-cre_dec-" (ckn-mc->n note) "-ppmfpp" ".aif")))

; =======================

; 006 

(defmethod!  Fl-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/decrescendo/" "Fl-decresc-" (ckn-mc->n note) "-ffpp" ".aif")))

; =======================

; 007 

(defmethod!  Fl-disc-fing ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/discolored-fingering/" "Fl-dsclrd-fngr-" (ckn-mc->n note) "-mf" ".aif")))

; 008
; =======================

(defmethod!  Fl-flatt ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/flatterzunge/" "Fl-flatt-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

; 009 

(defmethod!  Fl-flatt-to-ord ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/flatterzunge-to-ordinario/" "Fl-flatt_ord-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

; 010 

(defmethod!  Fl-harm-fngr ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-p" "-f") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/harmonic-fingering/" "Fl-harm-fngr-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 2)
      (if (> velocity 60) (car (last action2)) (first action2))
      (nth-random action2))))

; =======================
; 011

(defmethod!  Fl-jet-wh ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/jet-whistle/"  "Fl-jet-wh" ".aif")))

; =======================

; 012 

(defmethod!  Fl-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/key-click/"  "Fl-key-cl-"  (ckn-mc->n note) "-f" ".aif")))

; =======================

; 020

(defmethod!  Fl-pizz ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file. The number in Ircam Instruments is 20."

(probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute/pizzicato/" "Fl-pizz-" (ckn-mc->n note) "-f" ".aif")))

; =======================

(defmethod! Fl-tongue-ram ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute/tongue-ram/" "Fl-tng-ram-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/ordinario/" "Fl-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))


; =======================

(defmethod! Fl-ord-1q ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (correction (if (om= (om- note (approx-m (1- note) 2)) 50) "+" "-"))
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/ordinario-1q/" "Fl-ord-" (ckn-mc->n (approx-m (1- note) 2)) correction y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))


; =======================

(defmethod! Fl-ord_aeol ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/ordinario-to-aeolian/" "Fl-ord_aeol-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ord_flatt ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/ordinario-to-flatterzunge/" "Fl-ord_flatt-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ply+sng ((note list))
:initvals '(nil)
:indoc '("Two midicents notes, The first will be sing and the second played.") 
:icon '17359
:doc "It reads a wave file from ircam instruments."

(probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute/play-and-sing/" "Fl-ply+sng-" (ckn-mc->n (first note)) "+"  (ckn-mc->n (second note)) "-mf" ".aif")))

; =======================

(defmethod! Fl-ply+sng-uni ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/play-and-sing-unison/" "Fl-ply+sng-uni-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-sfz ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-fp" "-f") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "01 Flute/sforzando/" "Fl-sfz-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(nth-random action2)))

; =======================

(defmethod! Fl-stacc ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute/staccato/" "Fl-stacc-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-trill-maj2 ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/trill-major-second-up/" "Fl-trill-maj2-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-trill-min2 ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/trill-minor-second-up/" "Fl-trill-min2-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-whst-tn ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/whistle-tones/" "Fl-whst-tn-" (ckn-mc->n note) "-pp" ".aif")))

; =======================

(defmethod! Fl-whst-tn-sw-slw ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (namestring *IRCAM-PATH*) "01 Flute/whistle-tones-sweeping/" "Fl-whst-tn-sw-slw-" (ckn-mc->n note) "-pp" ".aif")))


; =======================

(defmethod! fl-multi ((notes list))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the dll plugins. Connect it to a LIST-SELECTION object."

(let* (
      (thepath (merge-pathnames "01 Flute\\multiphonics\\" (namestring *IRCAM-PATH*)))
      (thefilelist (om-directory thepath 
                              :type "aif" :directories nil :files t 
                              :resolve-aliases nil :hidden-files nil))
      (name-of-all-notes (mapcar (lambda (x) (get-filename x)) thefilelist))
      (multifonico-mais-parecido (ckn-multiphonics-notes name-of-all-notes notes)))
      (probe-file (string+ (namestring *IRCAM-PATH*) "01 Flute\\multiphonics\\" multifonico-mais-parecido))))


;; ==================================================== OBOE ====================================================

(defmethod! Ob-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (namestring *IRCAM-PATH*) "02 Oboe/key-click/" "Ob-key-cl-" (ckn-mc->n note) "-pp" ".aif")))

; =======================

(defmethod! Ob-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "02 Oboe/ordinario/" "Ob-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

(defmethod! Ob-stacc ((note integer))
:initvals '(6700)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (namestring *IRCAM-PATH*) "02 Oboe/staccato/" "Ob-stacc-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! ob-multi ((notes list))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the dll plugins. Connect it to a LIST-SELECTION object."

(let* (
      (thepath (merge-pathnames "02 Oboe\\multiphonics\\" (namestring *IRCAM-PATH*)))
      (thefilelist (om-directory thepath 
                              :type "aif" :directories nil :files t 
                              :resolve-aliases nil :hidden-files nil))
      (name-of-all-notes (mapcar (lambda (x) (get-filename x)) thefilelist))
      (multifonico-mais-parecido (ckn-multiphonics-notes name-of-all-notes notes)))
      (probe-file (string+ (namestring *IRCAM-PATH*) "02 Oboe\\multiphonics\\" multifonico-mais-parecido))))

;; ==================================================== CLARINETE ====================================================

(defmethod! Cl-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "03 Clarinet in Bb/ordinario/" "BbCl-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

(defmethod! Cl-multi ((notes list))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the dll plugins. Connect it to a LIST-SELECTION object.
If you have some error you need to rename these two multiphonics, BbCl-mul-D3-mf-1.aif and BbCl-mul-D3-mf-2, I suggest 1BbCl-mul-D3-mf and 2BbCl-mul-D3-mf.
"

(om-print "If you have some error you need to rename these two multiphonics, BbCl-mul-D3-mf-1.aif and BbCl-mul-D3-mf-2, I suggest 1BbCl-mul-D3-mf and 2BbCl-mul-D3-mf.")

(let* (
      (thepath (merge-pathnames "03 Clarinet in Bb\\multiphonics\\" (namestring *IRCAM-PATH*)))
      (thefilelist (om-directory thepath 
                              :type "aif" :directories nil :files t 
                              :resolve-aliases nil :hidden-files nil))
      (name-of-all-notes (mapcar (lambda (x) (get-filename x)) thefilelist))
      (multifonico-mais-parecido (ckn-multiphonics-notes name-of-all-notes notes)))
      (probe-file (string+ (namestring *IRCAM-PATH*) "03 Clarinet in Bb\\multiphonics\\" multifonico-mais-parecido))))


;; ==================================================== BASSOON ====================================================

;; ==================================================== SAXOPHONE ====================================================

(defmethod! Asax-slap ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-mf" "-f") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "05 Saxophone Alto in Eb/slap-pitched/" "ASax-slap-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 2)
      (if (> velocity 80) (car (last action2)) (first action2)))


(first action2)))

;; ==================================================== FRENCH HORN ================================================

;; ==================================================== TRUMPET ====================================================

(defmethod! CTp-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Integer in Midicents") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-ff" "-mf" "-pp") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "07 Trumpet in C/ordinario/" "CTp-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))


(if (equal (length action2) 3)
      (if (> velocity 80) (car action2) (if (> velocity 40) (second action2) (third action2)))


  (om::nth-random action2))))

;; ==================================================== GUITAR  ====================================================

(defmethod! Gtr-pizz-bartok ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
       (loop :for x :in '("1c" "2c" "3c" "4c" "5c" "6c") :collect 
             (probe-file (string+ (namestring *IRCAM-PATH*) "11 Guitar/pizzicato-bartok/" "Gtr-pizz-bartok-" (ckn-mc->n note) "-ff-" x ".aif"))))

      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

(defmethod! Gtr-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- " "5c- " "6c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "11 Guitar/ordinario/" "Gtr-ord-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

      ;; resolver questoes de dinâmicas



;; ==================================================== CELLO  ====================================================

(defmethod! Vc-pizz-secco ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- " "5c- " "6c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "15 Cello/pizzicato-secco/" "Vc-pizz-sec-" y x (ckn-mc->n note) ".aif"))))))
(remove nil (flat action1))))


;; =======================================

(defmethod! Cello-pizz-secco ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- " "5c- " "6c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*)
            "15 Cello/pizzicato-secco/" "Vc-pizz-sec-" y x (ckn-mc->n note) ".aif"))))))
(remove nil (flat action1))))


;; =======================================


;;Vc-ord



;; ==================================================== HARP  ====================================================

(defmethod! Hp-ord ((note integer) (velocity integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("pp" "mf" "ff") :collect 
        (probe-file (string+ (namestring *IRCAM-PATH*) 
            "12 Harp/ordinario/" "Hp-ord-" (ckn-mc->n note) "-" x ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

;; ==================================================== Violin  ====================================================

(defmethod! Violin-pizz-secco ((note integer) &optional (velocity 60))
:initvals '(6000)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "13 Violin/pizzicato-secco/" "Vn-pizz-sec-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

;; ==================================================== Cello  ====================================================

(defmethod! Vc-pizz-lv ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "15 Cello/pizzicato-l-vib/" "Vc-pizz-lv-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))





;; =======================================

(defmethod! Vc-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "15 Cello/ordinario/" "Vc-ord-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

;;Vc-ord



;; ==================================================== Contrabaixo  ====================================================

(defmethod! Cb-pizz-lv ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c" "2c" "3c" "4c") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ (namestring *IRCAM-PATH*) 
            "16 Contrabass/pizzicato-l-vib/" "Cb-pizz-lv-" (ckn-mc->n note) "-" y x ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))
