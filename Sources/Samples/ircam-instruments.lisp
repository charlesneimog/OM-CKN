(in-package :om)

;; ==================================================== Preferencias ==================================

(add-preference-section :externals "OM-CKN" nil '(:ircam-instruments :MrsWatson-exe :sox-exe :plugins ::fxp-presets))
(add-preference :externals :ircam-instruments "Ircam Instruments Path" 
                :path nil)


;; ==================================================== 

(defmethod! check-samples-in-voice ((voice voice))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It create the patch of a sound."

(let* (
(notas (approx-m (flat (lmidic voice)) 2))
(canais (flat (lchan voice)))
(vel (flat (lvel voice)))

(test 
 (loop :for loop-notas :in notas
      :for loop-canais :in canais
      :for loop-vel :in vel
      :collect 
      (equal nil (ircam-instruments loop-notas loop-canais loop-vel)))))

(if (equal nil (remove nil test)) "Todas as alturas possuem samples correspondentes" 
(format nil "A nota ~d não possuem sample correspondente" (1+ (position t test))))))

;; ==================================================== 

(defmethod!  ircam-instruments ((note integer) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound.

============================  FLUTE ============================

||| 1 = Flute Aeolian   ||| 2 = Flute Aeolian+Ordinario ||| 3 = Flute Aeolian-to-ordinário 

||| 5 = Flute crescendo ||| 6 = Flute-cres-to-desc      ||| 6 = Flute-decr                 
_______________________________________________________________________

============================= OBOE ===============================

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
(14 (print "I need to implement"))
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
(44 (mute ordd))
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
(61 (multiphonics))
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

(300 (Hp-ord note))

;; 13 Violin  ================================================================

(328 (Violin-pizz-secco note velocity))

;; 13 Viola  ================================================================

;; 349 - 394 

;; 14 Violoncello  ================================================================

;; 395 - 440 

(425 (Vc-pizz-lv note velocity))

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

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/aeolian/" "Fl-aeol-" (ckn-mc->n note) "-p" ".aif")))

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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/aeolian-to-ordinario/" "Fl-aeol_ord-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod!  Fl-cresc ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/crescendo/" "Fl-cresc-" (ckn-mc->n note) "-ppff" ".aif")))


; =======================

; 005 

(defmethod!  Fl-cresc-to-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/crescendo-to-decrescendo/" "Fl-cre_dec-" (ckn-mc->n note) "-ppmfpp" ".aif")))

; =======================

; 006 

(defmethod!  Fl-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/decrescendo/" "Fl-decresc-" (ckn-mc->n note) "-ffpp" ".aif")))

; =======================

; 007 

(defmethod!  Fl-disc-fing ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/discolored-fingering/" "Fl-dsclrd-fngr-" (ckn-mc->n note) "-mf" ".aif")))

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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/flatterzunge-to-ordinario/" "Fl-flatt_ord-" (ckn-mc->n note) "-mf" ".aif")))

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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/jet-whistle/"  "Fl-jet-wh" ".aif")))

; =======================

; 012 

(defmethod!  Fl-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/key-click/"  "Fl-key-cl-"  (ckn-mc->n note) "-f" ".aif")))

; =======================

; 020

(defmethod!  Fl-pizz ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file. The number in Ircam Instruments is 20."

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/pizzicato/" "Fl-pizz-" (ckn-mc->n note) "-f" ".aif")))

; =======================

(defmethod! Fl-tongue-ram ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/tongue-ram/" "Fl-tng-ram-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/ordinario-to-aeolian/" "Fl-ord_aeol-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ord_flatt ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/ordinario-to-flatterzunge/" "Fl-ord_flatt-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ply+sng ((note list))
:initvals '(nil)
:indoc '("Two midicents notes, The first will be sing and the second played.") 
:icon '17359
:doc "It reads a wave file from ircam instruments."

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/play-and-sing/" "Fl-ply+sng-" (ckn-mc->n (first note)) "+"  (ckn-mc->n (second note)) "-mf" ".aif")))

; =======================

(defmethod! Fl-ply+sng-uni ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/play-and-sing-unison/" "Fl-ply+sng-uni-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-sfz ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-fp" "-f") :collect
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
            "01 Flute/sforzando/" "Fl-sfz-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(nth-random action2)))

; =======================

(defmethod! Fl-stacc ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/staccato/" "Fl-stacc-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-trill-maj2 ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/trill-major-second-up/" "Fl-trill-maj2-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-trill-min2 ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/trill-minor-second-up/" "Fl-trill-min2-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-whst-tn ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/whistle-tones/" "Fl-whst-tn-" (ckn-mc->n note) "-pp" ".aif")))

; =======================

(defmethod! Fl-whst-tn-sw-slw ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file 
      (string+ (get-pref-value :externals :ircam-instruments) "01 Flute/whistle-tones-sweeping/" "Fl-whst-tn-sw-slw-" (ckn-mc->n note) "-pp" ".aif")))


;; ==================================================== OBOE ====================================================

(defmethod! Ob-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "02 Oboe/key-click/" "Ob-key-cl-" (ckn-mc->n note) "-pp" ".aif")))

; =======================

(defmethod! Ob-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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

(probe-file (string+ (get-pref-value :externals :ircam-instruments) "02 Oboe/staccato/" "Ob-stacc-" (ckn-mc->n note) "-mf" ".aif")))

;; ==================================================== CLARINETE ====================================================

(defmethod! Cl-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
            "03 Clarinet in Bb/ordinario/" "BbCl-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
            "05 Saxophone Alto in Eb/slap-pitched/" "ASax-slap-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 2)
      (if (> velocity 80) (car (last action2)) (first action2)))


(first action2)))



;; ==================================================== GUITAR  ====================================================

(defmethod! Gtr-pizz-bartok ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
       (loop :for x :in '("1c" "2c" "3c" "4c" "5c" "6c") :collect 
             (probe-file (string+ (get-pref-value :externals :ircam-instruments) "11 Guitar/pizzicato-bartok/" "Gtr-pizz-bartok-" (ckn-mc->n note) "-ff-" x ".aif"))))

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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
            (probe-file (string+ *IRCAM-PATH*
            "15 Cello/pizzicato-secco/" "Vc-pizz-sec-" y x (ckn-mc->n note) ".aif"))))))
(remove nil (flat action1))))




;; ==================================================== HARP  ====================================================

(defmethod! Hp-ord ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '17359
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("ff" "mp" "pp") :collect 
        (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
            "15 Cello/pizzicato-l-vib/" "Vc-pizz-lv-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))


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
            (probe-file (string+ (get-pref-value :externals :ircam-instruments) 
            "16 Contrabass/pizzicato-l-vib/" "Cb-pizz-lv-" (ckn-mc->n note) "-" y x ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))
