(in-package :om)

;; ==================================================== Preferencias ==================================

(defmethod! ircam-samples-folder ((composer string) (piece-name string))
:initvals '(nil)
:indoc '("Name of the composer" "Name of the piece") 
:icon '161
:doc "This object define the name of the composer and the name of the piece."

(defparameter *IRCAM-PATH* (namestring x)))

;; ==================================================== 

(defmethod! lily-variables ((composer string) (piece-name string))
:initvals '(nil)
:indoc '("Name of the composer" "Name of the piece") 
:icon '161
:doc "This object define the name of the composer and the name of the piece."


(defvar *lily-composer-name* composer)
(defvar *lily-title* piece-name)

(om-print *lily-composer-name*)
(om-print *lily-title*))


;; ==================================================== 

(defmethod! check-samples-in-voice ((voice voice))
:initvals '(nil)
:indoc '("Sound class") 
:icon 'sound
:doc "It create the patch of a sound."

(let* (
(ckn-acorde (make-instance 'chord-seq :lmidic (chords voice)))
(notas (approx-m (flat (lmidic ckn-acorde)) 2))
(canais (flat (lchan ckn-acorde)))
(vel (flat (lvel ckn-acorde)))

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
:doc "It create the patch of a sound."

(case number-of-the-instrument
(1 (Fl-aeol note))
(2 (Fl-aeol+ord note velocity))
(3 (Fl-aeol-to-ord note))
(5 (Fl-cresc note))
(6 (Fl-cresc-to-decr note))
(7 (Fl-decr note))
(8 (Fl-disc-fing note))
(9 (Fl-flatt note velocity))
(16 (Fl-ord note))
(20 (Fl-pizz note))
(25 (Fl-tongue-ram note))
(40 (Ob-key-click note))
(46 (Ob-ord note velocity))
(48 (Ob-stacc note))
(296 (Hp-ord note))
(267 (Gtr-ord note velocity))
(270 (Gtr-pizz-bartok note))
(1997 (Cb-pizz-lv note velocity))
(2021 (Violin-pizz-secco note velocity))
(nil nil)))


;; ==================================================== FLUTE ====================================================

; 001

(defmethod!  Fl-aeol ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file (string+ *IRCAM-PATH* "01 Flute/aeolian/" "Fl-aeol-" (ckn-mc->n note) "-p" ".aif")))

; =======================

; 002
(defmethod!  Fl-aeol+ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ *IRCAM-PATH* 
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
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/aeolian-to-ordinario/" "Fl-aeol_ord-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

; 004

(defmethod!  Fl-cresc ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/crescendo/" "Fl-cresc-" (ckn-mc->n note) "-ppff" ".aif")))

; =======================

; 005 

(defmethod!  Fl-cresc-to-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/crescendo-to-decrescendo/" "Fl-cre_dec-" (ckn-mc->n note) "-ppmfpp" ".aif")))

; =======================

; 006 

(defmethod!  Fl-decr ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/decrescendo/" "Fl-decresc-" (ckn-mc->n note) "-ffpp" ".aif")))

; =======================

; 007 

(defmethod!  Fl-disc-fing ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/discolored-fingering/" "Fl-dsclrd-fngr-" (ckn-mc->n note) "-mf" ".aif")))

; 008
; =======================

(defmethod!  Fl-flatt ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ *IRCAM-PATH* 
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
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/flatterzunge-to-ordinario/" "Fl-flatt_ord-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

; 010 

(defmethod!  Fl-harm-fngr ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-p" "-f") :collect
            (probe-file (string+ *IRCAM-PATH* 
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
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/jet-whistle/"  "Fl-jet-wh" ".aif")))

; =======================

; 012 

(defmethod!  Fl-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file 
      (string+ *IRCAM-PATH* "01 Flute/key-click/"  "Fl-key-cl-"  (ckn-mc->n note) "-f" ".aif")))

; =======================

; 012 

(defmethod!  Fl-pizz ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file (string+ *IRCAM-PATH* "01 Flute/pizzicato/" "Fl-pizz-" (ckn-mc->n note) "-f" ".aif")))

; =======================

(defmethod! Fl-tongue-ram ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file (string+ *IRCAM-PATH* "01 Flute/tongue-ram/" "Fl-tng-ram-" (ckn-mc->n note) "-mf" ".aif")))

; =======================

(defmethod! Fl-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ *IRCAM-PATH* 
            "01 Flute/ordinario/" "Fl-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))


;; ==================================================== OBOE ====================================================

(defmethod! Ob-key-click ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(probe-file (string+ *IRCAM-PATH* "02 Oboe/key-click/" "Ob-key-cl-" (ckn-mc->n note) "-pp" ".aif")))

; =======================

(defmethod! Ob-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for y :in '("-pp" "-mf" "-ff") :collect
            (probe-file (string+ *IRCAM-PATH* 
            "02 Oboe/ordinario/" "Ob-ord-" (ckn-mc->n note) y ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

(defmethod! Ob-stacc ((note integer))
:initvals '(6700)
:indoc '("Sound class") 
:icon 'ircam
:doc "It reads a wave file."

(probe-file (string+ *IRCAM-PATH* "02 Oboe/staccato/" "Ob-stacc-" (ckn-mc->n note) "-mf" ".aif")))

;; ==================================================== GUITAR  ====================================================

(defmethod! Gtr-pizz-bartok ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
       (loop :for x :in '("1c" "2c" "3c" "4c" "5c" "6c") :collect 
             (probe-file (string+ *IRCAM-PATH* "11 Guitar/pizzicato-bartok/" "Gtr-pizz-bartok-" (ckn-mc->n note) "-ff-" x ".aif"))))

      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

; =======================

(defmethod! Gtr-ord ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- " "5c- " "6c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ *IRCAM-PATH* 
            "11 Guitar/ordinario/" "Gtr-ord-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

      ;; resolver questoes de dinâmicas



;; ==================================================== CELLO  ====================================================

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


;; ==================================================== CELLO  ====================================================

(defmethod! Cb-pizz-lv ((note integer) &optional (velocity 60))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c" "2c" "3c" "4c") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ *IRCAM-PATH* 
            "16 Contrabass/pizzicato-l-vib/" "Cb-pizz-lv-" (ckn-mc->n note) "-" y x ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

;; ==================================================== HARP  ====================================================

(defmethod! Hp-ord ((note integer))
:initvals '(nil)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("ff" "mp" "pp") :collect 
        (probe-file (string+ *IRCAM-PATH* 
            "12 Harp/ordinario/" "Hp-ord-" (ckn-mc->n note) "-" x ".aif"))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))

;; ==================================================== Violin  ====================================================

(defmethod! Violin-pizz-secco ((note integer) &optional (velocity 60))
:initvals '(6000)
:indoc '("Sound class") 
:icon '12874613924
:doc "It reads a wave file."

(let* (
      (action1 
        (loop :for x :in '("1c- " "2c- " "3c- " "4c- ") :collect 
        (loop :for y :in '("pp-" "mf-" "ff-") :collect
            (probe-file (string+ *IRCAM-PATH* 
            "13 Violin/pizzicato-secco/" "Vn-pizz-sec-" y x (ckn-mc->n note) ".aif")))))
      (action2 (remove nil (flat action1))))

(if (equal (length action2) 3)
      (if (> velocity 87) (car (last action2)) (if (>= velocity 56) (second action2) (first action2)))
      (first action2))))
