(in-package :om)

;; ==================================================== Preferencias ==================================

(if (equal *app-name* "om-sharp")
    (defvar *IRCAM-PATH* (get-pref-value :externals :ircam-instruments))
    nil)

;; ===================================

(defmethod! ircam-samples-folder ((x string))
:initvals '(nil)
:indoc '("Name of the composer" "Name of the piece") 
:icon 'omckn-sound
:doc "This object define the name of the composer and the name of the piece."

      (defparameter *IRCAM-PATH* (namestring x))
      (om::om-save-pathname (namestring *IRCAM-PATH*))
*IRCAM-PATH*)

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
                        (progn
                              (om-print "Nao ha nenhuma nota corresponde nos multifonicos disponiveis, escolhendo aleatoriamente" "WARNING!!!")
                              (om::nth-random all-names))
                        (if (om::om< (length action6) 2)
                              (progn  
                                    (om-print 
                                          (string+ "O multifonico com mais notas em comum e esse" 
                                                            (first (choose all-names action6)))
                                          "Notice!")
                                    (first (choose all-names action6)))
                              (progn  
                                    (om-print "Ha alguns multifonicos com a mesma quantidade de notas em comum, escolhendo aleatoriamente entre eles." "Notice!")
                                    (choose all-names (om::nth-random action6)))))))
                  action7))
            
                        
;; ==================================================== 

(defmethod! check-samples-in-voice ((voice voice))
:initvals '(nil)
:indoc '("Sound class") 
:icon 'omckn-sound
:doc "It create the patch of a sound."

(let* (
      (midis-no-om6 (make-instance 'chord-seq :lmidic (chords voice)))
      (notas (approx-m (flat (lmidic midis-no-om6)) 2))
      (canais (flat (lchan midis-no-om6)))
      (vel (flat (lvel midis-no-om6)))
      (test-samples
            (loop :with not-found := nil
                  :for loop-notas :in notas
                  :for loop-canais :in canais
                  :for loop-vel :in vel
                  :do (setf not-found (flat (append not-found (list (FULL-SOL-instruments loop-notas loop-canais loop-vel)))))
                  :collect (if (equal nil (FULL-SOL-instruments loop-notas loop-canais loop-vel))
                              ; if true it is an error! Stop the loop   
                              (progn
                                    (print (format nil "There is no sample for the note ~a in the channel ~a with velocity ~a" loop-notas loop-canais loop-vel))
                                    (om::abort-eval))))))
                        

      (if (print (equal nil (remove nil test-samples))) "Todas as alturas possuem samples correspondentes" 
      (format nil "A nota ~d nao possuem sample correspondente! :( " (1+ (position t test-samples))))))

;; ====================================================
(defun 2-samples-without-notes (path velocity) 

(om::om-print "This sample was decided by the velocity" "OM-CKN ::")
(let* ((all-samples (ckn-in-files (merge-pathnames path *IRCAM-PATH*) 'wav)))
      (if (< velocity 50) (car all-samples) (car (cdr all-samples)))))
      
;; ==================================================== 

(defun 1-samples-without-notes (path) 

(om::om-print "This sample was decided by the velocity" "OM-CKN ::")
(let* ((all-samples (ckn-in-files (merge-pathnames path *IRCAM-PATH*) 'wav)))
      (car all-samples)))
      
;; ===================================================

(defun multiphonics-notes (path note)
      (let* (
      (all-samples (ckn-in-files (merge-pathnames path *IRCAM-PATH*) 'wav))
      (file-names (mapcar (lambda (x) (get-filename x)) all-samples))
      (caution-names (let* (
                            (extensions (mapcar (lambda (x) (cdr x)) (mapcar (lambda (y) (string-to-list y ".")) file-names))))
                       (remove nil  
                               (loop :for loop-ext :in extensions
                                     :do (print "Ok")
                                     :for loop-names :in file-names
                                     :collect (if (or (equal '("aif") loop-ext) (equal '("wav") loop-ext) (equal '("aiff") loop-ext))
                                                  loop-names
                                                nil))))))
      (ckn-multiphonics-notes caution-names (om::list! note))))


;; ==================================================== 

(defmethod! ckn-instruments ((note integer) (number-of-the-instrument pathname) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon 'omckn-sound
; This is a hack for a piece. It is not a good idea to use it in other pieces.
(probe-file number-of-the-instrument))

;; ==================================================== 

(defmethod! ckn-instruments ((note integer) (number-of-the-instrument list) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon 'omckn-sound

(probe-file (car number-of-the-instrument)))

;; ==================================================== 

(defmethod! ckn-instruments ((note integer) (number-of-the-instrument null) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon 'omckn-sound

nil)

;; ==================================================== 

(defmethod! ckn-instruments ((note integer) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon 'omckn-sound

(if (> number-of-the-instrument 1000)
      (FULL-SOL-instruments note (- number-of-the-instrument 1000) velocity)
      (orchidea-instruments note number-of-the-instrument velocity)))

;; ==================================================== 
(defmethod! FULL-SOL-instruments ((note_list list) (number-of-the-instrument_list list) &optional (velocity 60))

(let* (
      (action1
                (case (car number-of-the-instrument_list)
                      (14 (merge-pathnames (string+ "01 Flute/multiphonics/" (multiphonics-notes "01 Flute/multiphonics/" note_list)) *IRCAM-PATH*)))))
       (if (not action1) nil action1)))
                 

;; ==================================================== 
;; ==================================================== 
;; ==================================================== 

(defmethod! FULL-SOL-instruments ((note integer) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon 'omckn-sound
:doc "It create the patch of a sound.

============================  FLUTE ============================

||| 01 = Flute Aeolian   ||| 02 = Flute Aeolian+Ordinario ||| 03 = Flute Aeolian-to-ordinário 
||| 05 = Flute crescendo ||| 06 = Flute-cres-to-desc      ||| 07 = Flute-decr     
||| 08 = Flute Disc fing ||| 09 = Fl-flatt                ||| 10 = Fl-harm-fngr
||| 11 = Fl-harm-fngr    ||| 12 = Fl-jet-wh               ||| 13 = Fl-key-click    
||| 14 =  Fl-multi       ||| 16 = Fl-ord                  ||| 17 = Fl-ord-quarter-tone-up
||| 18 = Fl-ord_aeol     ||| 19 = Fl-ord_flatt            ||| 20 = Fl-pizz 21  Fl-ply+sng 
||| 22 = Fl-ply+sng-uni  ||| 23 = Fl-sfz                  ||| 24 = Fl-stacc 
||| 25 = Fl-tongue-ram   ||| 26 = Fl-trill-maj2           ||| 27 = Fl-trill-min2 
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
||| 51  =  trill-major           ||| 52  = Vibrato 

"

;;; 01 Flute 
(let* (
      (action1
            (case number-of-the-instrument
                  (1 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/aeolian/" 'wav) velocity))
                  (2 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/aeolian-and-ordinario/" 'wav) velocity))
                  (3 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/aeolian-to-ordinario/" 'wav) velocity))
                  (4 (2-samples-without-notes "01 Flute/chromatic-scale/" velocity))    
                  (5 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/crescendo/" 'wav) velocity))
                  (6 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/crescendo-to-decrescendo/" 'wav) velocity))
                  (7 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/decrescendo/" 'wav) velocity))
                  (8 (ckn-dinamics   (ckn-find-the-samples 4 note *IRCAM-PATH* "01 Flute/discolored-fingering/" 'wav) velocity))
                  (9 (ckn-dinamics   (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/flatterzunge/" 'wav) velocity))
                  (10 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/flatterzunge-to-ordinario/" 'wav) velocity))
                  (11 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "01 Flute/harmonic-fingering/" 'wav) velocity))
                  (12 (1-samples-without-notes "01 Flute/jet-whistle/"))
                  (13 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "01 Flute/key-click/" 'wav) velocity))
                  (14 (merge-pathnames (string+ "01 Flute/multiphonics/" (multiphonics-notes "01 Flute/multiphonics/" note)) *IRCAM-PATH*))
                  (15 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "01 Flute/note-lasting/" 'wav) velocity))
                  (16 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/ordinario/" 'wav) velocity))
                  (17 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/ordinario-1q/" 'wav) velocity))
                  (18 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/ordinario-to-aeolian/" 'wav) velocity))
                  (19 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/ordinario-to-flatterzunge/" 'wav) velocity))
                  (20 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/pizzicato/" 'wav) velocity))
                  (21 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/play-and-sing/" 'wav) velocity))
                  (22 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/play-and-sing-unison/" 'wav) velocity))
                  (23 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/sforzando/" 'wav) velocity))
                  (24 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/staccato/" 'wav) velocity))
                  (25 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "01 Flute/tongue-ram/" 'wav) velocity))
                  (26 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/trill-major-second-up/" 'wav) velocity))
                  (27 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/trill-minor-second-up/" 'wav) velocity))
                  (28 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/whistle-tones/" 'wav) velocity))
                  (29 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "01 Flute/whistle-tones-sweeping/" 'wav) velocity))

            ;;; 02 Oboe 
                  (30 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/blow-without-reed/" 'wav) velocity))
                  (31 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/chromatic-scale/" 'wav) velocity))
                  (32 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/crescendo/" 'wav) velocity))
                  (33 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/crescendo-to-decrescendo/" 'wav) velocity))
                  (34 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/decrescendo/" 'wav) velocity))
                  (35 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/decolored-fingering/" 'wav) velocity))
                  (36 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/double-trill-major-second-up/" 'wav) velocity))
                  (37 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/double-trill-minor-second-up/" 'wav) velocity))
                  (38 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/flatterzunge/" 'wav) velocity))
                  (39 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/harmonic-fingering/" 'wav) velocity))
                  (40 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "02 Oboe/key-click/" 'wav) velocity))
                  (41 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/kiss/" 'wav) velocity))
                  (42 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/lip-glissando/" 'wav) velocity))
                  (43 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/multiphonics/" 'wav) velocity))
                  (44 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/mute ordinario/" 'wav) velocity))
                  (45 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/note-lasting/" 'wav) velocity))
                  (46 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/ordinario/" 'wav) velocity))
                  (47 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/ordinario-1q/" 'wav) velocity))
                  (48 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/sforzando/" 'wav) velocity))
                  (49 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/staccato/" 'wav) velocity))
                  (50 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/trill-major-second-up/" 'wav) velocity))
                  (51 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/trill-minor-second-up/" 'wav) velocity))
                  (52 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "02 Oboe/vibrato/" 'wav) velocity))

            ;;  03 Clarinet in Bb
                  (53 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/aeolian-and-ordinario/" 'wav) velocity))
                  (54 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/crescendo/" 'wav) velocity))
                  (55 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/crescendo-to-decrescendo/" 'wav) velocity))
                  (56 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/decrescendo/" 'wav) velocity))
                  (57 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/flatterzunge/" 'wav) velocity))
                  (58 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/flatterzunge-high-register/" 'wav) velocity))
                  (59 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/glissando/" 'wav) velocity))
                  (60 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/key-click/" 'wav) velocity))
                  (61 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/mutiphonics/" 'wav) velocity))
                  (62 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/note-lasting/" 'wav) velocity))
                  (63 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/ordinario/" 'wav) velocity))
                  (64 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/ordinario-1q/" 'wav) velocity))
                  (65 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/ordinario-high-register/" 'wav) velocity))
                  (66 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/sforzando/" 'wav) velocity))
                  (67 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/staccato/" 'wav) velocity))
                  (68 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/trill-major-second-up/" 'wav) velocity))
                  (69 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "03 Clarinet in Bb/trill-minor-second-up/" 'wav) velocity))

            ;;; 04 bassoon
                  (70 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/blow-without-reed/" 'wav) velocity))
                  (71 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/chromatic-scale/" 'wav) velocity))
                  (72 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/crescendo/" 'wav) velocity))
                  (73 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/crescendo-to-decrescendo/" 'wav) velocity))
                  (74 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/decrescendo/" 'wav) velocity))
                  (75 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/flatterzunge/" 'wav) velocity))
                  (76 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/glissando-with-throat/" 'wav) velocity))
                  (77 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/harmonic-fingering/" 'wav) velocity))
                  (78 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/key-click/" 'wav) velocity))
                  (79 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/mutiphonics/" 'wav) velocity))
                  (80 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/note-lasting/" 'wav) velocity))
                  (81 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/ordinario/" 'wav) velocity))
                  (82 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/ordinario-1q/" 'wav) velocity))
                  (83 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/sforzando/" 'wav) velocity))
                  (84 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/sordina ordinario/" 'wav) velocity))
                  (85 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/staccato/" 'wav) velocity))
                  (86 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/trill-major-second-up/" 'wav) velocity))
                  (87 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/trill-minor-second-up/" 'wav) velocity))
                  (88 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "04 Bassoon/vibrato/" 'wav) velocity))

            ;;; 05 Saxophone Alto in Eb
                  (89 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/aeolian/" 'wav) velocity))
                  (90 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/backwards/" 'wav) velocity))
                  (91 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/bisbigliando/" 'wav) velocity))
                  (92 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/blow-without-reed/" 'wav) velocity))
                  (93 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/chromatic-scale/" 'wav) velocity))
                  (94 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/crescendo/" 'wav) velocity))
                  (95 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/crescendo-to-decrescendo/" 'wav) velocity))
                  (96 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/decrescendo/" 'wav) velocity))
                  (97 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/discolored-fingering/" 'wav) velocity))
                  (98 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/double-tonguing/" 'wav) velocity))
                  (99 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/exploding-slap-pitched/" 'wav) velocity))
                  (100 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/flatterzunge/" 'wav) velocity))
                  (101 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/flatterzunge-to-ordinario/" 'wav) velocity))
                  (102 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/glissando/" 'wav) velocity))
                  (103 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/harmonic-fingering/" 'wav) velocity))
                  (104 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/harmonic-glissando/" 'wav) velocity))
                  (105 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/key-click/" 'wav) velocity))
                  (106 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/kiss/" 'wav) velocity))
                  (107 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/move-bell-from-down-to-up/" 'wav) velocity))
                  (108 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/move-bell-from-left-to-right/" 'wav) velocity))
                  (109 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/multiphonics/" 'wav) velocity))
                  (110 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/ordinario/" 'wav) velocity))
                  (111 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/ordinario-1q/" 'wav) velocity))
                  (112 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/ordinario-high-register/" 'wav) velocity))
                  (113 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/ordinario-to-flatterzunge/" 'wav) velocity))
                  (114 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/play-and-sing-glissando/" 'wav) velocity))
                  (115 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/play-and-sing-m2-up/" 'wav) velocity))
                  (116 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/play-and-sing-unison/" 'wav) velocity))
                  (117 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/sforzando/" 'wav) velocity))
                  (118 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/slap-pitched/" 'wav) velocity))
                  (119 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/slap-unpitched/" 'wav) velocity))
                  (120 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/staccato/" 'wav) velocity))
                  (121 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/trill-major-second-up/" 'wav) velocity))
                  (122 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "05 Saxophone Alto in Eb/trill-minor-second-up/" 'wav) velocity))

            ;;; 06 French Horn
                  (123 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/brassy/" 'wav) velocity))
                  (124 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/brassy-to-ordinario/" 'wav) velocity))
                  (125 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/chromatic-scale/" 'wav) velocity))
                  (126 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/crescendo/" 'wav) velocity))
                  (127 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/crescendo-to-ordinario/" 'wav) velocity))
                  (128 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/decrescendo/" 'wav) velocity))
                  (129 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/flatterzunge/" 'wav) velocity))
                  (130 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/flatterzunge-stopped/" 'wav) velocity))
                  (131 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/flatterzunge-to-ordinario/" 'wav) velocity))
                  (132 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/mute flatterzunge/" 'wav) velocity))
                  (133 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/mute ordinario/" 'wav) velocity))
                  (134 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/note-lasting/" 'wav) velocity))
                  (135 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/open-to-stopped/" 'wav) velocity))
                  (136 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/ordinario/" 'wav) velocity))
                  (137 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/ordinario-to-brazzy/" 'wav) velocity))
                  (138 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/ordinario-to-flatterzunge/" 'wav) velocity))
                  (139 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/sforzando/" 'wav) velocity))
                  (140 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/slap-pitched/" 'wav) velocity))
                  (141 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/staccato/" 'wav) velocity))
                  (142 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/stopped/" 'wav) velocity))
                  (143 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/stopped-to-open/" 'wav) velocity))
                  (144 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/trill-major-second-up/" 'wav) velocity))
                  (145 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "06 French Horn/trill-minor-second-up/" 'wav) velocity))

            ;;; 07 Trumpet in C
                  (146 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/brassy/" 'wav) velocity))
                  (147 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/brassy-to-ordinario/" 'wav) velocity))
                  (148 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/crescendo/" 'wav) velocity))
                  (149 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/crescendo-to-decrescendo/" 'wav) velocity))
                  (150 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/cup flatterzunge/" 'wav) velocity))
                  (151 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/cup ordinario/" 'wav) velocity))
                  (152 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/decrescendo/" 'wav) velocity))
                  (153 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/flatterzunge/" 'wav) velocity))
                  (154 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/flatterzunge-to-ordinario/" 'wav) velocity))
                  (155 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/glissando-embouchure/" 'wav) velocity))
                  (156 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/half-valve-glissando/" 'wav) velocity))
                  (157 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/harmon flatterzunge/" 'wav) velocity))
                  (158 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/harmon ordinario/" 'wav) velocity))
                  (159 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/harmonics-glissando/" 'wav) velocity))
                  (160 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/increasing-intervals-legato/" 'wav) velocity))
                  (161 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/note-lasting/" 'wav) velocity))
                  (162 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/ordinario/" 'wav) velocity))
                  (163 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/ordinario-to-brazzy/" 'wav) velocity))
                  (164 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/ordinario-to-flatterzunge/" 'wav) velocity))
                  (165 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/pedal-tone/" 'wav) velocity))
                  (166 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/sforzando/" 'wav) velocity))
                  (167 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/slap-pitched/" 'wav) velocity))
                  (168 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/staccato/" 'wav) velocity))
                  (169 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/straight flatterzunge/" 'wav) velocity))
                  (170 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/straight ordinario/" 'wav) velocity))
                  (171 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/trill-major-second-up/" 'wav) velocity))
                  (172 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/trill-minor-second-up/" 'wav) velocity))
                  (173 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/vocalize-on-harmonics/" 'wav) velocity))
                  (174 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/wawa closed-to-open/" 'wav) velocity))
                  (175 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/wawa flatterzunge-open/" 'wav) velocity))
                  (176 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/wawa open-to-closed/" 'wav) velocity))
                  (177 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/wawa ordinario-closed/" 'wav) velocity))
                  (178 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "07 Trumpet in C/wawa ordinario-open/" 'wav) velocity))

            ;;; 08 Trombone
                  (179 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/brassy/" 'wav) velocity))
                  (180 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/brassy-to-ordinario/" 'wav) velocity))
                  (181 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/cromatic-scale/" 'wav) velocity))
                  (182 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/crescendo/" 'wav) velocity))
                  (183 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/crescendo-to-decrescendo/" 'wav) velocity))
                  (184 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/cup mute/" 'wav) velocity))
                  (185 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/decrescendo/" 'wav) velocity))
                  (186 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/flatterzunge/" 'wav) velocity))
                  (187 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/flatterzunge-no-mouthpiece/" 'wav) velocity))
                  (188 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/flatterzunge-to-ordinario/" 'wav) velocity))
                  (189 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/glissando/" 'wav) velocity))
                  (190 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/harmon mute/" 'wav) velocity))
                  (191 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/note-lasting/" 'wav) velocity))
                  (192 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/ordinario/" 'wav) velocity))
                  (193 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/ordinario-no-mouthpiece/" 'wav) velocity))
                  (194 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/ordinario-to-brazzy/" 'wav) velocity))
                  (195 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/ordinario-to-flatterzunge/" 'wav) velocity))
                  (196 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/pedal-tone/" 'wav) velocity))
                  (197 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/sforzando/" 'wav) velocity))
                  (198 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/slap-pitched/" 'wav) velocity))
                  (199 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/staccato/" 'wav) velocity))
                  (200 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/straight mute/" 'wav) velocity))
                  (201 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "08 Trombone in C/wawa mute/" 'wav) velocity))

            ;;; 09 Tuba
                  (202 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/bisbigliando/" 'wav) velocity))
                  (203 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/blow/" 'wav) velocity))
                  (204 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/brassy/" 'wav) velocity))
                  (205 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/breath/" 'wav) velocity))
                  (206 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/buzz/" 'wav) velocity))
                  (207 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/chromatic-scale/" 'wav) velocity))
                  (208 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/crescendo/" 'wav) velocity))
                  (209 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/crescendo-to-decrescendo/" 'wav) velocity))
                  (210 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/decrescendo/" 'wav) velocity))
                  (211 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/discolored-fingering/" 'wav) velocity))
                  (212 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/discolored-fingering-1q/" 'wav) velocity))
                  (213 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/exploding-slap-pitched/" 'wav) velocity))
                  (214 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/exploding-slap-unpitched/" 'wav) velocity))
                  (215 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/filtered-by-voice/" 'wav) velocity))
                  (216 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/flatterzunge/" 'wav) velocity))
                  (217 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/flatterzunge-and-voice-unison/" 'wav) velocity))
                  (218 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/flatterzunge-to-ordinario/" 'wav) velocity))
                  (219 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/glissando/" 'wav) velocity))
                  (220 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/growl/" 'wav) velocity))
                  (221 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/inhaled/" 'wav) velocity))
                  (222 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/kiss/" 'wav) velocity))
                  (223 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/multiphonics/" 'wav) velocity))
                  (224 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/ordinario/" 'wav) velocity))
                  (225 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/ordinario-1q/" 'wav) velocity))
                  (226 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/ordinario-high-register/" 'wav) velocity))
                  (227 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/ordinario-to-flatterzunge/" 'wav) velocity))
                  (228 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/pedal-tone/" 'wav) velocity))
                  (229 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/percussion-embouchure/" 'wav) velocity))
                  (230 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-aug4-up/" 'wav) velocity))
                  (231 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-glissando/" 'wav) velocity))
                  (232 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-m2-up/" 'wav) velocity))
                  (233 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-M7-up/" 'wav) velocity))
                  (234 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-P5-up/" 'wav) velocity))
                  (235 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/play-and-sing-unison/" 'wav) velocity))
                  (236 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/sforzando/" 'wav) velocity))
                  (237 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/single-tonguing/" 'wav) velocity))
                  (238 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/slap-pitched/" 'wav) velocity))
                  (239 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/slap-unpitched/" 'wav) velocity))
                  (240 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/sordina ordinario/" 'wav) velocity))
                  (241 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/speak-into-instrument/" 'wav) velocity))
                  (242 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/staccato/" 'wav) velocity))
                  (243 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/trill-major-second-up/" 'wav) velocity))
                  (244 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "09 Tuba in C/trill-minor-second-up/" 'wav) velocity))

            ;;;; 10 Accordion
                  (245 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/backwards/" 'wav) velocity))
                  (246 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/bellowshake/" 'wav) velocity))
                  (247 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/breath/" 'wav) velocity))
                  (248 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/combination-of-registers/" 'wav) velocity))
                  (249 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/crescendo/" 'wav) velocity))
                  (250 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/crescendo-to-decrescendo/" 'wav) velocity))
                  (251 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/decrescendo/" 'wav) velocity))
                  (252 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/difference-tones/" 'wav) velocity))
                  (253 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/key-click/" 'wav) velocity))
                  (254 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/ordinario/" 'wav) velocity))
                  (255 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/sforzando/" 'wav) velocity))
                  (256 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/staccato/" 'wav) velocity))
                  (257 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "10 Accordion/tap-on-body/" 'wav) velocity))

            ;;; 11 Guitar 
                  (258 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/behind-the-frog/" 'wav) velocity))
                  (259 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/bell-effect/" 'wav) velocity))
                  (260 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/bottleneck/" 'wav) velocity))
                  (261 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/chromatic-scale/" 'wav) velocity))
                  (262 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/dedillo/" 'wav) velocity))
                  (263 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/drum-effect/" 'wav) velocity))
                  (264 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/glissando/" 'wav) velocity))
                  (265 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/harmonic-fingering/" 'wav) velocity))
                  (266 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/ordinario/" 'wav) velocity))
                  (267 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/ordinario-high-register/" 'wav) velocity))
                  (268 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/pizzicato/" 'wav) velocity))
                  (269 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/pizzicato-bartok/" 'wav) velocity))
                  (270 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/rasguado/" 'wav) velocity))
                  (271 (first (ckn-in-files (merge-pathnames "11 Guitar/scratch-with-nail/" *IRCAM-PATH*) 'wav)))
                  (272 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/slide/" 'wav) velocity)) ;; need to fix!
                  (273 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/sul-ponticello/" 'wav) velocity))
                  (274 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/sul-tasto/" 'wav) velocity))
                  (275 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/tap-on-body/" 'wav) velocity))
                  (276 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "11 Guitar/tap-with-stick/" 'wav) velocity))

            ;;; 12 Harp                   (277 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/bisbigliando/" 'wav) velocity))
                  (278 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/bisbigliando-with-stick/" 'wav) velocity))
                  (279 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/buzzing-pedal/" 'wav) velocity))
                  (280 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/cluster/" 'wav) velocity))
                  (281 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/cluster-with-nail/" 'wav) velocity))
                  (282 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/damped/" 'wav) velocity))
                  (283 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/double-glissando/" 'wav) velocity))
                  (284 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando/" 'wav) velocity))
                  (285 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando-fluido-with-stick/" 'wav) velocity))
                  (286 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando-near-the-board/" 'wav) velocity))
                  (287 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando-with-nail/" 'wav) velocity))
                  (288 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando-with-pedal/" 'wav) velocity))
                  (289 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/glissando-with-stick/" 'wav) velocity))
                  (290 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "12 Harp/harmonic-fingering/" 'wav) velocity))
                  (291 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/harmonic-in-wood/" 'wav) velocity))
                  (292 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/near-the-board/" 'wav) velocity))
                  (293 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/near-the-board-with-nail/" 'wav) velocity))
                  (294 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/near-the-pegs/" 'wav) velocity))
                  (295 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/ordinario/" 'wav) velocity))
                  (296 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "12 Harp/pizzicato-bartok/" 'wav) velocity))
                  (297 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/scratch-with-nail/" 'wav) velocity))
                  (298 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/tap-on-body/" 'wav) velocity))
                  (299 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/tap-with-stick/" 'wav) velocity))
                  (300 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "12 Harp/tremolo-with-fingertips/" 'wav) velocity))
                  (301 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "12 Harp/xylophonic/" 'wav) velocity))

            ;;; 13 Violin
                  (302 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/artificial-harmonic/" 'wav) velocity))
                  (303 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/artificial-harmonic-tremolo/" 'wav) velocity))
                  (304 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/behind-the-bridge/" 'wav) velocity))
                  (305 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/behind-the-fingerboard/" 'wav) velocity))
                  (306 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/chromatic-scale/" 'wav) velocity))
                  (307 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/col-legno-battuto/" 'wav) velocity))
                  (308 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/col-legno-tratto/" 'wav) velocity))
                  (309 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/crescendo/" 'wav) velocity))
                  (310 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/crescendo-to-decrescendo/" 'wav) velocity))
                  (311 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/crushed-to-ordinario/" 'wav) velocity))
                  (312 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/decrescendo/" 'wav) velocity))
                  (313 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/hit-on-body/" 'wav) velocity))
                  (314 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/mute/" 'wav) velocity))
                  (315 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/mute piombo non-vibrato/" 'wav) velocity))
                  (316 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/mute piombo ordinario/" 'wav) velocity))
                  (317 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/mute piombo tremolo/" 'wav) velocity))
                  (318 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/natural-harmonics-glissandi/" 'wav) velocity))
                  (319 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/non-vibrato/" 'wav) velocity))
                  (320 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/note-lasting/" 'wav) velocity))
                  (321 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/on-the-tailpiece/" 'wav) velocity))
                  (322 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/on-the-tuning-pegs/" 'wav) velocity))
                  (323 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/ordinario/" 'wav) velocity))
                  (324 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/ordinario-to-crushed/" 'wav) velocity))
                  (325 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/ordinario-to-sul-ponticello/" 'wav) velocity))
                  (326 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/ordinario-to-sul-tasto/" 'wav) velocity))
                  (327 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/ordinario-to-tremolo/" 'wav) velocity))
                  (328 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "13 Violin/pizzicato-bartok/" 'wav) velocity))
                  (329 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/pizzicato-l-vib/" 'wav) velocity))
                  (330 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/pizzicato-secco/" 'wav) velocity))
                  (331 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sforzato/" 'wav) velocity))
                  (332 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/staccato/" 'wav) velocity))
                  (333 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-ponticello/" 'wav) velocity))
                  (334 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-ponticello-to-ordinario/" 'wav) velocity))
                  (335 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-ponticello-to-sul-tasto/" 'wav) velocity))
                  (336 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-ponticello-tremolo/" 'wav) velocity))
                  (337 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-tasto/" 'wav) velocity))
                  (338 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-tasto-to-ordinario/" 'wav) velocity))
                  (339 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/sul-tasto-to-sul-ponticello/" 'wav) velocity))
                  (340 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/tremolo/" 'wav) velocity))
                  (341 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/tremolo-to-ordinario/" 'wav) velocity))
                  (342 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/trill-major-second-up/" 'wav) velocity))
                  (343 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "13 Violin/trill-minor-second-up/" 'wav) velocity))

            ;;; 14 Viola
                  (344 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/artificial-harmonic/" 'wav) velocity))
                  (345 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/artificial-harmonic-tremolo/" 'wav) velocity))
                  (346 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/behind-the-bridge/" 'wav) velocity))
                  (347 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/behind-the-fingerboard/" 'wav) velocity))
                  (348 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/chromatic-scale/" 'wav) velocity))
                  (349 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/col-legno-battuto/" 'wav) velocity))
                  (350 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/col-legno-tratto/" 'wav) velocity))
                  (351 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/crescendo/" 'wav) velocity))
                  (352 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/crescendo-to-decrescendo/" 'wav) velocity))
                  (353 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/crushed-to-ordinario/" 'wav) velocity))
                  (354 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/decrescendo/" 'wav) velocity))
                  (355 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/hit-on-body/" 'wav) velocity))
                  (356 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute non-vibrato/" 'wav) velocity))
                  (357 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute ordinario/" 'wav) velocity))
                  (358 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute piombo non-vibrato/" 'wav) velocity))
                  (359 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute piombo ordinario/" 'wav) velocity))
                  (360 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute piombo tremolo/" 'wav) velocity))
                  (361 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/mute tremolo/" 'wav) velocity))
                  (362 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/natural-harmonics-glissandi/" 'wav) velocity))
                  (363 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/non-vibrato/" 'wav) velocity))
                  (364 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/note-lasting/" 'wav) velocity))
                  (365 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/on-the-bridge/" 'wav) velocity))
                  (366 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/on-the-frog/" 'wav) velocity))
                  (367 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/on-the-tuning-pegs/" 'wav) velocity))
                  (368 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/ordinario/" 'wav) velocity))
                  (369 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/ordinario-to-crushed/" 'wav) velocity))
                  (370 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/ordinario-to-sul-ponticello/" 'wav) velocity))
                  (371 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/ordinario-to-sul-tasto/" 'wav) velocity))
                  (372 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/ordinario-to-tremolo/" 'wav) velocity))
                  (373 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/pizzicato-bartok/" 'wav) velocity))
                  (374 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/pizzicato-l-vib/" 'wav) velocity))
                  (375 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/pizzicato-secco/" 'wav) velocity))
                  (376 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sforzato/" 'wav) velocity))
                  (377 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/staccato/" 'wav) velocity))
                  (378 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-ponticello/" 'wav) velocity))
                  (379 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-ponticello-to-ordinario/" 'wav) velocity))
                  (380 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-ponticello-to-sul-tasto/" 'wav) velocity))
                  (381 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-ponticello-tremolo/" 'wav) velocity))
                  (382 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-tasto/" 'wav) velocity))
                  (383 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-tasto-to-ordinario/" 'wav) velocity))
                  (384 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-tasto-to-sul-ponticello/" 'wav) velocity))
                  (385 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/sul-tasto-tremolo/" 'wav) velocity))
                  (386 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/tremolo/" 'wav) velocity))
                  (387 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/tremolo-to-ordinario/" 'wav) velocity))
                  (388 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/trill-major-second-up/" 'wav) velocity))
                  (389 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "14 Viola/trill-minor-second-up/" 'wav) velocity))

            ;;; 15 Cello
                  (390 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/artificial-harmonic/" 'wav) velocity))
                  (391 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/artificial-harmonic-tremolo/" 'wav) velocity))
                  (392 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/behind-the-bridge/" 'wav) velocity))
                  (393 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/chromatic-scale/" 'wav) velocity))
                  (394 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/col-legno-battuto/" 'wav) velocity))
                  (395 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/col-legno-tratto/" 'wav) velocity))
                  (396 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/crescendo/" 'wav) velocity))
                  (397 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/crescendo-to-decrescendo/" 'wav) velocity))
                  (398 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/crushed-to-ordinario/" 'wav) velocity))
                  (399 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/decrescendo/" 'wav) velocity))
                  (400 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/hit-on-body/" 'wav) velocity))
                  (401 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute non-vibrato/" 'wav) velocity))
                  (402 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute ordinario/" 'wav) velocity))
                  (403 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute piombo non-vibrato/" 'wav) velocity))
                  (404 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute piombo ordinario/" 'wav) velocity))
                  (405 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute piombo tremolo/" 'wav) velocity))
                  (406 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/mute tremolo/" 'wav) velocity))
                  (407 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/natural-harmonics-glissandi/" 'wav) velocity))
                  (408 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/non-vibrato/" 'wav) velocity))
                  (409 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/note-lasting/" 'wav) velocity))
                  (410 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/on-the-bridge/" 'wav) velocity))
                  (411 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/on-the-frog/" 'wav) velocity))
                  (412 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/on-the-tailpiece/" 'wav) velocity))
                  (413 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/on-the-tuning-pegs/" 'wav) velocity))
                  (414 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/ordinario/" 'wav) velocity))
                  (415 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/ordinario-to-crushed/" 'wav) velocity))
                  (416 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/ordinario-to-sul-ponticello/" 'wav) velocity))
                  (417 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/ordinario-to-sul-tasto/" 'wav) velocity))
                  (418 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/ordinario-to-tremolo/" 'wav) velocity))
                  (419 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "15 Cello/pizzicato-bartok/" 'wav) velocity))
                  (420 (ckn-dinamics  (ckn-find-the-samples 6 note *IRCAM-PATH* "15 Cello/pizzicato-l-vib/" 'wav) velocity)) ;; NOT WORK
                  (421 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/pizzicato-secco/" 'wav) velocity))
                  (422 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sforzato/" 'wav) velocity))
                  (423 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/staccato/" 'wav) velocity))
                  (424 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "15 Cello/sul-ponticello/" 'wav) velocity))
                  (425 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-ponticello-to-ordinario/" 'wav) velocity))
                  (426 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-ponticello-to-sul-tasto/" 'wav) velocity))
                  (427 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-ponticello-tremolo/" 'wav) velocity))
                  (428 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-tasto/" 'wav) velocity))
                  (429 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-tasto-to-ordinario/" 'wav) velocity))
                  (430 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-tasto-to-sul-ponticello/" 'wav) velocity))
                  (431 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/sul-tasto-tremolo/" 'wav) velocity))
                  (432 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/tremolo/" 'wav) velocity))
                  (433 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/tremolo-to-ordinario/" 'wav) velocity))
                  (434 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/trill-major-second-up/" 'wav) velocity))
                  (435 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "15 Cello/trill-minor-second-up/" 'wav) velocity))

            ;;; 16 Contrabass
                  (436 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/artificial-harmonic/" 'wav) velocity))
                  (437 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/artificial-harmonic-tremolo/" 'wav) velocity))
                  (438 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/behind-the-bridge/" 'wav) velocity))
                  (439 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/chromatic-scale/" 'wav) velocity))
                  (440 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/col-legno-battuto/" 'wav) velocity))
                  (441 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/col-legno-tratto/" 'wav) velocity))
                  (442 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/crescendo/" 'wav) velocity))
                  (443 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/crescendo-to-decrescendo/" 'wav) velocity))
                  (444 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/crushed-to-ordinario/" 'wav) velocity))
                  (445 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/decrescendo/" 'wav) velocity))
                  (446 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/hit-on-body/" 'wav) velocity))
                  (447 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/mute non-vibrato/" 'wav) velocity))
                  (448 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/mute ordinario/" 'wav) velocity))
                  (449 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/mute tremolo/" 'wav) velocity))
                  (450 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/natural-harmonics-glissandi/" 'wav) velocity))
                  (451 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/non-vibrato/" 'wav) velocity))
                  (452 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/note-lasting/" 'wav) velocity))
                  (453 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/on-the-bridge/" 'wav) velocity))
                  (454 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/on-the-tailpiece/" 'wav) velocity))
                  (455 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/on-the-tuning-pegs/" 'wav) velocity))
                  (456 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/ordinario/" 'wav) velocity))
                  (457 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/ordinario-to-crushed/" 'wav) velocity))
                  (458 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/ordinario-to-sul-ponticello/" 'wav) velocity))
                  (459 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/ordinario-to-sul-tasto/" 'wav) velocity))
                  (460 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/ordinario-to-tremolo/" 'wav) velocity))
                  (461 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/pizzicato-bartok/" 'wav) velocity))
                  (462 (ckn-dinamics  (ckn-find-the-samples 4 note *IRCAM-PATH* "16 Contrabass/pizzicato-l-vib/" 'wav) velocity))
                  (463 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/pizzicato-secco/" 'wav) velocity))
                  (464 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sforzato/" 'wav) velocity))
                  (465 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/staccato/" 'wav) velocity))
                  (466 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-ponticello/" 'wav) velocity))
                  (467 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-ponticello-to-ordinario/" 'wav) velocity))
                  (468 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-ponticello-to-sul-tasto/" 'wav) velocity))
                  (469 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-ponticello-tremolo/" 'wav) velocity))
                  (470 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-tasto/" 'wav) velocity))
                  (471 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-tasto-to-ordinario/" 'wav) velocity))
                  (472 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-tasto-to-sul-ponticello/" 'wav) velocity))
                  (473 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/sul-tasto-tremolo/" 'wav) velocity))
                  (474 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/tremolo/" 'wav) velocity))
                  (475 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/tremolo-to-ordinario/" 'wav) velocity))
                  (476 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/trill-major-second-up/" 'wav) velocity))
                  (477 (ckn-dinamics  (ckn-find-the-samples 3 note *IRCAM-PATH* "16 Contrabass/trill-minor-second-up/" 'wav) velocity)))))
(if (not action1) nil action1)))