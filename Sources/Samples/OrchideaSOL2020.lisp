(in-package :om)

;; ==================================================== 

(defmethod! OrchideaSOL-folder ((x string))
:initvals '(nil)
:indoc '("Name of the composer" "Name of the piece") 
:icon '17359
:doc "This object define the name of the composer and the name of the piece."

(defparameter *OrchideaSOL-PATH* x)
(om-save-pathname *OrchideaSOL-PATH*)
*OrchideaSOL-PATH*)

;; ====================================================

(defun ckn-in-files (path type)

            (let* (
                        (thefilelist (om-directory path 
                                             :type type :directories nil :files t 
                                             :resolve-aliases nil :hidden-files nil)))
              thefilelist))

;; ==================================================== 

(defun ckn-find-the-samples (where-is-the-nome note OrchideaSOL path-of-the-instrument type)
  (let* (
        (instrument-pathname (merge-pathnames path-of-the-instrument OrchideaSOL))
        (all-the-files (ckn-in-files (namestring instrument-pathname) type))
        (all-the-notes (n->mc (mapcar (lambda (x) (choose (string-to-list (get-filename x) "-") where-is-the-nome)) all-the-files) 4))
        (position-of-the-note (ckn-position all-the-notes note)))
        (choose all-the-files position-of-the-note)))

;; ==================================================== 

(defun ckn-dinamics (list-of-samples velocity)

(if (equal (length list-of-samples) 3)
      (if (> velocity 87) (car (last list-of-samples)) (if (>= velocity 56) (second list-of-samples) (first list-of-samples)))
      (first list-of-samples)))

;; ==================================================== 

(defun ckn-dinamics-2 (list-of-samples velocity)

(if (equal (length list-of-samples) 2)
      (if (> velocity 50) (cdr list-of-samples) (car list-of-samples))
      (car list-of-samples)))

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
      (equal nil (orchidea-samples loop-notas loop-canais loop-vel)))))

(if (equal nil (remove nil test)) "Todas as alturas possuem samples correspondentes" 
(format nil "A nota ~d nao possuem sample correspondente! :( " (1+ (position t test))))))

;; ====================================================

(defmethod!  orchidea-instruments ((note integer) (number-of-the-instrument integer) &optional (velocity 60))
:initvals '(6000 20 60)
:indoc '("Sound class" "Number of the instrument (technique)") 
:icon '17359
:doc "It create the patch of a sound.

============================  FLUTE ============================



============================= OBOE ===============================


"

(case number-of-the-instrument

;; ============== Winds ===============
;; Flute 

(1  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/aeolian/" 'wav) velocity))
(2  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/aeolian_and_ordinario/" 'wav) velocity))
(3  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/discolored_fingering/" 'wav) velocity))
(4  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/flatterzunge/" 'wav) velocity))
(5  (first          (ckn-dinamics-2 (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/harmonic_fingering/" 'wav) velocity)))
(6  (first          (ckn-in-files (merge-pathnames "Winds/Flute/jet_whistle/" *OrchideaSOL-PATH*) 'wav)))
(7  (first          (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/key_click/" (quote 'wav)))) ;; WRONG CODE 

(8  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/ordinario/" 'wav) velocity))
(9  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/sforzato/" 'wav) velocity))
(10 (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Flute/whistle_tones/" 'wav) velocity))

;; Oboe

(11 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/blow_without_reed/" 'wav) velocity))
(12 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/discolored_fingering/" 'wav) velocity))
(13 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/flatterzunge/" 'wav) velocity))
(14 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/harmonic_fingering/" 'wav) velocity))
(15 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/key_click/" 'wav) velocity))
(16 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/kiss/" 'wav) velocity))
(17 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/ordinario/" 'wav) velocity))
(18 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/sforzato/" 'wav) velocity))
(19 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe/vibrato/" 'wav) velocity))

;; Oboe + surdina

(20 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Oboe+sordina/ordinario/" 'wav) velocity))

;; Clarinete_Bb

(21 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/aeolian_and_ordinario/" 'wav) velocity))
(22 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/flatterzunge/" 'wav) velocity))
(23 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/flatterzunge_high_register/" 'wav) velocity))
(24 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/key_click/" 'wav) velocity))
(25 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/ordinario/" 'wav) velocity))
(26 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/ordinario_high_register/" 'wav) velocity))
(27 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Clarinet_Bb/sforzato/" 'wav) velocity))

;;; Bassoon

(28 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/blow_without_reed/" 'wav) velocity))
(29 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/flatterzunge/" 'wav) velocity))
(30 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/harmonic_fingering/" 'wav) velocity))
(31 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/key_click/" 'wav) velocity))
(32 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/ordinario/" 'wav) velocity))
(33 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/sforzato/" 'wav) velocity))
(34 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon/vibrato/" 'wav) velocity))

;; Bassoon+sordina 

(35 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Bassoon+sordina/ordinario/" 'wav) velocity))

;; Sax_Alto

(36 (ckn-dinamics (ckn-find-the-samples  3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/aeolian/" 'wav) velocity))
(37 (ckn-dinamics (ckn-find-the-samples  3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/bisbigliando/" 'wav) velocity))
(38 (first (ckn-in-files (merge-pathnames "Winds/Sax_Alto/blow_without_reed/" *OrchideaSOL-PATH*) 'wav)))
(39 (ckn-dinamics (ckn-find-the-samples  3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/discolored_fingering/" 'wav) velocity))
(40 (ckn-dinamics (ckn-find-the-samples  3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/double_tonguing/" 'wav) velocity))
(41 (ckn-dinamics (ckn-find-the-samples  3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/flatterzunge/" 'wav) velocity))
(42 (car (Cl-harmonic_fingering 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/harmonic_fingering/" 'wav)))
(43 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/key_click/" 'wav) velocity))
(44 (first (ckn-in-files (merge-pathnames "Winds/Sax_Alto/kiss/" *OrchideaSOL-PATH*) 'wav)))
(45 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/ordinario/" 'wav) velocity))
(46 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/ordinario_high_register/" 'wav) velocity))
(47 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/sforzato/" 'wav) velocity))
(48 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/slap_pitched/" 'wav) velocity))
(49 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Winds/Sax_Alto/slap_unpitched/" 'wav) velocity))

;; ============== Brass ===============
;; Trumpet_C

(50 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/brassy/" 'wav) velocity))
(51 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/flatterzunge/" 'wav) velocity))
(52 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/ordinario/" 'wav) velocity))
(53 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/pedal_tone/" 'wav) velocity))
(54 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/sforzato/" 'wav) velocity))
(55 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C/slap_pitched/" 'wav) velocity))

;; Trumpet_C+sordina_wah

(56 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_wah/flatterzunge_open/" 'wav) velocity))
(57 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_wah/ordinario_closed/" 'wav) velocity))
(58 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_wah/ordinario_open/" 'wav) velocity))

;; Trumpet_C+sordina_straight

(59 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_straight/flatterzunge/" 'wav) velocity))
(60 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_straight/ordinario/" 'wav) velocity))

;; Trumpet_C+sordina_harmon

(61 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_harmon/flatterzunge/" 'wav) velocity))
(62 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_harmon/ordinario/" 'wav) velocity))

;; Trumpet_C+sordina_cup

(63 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_cup/flatterzunge/" 'wav) velocity))
(64 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trumpet_C+sordina_cup/ordinario/" 'wav) velocity))


;; Horn

(65 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/brassy/" 'wav) velocity))
(66 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/flatterzunge/" 'wav) velocity))
(67 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/flatterzunge_stopped/" 'wav) velocity))
(68 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/ordinario/" 'wav) velocity))
(69 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/sforzato/" 'wav) velocity))
(70 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/slap_pitched/" 'wav) velocity))
(71 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn/stopped/" 'wav) velocity))

;; Horn+sordina

(72 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn+sordina/flatterzunge/" 'wav) velocity))
(73 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Horn+sordina/ordinario/" 'wav) velocity))

;; Trombone 

(74 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/brassy/" 'wav) velocity))
(75 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/flatterzunge/" 'wav) velocity))
(76 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/flatterzunge_no_mouthpiece/" 'wav) velocity))
(77 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/ordinario/" 'wav) velocity))
(78 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/pedal_tone/" 'wav) velocity))
(79 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/sforzato/" 'wav) velocity))
(80 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone/slap_pitched/" 'wav) velocity))

;; Trombone+sordina_wah

(81 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_wah/flatterzunge_closed/" 'wav) velocity))
(82 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_wah/flatterzunge_open/" 'wav) velocity))
(83 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_wah/ordinario_closed/" 'wav) velocity))
(84 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_wah/ordinario_open/" 'wav) velocity))

;; Trombone+sordina_straight

(85 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_straight/flatterzunge/" 'wav) velocity))
(86 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_straight/ordinario/" 'wav) velocity))

;; Trombone+sordina_harmon

(87 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_harmon/flatterzunge/" 'wav) velocity))
(88 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_harmon/ordinario/" 'wav) velocity))

;; Trombone+sordina_cup
(89 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_cup/flatterzunge/" 'wav) velocity))
(90 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Trombone+sordina_cup/ordinario/" 'wav) velocity))

;; Bass_Tuba 
(91  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/bisbigliando/" 'wav) velocity))
(92  (first          (ckn-in-files (merge-pathnames "Brass/Bass_Tuba/blow/" *OrchideaSOL-PATH*) 'wav)))
(93  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/brassy/" 'wav) velocity))
(94  (first          (ckn-in-files (merge-pathnames "Brass/Bass_Tuba/breath/" *OrchideaSOL-PATH*) 'wav)))
(95  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/discolored_fingering/" 'wav) velocity))
(96  (ckn-dinamics   (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/flatterzunge/" 'wav) velocity))
(97  (ckn-dinamics-2 (ckn-in-files (merge-pathnames "Brass/Bass_Tuba/growl/" *OrchideaSOL-PATH*) 'wav) velocity))
(98  (first (ckn-in-files (merge-pathnames "Brass/Bass_Tuba/kiss/" *OrchideaSOL-PATH*) 'wav)))
(99  (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/ordinario/" 'wav) velocity))
(100 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/ordinario_high_register/" 'wav) velocity))
(101 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/pedal_tone/" 'wav) velocity))
(102 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/sforzato/" 'wav) velocity))
(103 (BTb-slap 3 note *OrchideaSOL-PATH* "Brass/Bass_Tuba/slap_pitched/" 'wav))
(104 (om::nth-random          (ckn-in-files (merge-pathnames "Brass/Bass_Tuba/slap_unpitched/" *OrchideaSOL-PATH*) 'wav)))

;; ============== Strings ===============
;; Violin

(105 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/artificial_harmonic/" 'wav) velocity))
(106 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/artificial_harmonic_tremolo/" 'wav) velocity))
(107 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Violin/behind_the_bridge/" *OrchideaSOL-PATH*) 'wav)))
                    ;;; 107 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(108 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Violin/behind_the_fingerboard/" *OrchideaSOL-PATH*) 'wav)))
(109 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/col_legno_battuto/" 'wav) velocity))
(110 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/col_legno_tratto/" 'wav) velocity))
(111 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/ordinario/" 'wav) velocity))
(112 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/pizzicato_bartok/" 'wav) velocity))
(113 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/pizzicato_l_vib/" 'wav) velocity))
(114 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/pizzicato_secco/" 'wav) velocity))



(115 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/sforzato/" 'wav) velocity))
(116 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/sul_ponticello/" 'wav) velocity))
(117 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/sul_ponticello_tremolo/" 'wav) velocity))
(118 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin/tremolo/" 'wav) velocity))

;; Violin+sordina
(119 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin+sordina/ordinario/" 'wav) velocity))
(120 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin+sordina/tremolo/" 'wav) velocity))

;; Violin+sordina_piombo
(121 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin+sordina_piombo/ordinario/" 'wav) velocity))
(122 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violin+sordina_piombo/tremolo/" 'wav) velocity))

;; Viola
(123 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/artificial_harmonic/" 'wav) velocity))
(124 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/artificial_harmonic_tremolo/" 'wav) velocity))
(125 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Viola/behind_the_bridge/" *OrchideaSOL-PATH*) 'wav)))
                    ;;; 107 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(126 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Viola/behind_the_fingerboard/" *OrchideaSOL-PATH*) 'wav)))
(127 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/col_legno_battuto/" 'wav) velocity))
(128 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/col_legno_tratto/" 'wav) velocity))
(129 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/ordinario/" 'wav) velocity))
(130 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/pizzicato_bartok/" 'wav) velocity))
(131 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/pizzicato_l_vib/" 'wav) velocity))
(132 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/pizzicato_secco/" 'wav) velocity))
(133 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/sforzato/" 'wav) velocity))
(134 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/sul_ponticello/" 'wav) velocity))
(135 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/sul_ponticello_tremolo/" 'wav) velocity))
(136 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola/tremolo/" 'wav) velocity))

;; Viola+sordina
(137 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola+sordina/ordinario/" 'wav) velocity))
(138 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola+sordina/tremolo/" 'wav) velocity))

;; Viola+sordina_piombo
(139 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola+sordina_piombo/ordinario/" 'wav) velocity))
(140 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Viola+sordina_piombo/tremolo/" 'wav) velocity))

;; Violoncello 
(141 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/artificial_harmonic/" 'wav) velocity))
(142 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/artificial_harmonic_tremolo/" 'wav) velocity))
(143 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Violoncello/behind_the_bridge/" *OrchideaSOL-PATH*) 'wav)))
                    ;;; 143 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(144 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/col_legno_battuto/" 'wav) velocity))
(145 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/col_legno_tratto/" 'wav) velocity))
(146 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/ordinario/" 'wav) velocity))
(147 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/pizzicato_bartok/" 'wav) velocity))
(148 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/pizzicato_l_vib/" 'wav) velocity))
(149 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/pizzicato_secco/" 'wav) velocity))
(150 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/sforzato/" 'wav) velocity))
(151 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/sul_ponticello/" 'wav) velocity))
(152 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/sul_ponticello_tremolo/" 'wav) velocity))
(153 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello/tremolo/" 'wav) velocity))

;; Violoncello+sordina
(154 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello+sordina/ordinario/" 'wav) velocity))
(155 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello+sordina/tremolo/" 'wav) velocity))

;; Violoncello+sordina_piombo

(156 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello+sordina_piombo/ordinario/" 'wav) velocity))
(157 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Violoncello+sordina_piombo/tremolo/" 'wav) velocity))

;; Contrabass 
(158 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/artificial_harmonic/" 'wav) velocity))
(159 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/artificial_harmonic_tremolo/" 'wav) velocity))
(160 (om::nth-random          (ckn-in-files (merge-pathnames "Strings/Contrabass/behind_the_bridge/" *OrchideaSOL-PATH*) 'wav)))
                    ;;; 160 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(161 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/col_legno_battuto/" 'wav) velocity))
(162 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/col_legno_tratto/" 'wav) velocity))
(163 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/ordinario/" 'wav) velocity))
(164 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/pizzicato_bartok/" 'wav) velocity))
(165 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/pizzicato_l_vib/" 'wav) velocity))
(166 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/pizzicato_secco/" 'wav) velocity))
(167 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/sforzato/" 'wav) velocity))
(168 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/sul_ponticello/" 'wav) velocity))
(169 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/sul_ponticello_tremolo/" 'wav) velocity))
(170 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass/tremolo/" 'wav) velocity))

;; Contrabass+sordina 
(171 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass+sordina/ordinario/" 'wav) velocity))
(172 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Strings/Contrabass+sordina/tremolo/" 'wav) velocity))

;; ============== PluckedStrings ===============
;;  Guitar 
(173 (om::nth-random          (ckn-in-files (merge-pathnames "PluckedStrings/Guitar/behind_the_frog/" *OrchideaSOL-PATH*) 'wav)))
(174 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Guitar/harmonic_fingering/" 'wav) velocity))
(175 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Guitar/ordinario/" 'wav) velocity))
(176 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Guitar/ordinario_high_register/" 'wav) velocity))
(177 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Guitar/pizzicato_bartok/" 'wav) velocity))
(178 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Guitar/sul_ponticello/" 'wav) velocity))

;;  Harp
(179 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/bisbigliando/" 'wav) velocity))
(180 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/bisbigliando_with_stick/" 'wav) velocity))
(181 (om::nth-random          (ckn-in-files (merge-pathnames "PluckedStrings/Harp/cluster/" *OrchideaSOL-PATH*) 'wav)))
            ;;; 181 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(182 (om::nth-random          (ckn-in-files (merge-pathnames "PluckedStrings/Harp/cluster_with_nail/" *OrchideaSOL-PATH*) 'wav)))
            ;;; 182 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)
(183 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/damped/" 'wav) velocity))
(184 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/harmonic_fingering/" 'wav) velocity))
(185 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/ordinario/" 'wav) velocity))
(186 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "PluckedStrings/Harp/pizzicato_bartok/" 'wav) velocity))
(187 (om::nth-random          (ckn-in-files (merge-pathnames "PluckedStrings/Harp/tremolo_with_fingertips/" *OrchideaSOL-PATH*) 'wav)))
            ;;; 187 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;)

;; ============== Keyboards ===============
;;  Accordion
(188 (om::nth-random          (ckn-in-files (merge-pathnames "Keyboards/Accordion/breath/" *OrchideaSOL-PATH*) 'wav)))
(189 (ckn-dinamics-2 (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Keyboards/Accordion/combination_of_registers/" 'wav) velocity))
      ;;;; 189 DESIRES SOME ATTENTION!!!!!!!!!!!!!!!! ;) VELOCITY PARA CONTROLAR O ESPECTRO QUE SERÁ UTILIZADO; 
(190 (om::nth-random          (ckn-in-files (merge-pathnames "Keyboards/Accordion/key_click/" *OrchideaSOL-PATH*) 'wav)))
(191 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Keyboards/Accordion/ordinario/" 'wav) velocity))
(192 (ckn-dinamics (ckn-find-the-samples 3 note *OrchideaSOL-PATH* "Keyboards/Accordion/sforzato/" 'wav) velocity))


))



;;;; Need special functions 

(defun Cl-harmonic_fingering (where-is-the-nome note OrchideaSOL path-of-the-instrument type)
  (let* (
        (instrument-pathname (merge-pathnames path-of-the-instrument OrchideaSOL))
        (all-the-files (ckn-in-files (namestring instrument-pathname) type))
        (all-the-notes (n->mc (mapcar (lambda (x) (second (string-to-list (choose (string-to-list (get-filename x) "-") where-is-the-nome) "_"))) all-the-files) 4))
        (position-of-the-note (ckn-position all-the-notes note)))
        (om-print "This function will load the sample that correspond to the second note." "OM-CKN ::")
        (choose all-the-files position-of-the-note)))

;; ==========================================

(defun BTb-slap (where-is-the-nome note OrchideaSOL path-of-the-instrument type)
  (let* (
        (instrument-pathname (merge-pathnames path-of-the-instrument OrchideaSOL))
        (all-the-files (ckn-in-files (namestring instrument-pathname) type))
        (all-the-notes (n->mc (mapcar (lambda (x) (first (string-to-list (choose (string-to-list (get-filename x) "-") where-is-the-nome) "_"))) all-the-files) 4))
        (position-of-the-note (ckn-position all-the-notes note)))
        (om-print "This function will load the sample that correspond to the second note." "OM-CKN ::")
        (om::nth-random (choose all-the-files position-of-the-note))))



;;;;; FINISHHHHHHHHHHHHHHHHHHHHHHH 