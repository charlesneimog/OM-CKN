
(in-package :om)

(defparameter *smufl-note-heads*
  '(("noteheadHalf" #xE0A3)
    ("noteheadBlack" #xE0A4)
    ("noteheadXBlack" #xE0A9)
    ("noteheadCircleX" #xE0B3)
    ("noteheadSquareWhite" #xE0B8)
    ("noteheadSquareWhite" #xE0B9)
    ("noteheadTriangleUpWhite" #xE0BD)
    ("noteheadTriangleUpBlack" #xE0BE)
    ("noteheadDiamondBlackWide" #xE0DC)
    ("noteheadDiamondWhiteWide" #xE0DE)
    ("noteheadDoubleWhole" #xE0A0)
    ("noteheadDoubleWholeSquare" #xE0A1)
    ("noteheadHalf" #xE0A3)
    ("noteheadBlack" #xE0A4) 
    ("notehead" #xE0A5)
    ("noteheadXDoubleWhole" #xE0A6)
    ( "noteheadXWhole" #xE0A7)
    ( "noteheadXHalf" #xE0A8)
    ("noteheadXBlack" #xE0A9)
    ("noteheadXOrnate" #xE0AA) 
    ("noteheadXOrnateEllipse" #xE0AB) 
    ("noteheadPlusDoubleWhole" #xE0AC)
    ("noteheadPlusWhole" #xE0AD)
    ("noteheadPlusHalf" #xE0AE)
    ("noteheadPlusBlack" #xE0AF)
    ("noteheadCircleXDoubleWhole" #xE0B0)
    ("noteheadCircleXWhole" #xE0B1)
    ("noteheadCircleXHalf" #xE0B2)
    ( "noteheadCircleX" #xE0B3)
    ("noteheadDoubleWholeWithX" #xE0B4)
    ("noteheadWholeWithX" #xE0B5)
    ("noteheadHalfWithX" #xE0B6)
    ("noteheadVoidWithX" #xE0B7)
    ("noteheadSquareWhite" #xE0B8)
    ("noteheadSquareBlack" #xE0B9)
    ("noteheadTriangleUpDoubleWhole" #xE0BA)
    ("noteheadTriangleUpWhole" #xE0BB)
    ("noteheadTriangleUpHalf" #xE0BC)
    ("noteheadTriangleUpWhite" #xE0BD)
    ("noteheadTriangleUpBlack" #xE0BE)
    ("noteheadTriangleLeftWhite" #xE0BF)
    ("noteheadTriangleLeftBlack" #xE0C0)
    ("noteheadTriangleRightWhite" #xE0C1)
    ("noteheadTriangleRightBlack" #xE0C2)
    ("noteheadTriangleDownDoubleWhole" #xE0C3)
    ("noteheadTriangleDownWhole" #xE0C4)
    ("noteheadTriangleDownHalf" #xE0C5)
    ("noteheadTriangleDownWhite" #xE0C6)
    ("noteheadTriangleDownBlack" #xE0C7)
    ("noteheadTriangleUpRightWhite" #xE0C8 )
    ("noteheadTriangleUpRightBlack" #xE0C9)
    ("noteheadMoonWhite" #xE0CA)
    ("noteheadMoonBlack" #xE0CB)
    ("noteheadTriangleRoundDownWhite" #xE0CC)
    ("noteheadTriangleRoundDownBlack" #xE0CD)
    ("noteheadParenthesis" #xE0CE)
    ("noteheadSlashedBlack1" #xE0CF)
    ("noteheadSlashedBlack2" #xE0D0)
    ("noteheadSlashedHalf1" #xE0D1)
    ("noteheadSlashedHalf2" #xE0D2)
    ("noteheadSlashedWhole1" #xE0D3)
    ("noteheadSlashedWhole2" #xE0D4)
    ("noteheadSlashedDoubleWhole1" #xE0D5)
    ("noteheadSlashedDoubleWhole2" #xE0D6)
    ("noteheadDiamondDoubleWhole" #xE0D7)
    ("noteheadDiamondWhole" #xE0D8)
    ("noteheadDiamondHalf" #xE0D9)
    ("noteheadDiamondHalfWide" #xE0DA)
    ("noteheadDiamondBlack" #xE0DB)
    ("noteheadDiamondBlackWide" #xE0DC)
    ("noteheadDiamondWhite" #xE0DD)
    ("noteheadDiamondWhiteWide" #xE0DE)
    ("noteheadDiamondDoubleWholeOld" #xE0DF)
    ("noteheadDiamondWholeOld" #xE0E0)
    ("noteheadDiamondHalfOld" #xE0E1)
    ("noteheadDiamondBlackOld" #xE0E2)
    ("noteheadDiamondHalfFilled" #xE0E3)
    ("noteheadCircledBlack" #xE0E4)
    ("noteheadCircledHalf" #xE0E5)
    ("noteheadCircledWhole" #xE0E6 )
    ("noteheadCircledDoubleWhole" #xE0E7)
    ("noteheadCircledBlackLarge" #xE0E8)
    ("noteheadCircledHalfLarge" #xE0E9)
    ("noteheadCircledWholeLarge" #xE0EA)
    ("noteheadCircledDoubleWholeLarge" #xE0EB)
    ("noteheadCircledXLarge" #xE0EC)
    ("noteheadLargeArrowUpDoubleWhole" #xE0ED)
    ("noteheadLargeArrowUpWhole" #xE0EE)
    ("noteheadLargeArrowUpHalf" #xE0EF)
    ("noteheadLargeArrowUpBlack" #xE0F0)
    ("noteheadLargeArrowDownDoubleWhole" #xE0F1)
    ("noteheadLargeArrowDownWhole" #xE0F2)
    ("noteheadLargeArrowDownHalf" #xE0F3)
    ("noteheadLargeArrowDownBlack" #xE0F4)
    ("noteheadParenthesisLeft" #xE0F5)
    ("noteheadParenthesisRight" #xE0F6)
    ("noteheadCircleSlash" #xE0F7)
    ("noteheadHeavyX" #xE0F8)
    ("noteheadSlashVerticalEnds" #xE100)
    ("noteheadHeavyXHat" #xE0F9)
    ("noteheadWholeFilled" #xE0FA)
    ("noteheadHalfFilled" #xE0FB)
    ("noteheadDiamondOpen" #xE0FC)
    ("noteheadDoubleWholeAlt" uniE0A0.salt01)
    ("noteheadDoubleWholeSmall" uniE0A0.ss01)
    ("noteheadDoubleWholeOversized" uniE0A0.ss05)
    ("noteheadDoubleWholeSquareOversized" uniE0A1.ss05)
    ("noteheadWholeSmall" uniE0A2.ss01)
    ("noteheadWholeOversized" uniE0A2.ss05)
    ("noteheadHalfSmall" uniE0A3.ss01)
    ("noteheadHalfOversized" uniE0A3.ss05)
    ("noteheadBlackSmall" uniE0A4.ss01)
    ("noteheadBlackOversized" uniE0A4.ss05)
    ("noteheadBlackParens" uniE0F5_uniE0A4_uniE0F6)
    ("noteheadHalfParens" uniE0F5_uniE0A3_uniE0F6)
    ("noteheadWholeParens" uniE0F5_uniE0A2_uniE0F6)
    ("noteheadDoubleWholeParens" uniE0F5_uniE0A0_uniE0F6)))

;; =======================================================

(defun midi-channel-position (channels number-ckn-midi)

(let* (
(action1 (mapcar (lambda (x) (if (equal x number-ckn-midi) 1 -1)) channels))
(action2 (choose-to-rest action1))
(action3 (om:arithm-ser 1 (length (remove nil (mapcar (lambda (c) (plusp c)) action1))) 1)))
(mapcar (lambda (y) (position y action2)) action3)))

;; =======================================================

(defun ckn-notehead-numbers (x)

(case x
    (16 nil)
    (20 57513)
    (25 57511)
    (40 57513)
    (63 nil)
    (118 57592)
    (271 nil)
    (274 57540)
    (300 nil)
    (328 nil)
    (425 nil)
    (497 nil)))
    
;; =======================================================

(defun ckn-headnote-extras (voice)

(let* (
(action1 (om::sort-list (om::remove-dup (om::flat (om::lchan voice)) 'eq 1)))
(action2 (loop :for x :in action1 :collect (let* (
                                            (action1 (notehead-char-codes (ckn-notehead-numbers x))))
                                            (if 
                                              (equal action1 nil)
                                              nil 
                                              (make-value 'head-extra 
                                                    (list (list :head-char action1)))))))
(action3 (om::mat-trans (list action2
                            (loop :for x :in action1 :collect (midi-channel-position (om::lchan voice) (list x))))))

(action4 (mat-trans (remove nil (mapcar (lambda (x) (if
                                      (or (equal (second x) nil)
                                          (equal nil (first x))) 
                                    nil
                                    x)) action3)))))
(om::add-extras voice (first action4) (second action4))))

;; =======================================================

(defclass ckn-extras ()
  (
(ckn-instrument-number :initform nil :initarg :ckn-instrument-number :accessor ckn-instrument-number)
(ckn-instrument-pathname :initform nil :initarg :ckn-instrument-pathname :accessor ckn-instrument-pathname)
(ckn-amplitude-normalization :initform nil :initarg :ckn-amplitude-normalization :accessor ckn-amplitude-normalization))) 




