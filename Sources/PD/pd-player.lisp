(in-package :om)

;; ============================================= MICROTONAL PLAYER WITH PUREDATA ==========================

(setf *PureData-PLAY-STATE* nil)


;; =============================================================================================================


(add-preference-section :externals "PD OSC player" nil '(:SoundFont-Folder :SoundFont :PureData-Player)) 
(add-preference :externals :PureData-Player "PureData Player" :bool t "If checked, the PureData player will be used to play the score.")

(add-preference :externals :SoundFont-Folder "SoundFound Folder" :folder "Choose a SoundFound Folder")


;; =============================================================================================================

(let* (
      (thepath (get-pref-value :externals :SoundFont-Folder))
      (thefilelist-sf2 (om::om-directory thepath 
                                    :type "sf2"
                                    :directories t 
                                    :files t 
                                    :resolve-aliases nil 
                                    :hidden-files nil))
      
      (thefilelist-sf3 (om::om-directory thepath 
                                    :type "sf3"
                                    :directories t 
                                    :files t 
                                    :resolve-aliases nil 
                                    :hidden-files nil))
      
      (thefilelist (append thefilelist-sf2 thefilelist-sf3))
      
      (check_files_inside_folder 
                  (loop :for loop-files :in thefilelist 
                        :collect (if 
                                    (system::directory-pathname-p loop-files)
                                    (search-inside-some-folder loop-files extension)
                                    loop-files))))
      (setf *all-available-fonts* (mapcar (lambda (sf2&sf3) (get-filename sf2&sf3)) (remove nil (om::flat check_files_inside_folder)))))


(add-preference :externals :SoundFont "SoundFound" *all-available-fonts* (car *all-available-fonts*))
;;; ============================================

(defmethod puredata-player ((voice voice) caller)


(if *PureData-PLAY-STATE*
      (let* ()
                  (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996)
                  (setf *PureData-PLAY-STATE* nil))



(let* ()
            (mp:process-run-function "Open PD"
                 () 
                  (lambda ()
                          (let* () 
                                (mp:process-run-function "Run PD in Backgroud!"
                                            () 
                                              (lambda () (pd~ 
                                                            (pd-define-patch "Microtonal-player.pd") 
                                                                :var (list (om::x-append 'soundfont (probe-file (om::string+ (om::string+ (get-pref-value :externals :SoundFont-Folder) (get-pref-value :externals :SoundFont))))))
                                                                :gui nil 
                                                                :offline nil 
                                                                :sound-out (tmpfile "casa.wav") 
                                                                :verbose nil))))))

            (setf *pd-is-open* nil)

            (om-start-udp-server 3320 "127.0.0.1" (lambda (msg) (let () (if  (equal (car (cdr (osc-decode msg))) 100.0)
                                                                              (let* () (setf *pd-is-open* t) nil)
                                                                              ))))

            (loop :with pd-start = nil 
                  :while (null *pd-is-open*)
                  :do (sleep 0.01))

            (om::om-print "PD is open!" "OM-CKN")


            (loop :for udp-server :in *running-udp-servers*
                  :do (if (equal (mp:process-name (third udp-server)) "UDP receive server on \"127.0.0.1\" 3320")
                  (let* () (om::om-stop-udp-server (third udp-server)))))
            
            
            (let* (
                  (score-lonset (lonset voice))
                  (dx-lonset (om::x-append (car score-lonset) (om::x->dx score-lonset)))
                  (score-data (mat-trans (list (om::lmidic voice) (om::lvel voice) (om::lchan voice) (om::ldur voice)))))
                  (setf *PureData-PLAY-STATE* t)
                  (loop :for onsets :in dx-lonset
                        :for notes :in score-data 
                        :while *PureData-PLAY-STATE*
                        :do (let* (
                                    (the-notes (list notes)))
                                    (sleep (om::ms->sec onsets))
                                    (mapcar (lambda (x) 
                                                (mapcar 
                                                      (lambda (notes lvel lchan ldur) 
                                                            (let* (
                                                                  (data2send (om::x-append notes lvel lchan ldur))
                                                                  (format-msg (om::osc-msg "/note" data2send)))
                                                                  (om::osc-send format-msg "127.0.0.1" 1996))) (first x) (second x) (third x) (fourth x))) the-notes)))
                              
                        (sleep (ms->sec (car (last dx-lonset))))
                        (sleep (ms->sec (car (last dx-lonset))))
                        (box-player-stop caller) ;; fazer ele se atualizar??????????
                        (if *PureData-PLAY-STATE* (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996))
                        (if *PureData-PLAY-STATE* (setf *PureData-PLAY-STATE* nil))))))
      

;; =====================================================================
;; Redefinindo o metodo que toca a partitura

(defmethod player-stop-object ((self scheduler) (object score-element))
  (send-current-midi-key-offs object)
  (when (and (equal :auto-bend (get-pref-value :score :microtone-bend))
             *micro-channel-mode-on*)
    (loop for p in (collec-ports-from-object object) do (micro-reset p)))
  
  (if (get-pref-value :externals :PureData-Player)
      (let* () 
              (om::om-print "Closing PD" "OM-CKN")
              (om::osc-send (om::osc-msg "/quit-pd" 0) "127.0.0.1" 1996)))
   
  (call-next-method))

;; =====================================================================

(defmethod PD-player-play-object ((self scheduler) (object score-element) (caller ScoreBoxEditCall) &key parent interval)
  
  (declare (ignore parent interval))
  
      (mp:process-run-function "Open PD" () (lambda () (puredata-player object caller))))
      ;(box-player-start object))

;;; =======================

(defmethod play/stop-boxes ((boxlist list))
  (let* ((play-boxes (remove-if-not 'play-box? boxlist)))
      
      (if (find-if 'play-state play-boxes)
        
        ;;; stop all
        (mapc #'(lambda (box) (player-stop-object *general-player* (get-obj-to-play box)) (box-player-stop box)) play-boxes)
      
        ;;; start all
        (if (get-pref-value :externals :PureData-Player)
            
            (if (member (type-of (car boxlist)) (list 'scoreboxeditcall 'voice 'chord 'chord-seq 'note))
                  (mapc (lambda (box) (when (play-obj? (get-obj-to-play box)) (PD-player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box))) play-boxes)
                  (mapc (lambda (box) (when (play-obj? (get-obj-to-play box)) (player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box))) play-boxes))

            (mapc 
                  (lambda (box) 
                      (when (play-obj? (get-obj-to-play box)) (player-play-object *general-player* (get-obj-to-play box) box) (box-player-start box)))
            play-boxes)))))

;; ========================================================================

