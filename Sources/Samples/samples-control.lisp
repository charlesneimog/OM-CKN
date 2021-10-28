(in-package :om)

;  ========================

(defclass! vst2 ()
    ((vst2-path :initform nil :initarg :vst2-path :accessor vst2-path)))
; ====
(defclass! vst3 ()
    ((vst3-path :initform nil :initarg :vst3-path :accessor vst3-path)))
; ====

(defclass! vst3-code ()
    (
        (plugin-path :initform nil :initarg :plugin-path :accessor plugin-path)
        (py :initform nil :initarg :py :accessor py)
        (out-sound :initform nil :initarg :out-sound :accessor out-sound)))
; ====

(defclass! vst3-main-code ()
    (
        (main-code :initform nil :initarg :main-code :accessor main-code)))

;; ======================================

(defmethod! plugins-list ((modo number))
:icon '17359
:menuins '((0 (("vst2" 1) ("vst3" 2) ("vts2 and vst3" 3))))
:doc "
From OM-Sox
Returns a list of file pathnames of the dll plugins. Connect it to a LIST-SELECTION object."

(if (= modo 3)
    (let* (
            (thepath (get-pref-value :externals :plugins))
            (thefile-vst2 (search-plugins "dll"))
            (thefile-vst3 (search-plugins "vst3")))
            (mapcar (lambda (x) (name-of-file x)) (x-append thefile-vst2 thefile-vst3)))

    (let* (
            (thepath (get-pref-value :externals :plugins))
            (thefilelist (search-plugins (if (= modo 1) "dll" "vst3"))))
            (mapcar (lambda (x) (name-of-file x)) thefilelist))))   

; ===========================

(defmethod! plugins-define-fxp ((fxp-presets string))
:initvals '(nil)
:indoc '("Define the fxp-presets to use in your sound.") 
:icon '17359
:doc "It defines the fxp-presets to use in your sound with the object ckn-VST2. You need be careful because the binaries that I am using not accept very long paths. Then prefer smaller paths."

(let* (
(action1 (probe-file (merge-pathnames fxp-presets (namestring (get-pref-value :externals :fxp-presets))))))
(if (equal nil action1) (let* ((action1 (om-print "fxp-presets not found" "Abort"))) (abort)) (namestring action1))))

;  ========================

(defmethod! plugins-define ((plugin-name string))
:initvals '("My awesome plugin.vst3")
:indoc '("Define the plugin-name to use in your sound, it need to be vst2 or vst3.") 
:icon '17359
:doc "It defines the plugin-name to use in your sound with the object ckn-VST2."

(let* ( 
        (all-plugins (x-append (search-plugins "dll") (search-plugins "vst3")))
        (name-of-all-plugins (mapcar (lambda (x) (name-of-file x)) all-plugins))
        (position-of-plugin (position plugin-name name-of-all-plugins :from-end nil :test (lambda (x y) (equal x y))))
        (action1 (probe-file (nth position-of-plugin all-plugins)))
        (action2 (if (equal nil action1) (let* () (om-print "Plugin not found" "Abort ::") (abort)) action1))
        (action3 (cdr (om::string-to-list (name-of-file action2) "."))))
        (if 
            (equal '("dll") action3)
            (make-value 'vst2 (list (list :vst2-path action2)))
            (make-value 'vst3 (list (list :vst3-path action2))))))


;  ========================

(defmethod! plugins-parameter-index ((plugin-path vst2))
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 plugin."

(let* (
      (python-code (format nil
                    "
import dawdreamer as daw

SAMPLE_RATE = 44100
BUFFER_SIZE = 128 
SYNTH_PLUGIN = r'~d'
print('==================================')
print('VST2 Index of Parameters')
engine = daw.RenderEngine(SAMPLE_RATE, BUFFER_SIZE)
synth = engine.make_plugin_processor('my_synth', SYNTH_PLUGIN)
all = list(synth.get_plugin_parameters_description())

for x in range(len(all)):
    name = synth.get_parameter_name(x)
    print (str(x) + ' : ' + str(name))
"                               
                                    (vst2-path plugin-path)))
      (save-python-code (om::save-as-text python-code (om::outfile "parameters-vst2-sound.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))
      "Done! Check the listener"))

;  ==========================================

(defmethod! plugins-parameter-index ((plugin-path vst3))
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 plugin."

(let* (
      (python-code (format nil
                    "
from pedalboard import load_plugin
plugin = load_plugin(r'~d')
Todos_parametros = (plugin.parameters.keys())
list_of_numbers = list(range(len(Todos_parametros)))
print(' ==================================  ')
print('VST3 Index of Parameters')
for (x,y) in zip(Todos_parametros, list_of_numbers):
    print (str(y) + ' : ' + x )
"                               
                                    (vst3-path plugin-path)))
      (save-python-code (om::save-as-text python-code (om::outfile "parameters-vst3-sound.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))
      "Done! Check the listener"))

;  ==========================================

(defmethod! plugins-valid-parameters ((plugin-path vst3) (parameter-index integer))
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 plugin."

(let* (
      (python-code (format nil
                    "
from pedalboard import load_plugin
plugin = load_plugin(r'~d')
all_parameters = list(plugin.parameters.keys())
choosed_parameter = all_parameters[~d]
print('================ OM-CKN ==============')
print(f'Showing the good values for the {choosed_parameter}.')
parameter = getattr(plugin, choosed_parameter)
goodvalues = parameter.valid_values

mynewlist = []
for item in goodvalues:
    try:
        int_value = int(item)
    except ValueError:
        pass
    else:
        mynewlist.append(item)

first_number = goodvalues[0]
last_number = goodvalues[-1]

if (goodvalues == mynewlist):
    print (f'The good values are one range starting from {first_number} and finished in {last_number}.')
else:
    print (f'The good values are {goodvalues}.')
"                               
                                    (vst3-path plugin-path) parameter-index))
      (save-python-code (om::save-as-text python-code (om::outfile "valid-vst3-parameter.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))
      "Done! Check the listener"))


;; ================================
(defmethod! plugins-process ((sound pathname) (sound-out string) (plugin-path vst2) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."
(plugins-process (make-value-from-model 'sound sound nil) sound-out plugin-path parameter_index verbose))

;; ================================
(defmethod! plugins-process ((sound pathname) (sound-out pathname) (plugin-path vst2) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."
(plugins-process (make-value-from-model 'sound sound nil) (namestring sound-out) plugin-path parameter_index verbose))

;; ================================
(defmethod! plugins-process ((sound string) (sound-out pathname) (plugin-path vst2) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."
(plugins-process (make-value-from-model 'sound sound nil) (namestring sound-out) plugin-path parameter_index verbose))

;  ========================
(defmethod! plugins-process ((sound sound) (sound-out pathname) (plugin-path vst2) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."
(plugins-process sound (namestring sound-out) plugin-path parameter_index verbose))

;; ========================================= VST3
(defmethod! plugins-process ((sound string) (sound-out string) (plugin-path VST3) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."

(let* (
        (action1 
            (concatString (loop :for parameters_values :in parameter_index
                            :collect 
                            (string+ "
setattr(" "plugin, " "all_parameters" 
                                                (format nil "[~d], " (first parameters_values)) 
                                                                (if (numberp (second parameters_values)) 
                                                                    (ckn-int2string (second parameters_values)) 
                                                                    (list->string-fun (list (second parameters_values)))) ")"))))
        (python-code (format nil
                    "
import soundfile as sf
from pedalboard import load_plugin

plugin = load_plugin(r'~d')
all_parameters = list(plugin.parameters.keys())
~d
audio, sample_rate = sf.read(r'~d')
final_audio = plugin.process(audio, sample_rate)
sf.write(r'~d', final_audio, sample_rate)
"                               
                                    (namestring (vst3-path plugin-path)) action1 sound sound-out))

        (save-python-code (om::save-as-text python-code (om::outfile "process-vst3.py")))
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-cmd-line (string+ "python " prepare-cmd-code))
      "Done! Check the listener"))

;; ========================================= VST3
(defmethod! plugins-multi-processes ((sound string) (sound-out string) (plugin-path VST3) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."

(let* (
        (action1 
            (concatString (loop :for parameters_values :in parameter_index
                            :collect 
                            (string+ "
setattr(" "plugin, " "all_parameters" 
                                                (format nil "[~d], " (first parameters_values)) 
                                                                (if (numberp (second parameters_values)) 
                                                                    (ckn-int2string (second parameters_values)) 
                                                                    (list->string-fun (list (second parameters_values)))) ")"))))
        (python-code (format nil
                    "
~d
audio, sample_rate = sf.read(r'~d')
final_audio = plugin.process(audio, sample_rate)
sf.write(r'~d', final_audio, sample_rate)
"                               
                action1 sound sound-out)))                              
(make-value 'vst3-code (list (list :plugin-path (namestring (vst3-path plugin-path))) (list :py python-code) (list :out-sound sound-out)))))

;  =====================================================
(defmethod! plugins-process-code ((code vst3-main-code))
:initvals '(nil)
:indoc '("Process vst3 plugin code in Python") 
:icon '17359
:doc ""
(om-print "================================")
(om-print "This can take some time" "OM-CKN ::")
(om-print "================================")
(let* (
        (plugin-name (plugin-path (car (main-code code))))
        (load-plugin-once (format nil "
import soundfile as sf
import time
start_time = time.time()
from pedalboard import load_plugin
plugin = load_plugin(r'~d')
all_parameters = list(plugin.parameters.keys())
        " (namestring plugin-name)))
        (time_execution (format nil "
end_time = time.time()
time_elapsed = round((end_time - start_time), 2)
print(f'O tempo gasto foi de {time_elapsed} segundos')
"))
        (concat-parameters-codes 
            (concatString (loop :for y :in (main-code code) 
                                :collect (py y))))
        (concat-all-the-code (concatString (list load-plugin-once concat-parameters-codes time_execution)))
        (save-python-code (om::save-as-text concat-all-the-code (om::outfile "all-code-vst3.py")))
        (sleep 1)
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-shell (string+ "python " prepare-cmd-code))
        (loop-until-probe-file (out-sound (car (last (main-code code)))))
        (loop :for y :in (main-code code) :collect (out-sound y))))

;; ======================================
(defmethod! voice->audio ((voice voice) (plugin_path vst2))
:initvals '(nil)
:indoc '("Process one voice to audio.") 
:icon '17359
:doc "It will genereta one audio using voices and vst2-plugins."

(let* (
        (true-durations (om6-true-durations voice))
        (to-seconds (om::ms->sec true-durations))
        (start_notes (om::dx->x 0 to-seconds))
        (to-chords (mapcar (lambda (x) (length x)) (lmidic voice)))
        (all_notes (om-round (om/ (flat (lmidic voice)) 100)))
        (all_velocities (flat (lvel voice)))
        (true-durations-with-chords (flat (loop :for to-chords-loop :in to-chords
                                                :for start_notes-loop :in true-durations
                                                :collect (om::repeat-n start_notes-loop to-chords-loop))))

        (chords-and-durations (flat (loop :for to-chords-loop :in to-chords
                                          :for start_notes-loop :in start_notes
                                          :collect (om::repeat-n start_notes-loop to-chords-loop))))
        (all_python_code 
            (concatString 
                (loop   :for note_loop :in (flat all_notes)
                        :for velocity_loop :in (flat all_velocities)
                        :for start_loop :in (flat chords-and-durations)
                        :for duration_loop :in (flat true-durations-with-chords)
                        :collect 
(string+    

"synth.add_midi_note(" (ckn-int2string note_loop) "," 
                                            (ckn-int2string velocity_loop) "," 
                                            (ckn-int2string start_loop) "," 
                                            (ckn-int2string duration_loop) ")
                                            
"))))     

        (durations (om+ 2 (reduce (lambda  (x y) (om+ x y)) to-seconds)))
        (python-code (format nil "
import dawdreamer as daw
from scipy.io import wavfile

SAMPLE_RATE = 44100
BUFFER_SIZE = 128 
SYNTH_PLUGIN = r'~d'
engine = daw.RenderEngine(SAMPLE_RATE, BUFFER_SIZE)
engine.set_bpm(~d) 
# here the code
synth = engine.make_plugin_processor('my_synth', SYNTH_PLUGIN)
~d
graph = [
  (synth, [])  
]
DURATION = ~d 
engine.load_graph(graph)
engine.render(DURATION)  
audio = engine.get_audio()  
wavfile.write(r'~d', SAMPLE_RATE, audio.transpose()) 
"
        (vst2-path plugin_path) (tempo voice) all_python_code durations (namestring (om::outfile  "voice2midi.wav" :subdirs "om-ckn"))))
        (save-python-code (om::save-as-text python-code (om::outfile "voice2midi.py")))
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-cmd-line (string+ "python " prepare-cmd-code))
        (namestring (om::outfile  "voice2midi.wav" :subdirs "om-ckn"))))


;; ======================================

(defmethod! list-fxp-presets (&key (type nil) (unix nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil) (path nil))
:icon '17359
:doc "
From OM-Sox
Returns a list of file pathnames of the fxp Presets. Connect it to a LIST-SELECTION object."

            (let* ((thepath (get-pref-value :externals :fxp-presets))
                  (thefilelist (om-directory thepath 
                                             :type "fxp" :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files)))
            (mapcar (lambda (x) (name-of-file x)) thefilelist)))


;  ======================== SOX controls ================================

(defun ckn-transpose-a-sound (instrumentos desvio) 

(let* (
(action1 (string-to-list (name-of-file instrumentos) "-"))
(action2 (1- (length (om::list! action1))))
(action3 (loop :for x :in action1 :collect (string+ x "-")))
(action4 (ckn-string-name (list! (first-n action3 action2))))
(action5 (string+ action4 (format nil "~d-cents" desvio) ".wav"))
(action6 (merge-pathnames (string+ "om-ckn/" action5) (outfile "")))
(action7 (namestring action6)))
(ckn-cmd-line (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
          " "
          (list->string-fun (list instrumentos))
          " "
          (list->string-fun (list action7))
          (format nil " pitch ~d" desvio)))
; (print (format nil "Transpondo em ~d cents" desvio))
action6))

;; Fazer um código mais bonito

;; ===============================================================================

(defun ckn-transpose  (nome cents)

(let* (
(action1 (namestring nome)))
 (progn
  (loop with ckn-loop 
        := nil 
        :while (setf ckn-loop (equal (probe-file (ckn-transpose-a-sound action1 cents)) nil))
        :do (let* () ckn-loop))
  (probe-file (ckn-transpose-a-sound action1 cents)))))

;====================================================================================

(defun build-sound-sequence-fun (sounds fade-bewteen-sound)

(let*  (
    (action1 (om::sound-seq (first sounds) (second sounds) fade-bewteen-sound))
    (action2 (if 
                (>  (length (x-append action1 sounds)) 2)
                (x-append action1 (last-n sounds (- (length sounds) 2)))
                action1)))   
    (if (< (length action2) 2)
            (first action2)
            (setf sounds (build-sound-sequence-fun action2 fade-bewteen-sound)))))

;====================================================================================

(defun build-sound-mix-fun (sounds)
(let*  (
    (action1 (sound-mix (first sounds) (second sounds)))
    (action2 (if 
                (>  (length (x-append action1 sounds)) 2)
                (x-append action1 (last-n sounds (- (length sounds) 2)))
                action1)))
    
    (if (< (length action2) 2)
            (first action2)
            (setf sounds (build-sound-mix-fun action2)))))

;====================================================================================

(defun ckn-mc->n (note)
(case note
(3600 "C2") (3700 "C#2") (3800 "D2") (3900 "D#2") (4000 "E2") (4100 "F2") (4200 "F#2") (4300 "G2") (4400 "G#2") (4500 "A2") (4600 "A#2") (4700 "B2")
(4800 "C3") (4900 "C#3") (5000 "D3") (5100 "D#3") (5200 "E3") (5300 "F3") (5400 "F#3") (5500 "G3") (5600 "G#3") (5700 "A3") (5800 "A#3") (5900 "B3")
(6000 "C4") (6100 "C#4") (6200 "D4") (6300 "D#4") (6400 "E4") (6500 "F4") (6600 "F#4") (6700 "G4") (6800 "G#4") (6900 "A4") (7000 "A#4") (7100 "B4")
(7200 "C5") (7300 "C#5") (7400 "D5") (7500 "D#5") (7600 "E5") (7700 "F5") (7800 "F#5") (7900 "G5") (8000 "G#5") (8100 "A5") (8200 "A#5") (8300 "B5")
(8400 "C6") (8500 "C#6") (8600 "D6") (8700 "D#6") (8800 "E6") (8900 "F6") (9000 "F#6") (9100 "G6") (9200 "G#6") (9300 "A6") (9400 "A#6") (9500 "B6")    
(nil nil)))
       
;====================================================================================

(defun build-seq-of-sounds (sounds sounds-length &optional result)

(let*  (


    (action1 (first-n sounds sounds-length))
    (action2  (if (plusp (om- (length sounds) sounds-length))
                  (last-n sounds (om::om- (length sounds) sounds-length))
                action1)))

(if 
    (plusp (om- (length sounds) sounds-length))
    (setf sounds (build-seq-of-sounds action2 sounds-length (push action1 result)))

  (x-append (cdr (reverse (x-append result (list action1)))) (list (flat
                                                              (let* (
                                                                   (action1 (last (x-append result (list action1))))
                                                                   (action2 (equal (length (flat action1)) 1)))
                                                               (if action2 (x-append action1 (sound-silence 0.0001)) action1))))))))

;====================================================================================
(defun sound-seq-list-multi-threading (sounds)
(let* (
    (action1 
          (mapcar (lambda (x) (string+ "Sound-seq-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
    (action2 (ckn-make-mail-box action1))
    (action3 (let* ()
                    (loop 
                            :for sound-loop :in sounds
                            :for names-loop :in action1
                            :for mail-box-loop :in action2 
                        :do 
                        (mp:process-run-function names-loop () (lambda (x w) (mp:mailbox-send w (sound-seq-list x 0.001))) sound-loop mail-box-loop)) 
                    (loop-until-finish-process action2) ;; espera todos os processos terminarem
                    (mapcar (lambda (x) (mp:mailbox-peek x)) action2))) ;; coleta os dados
    (action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))
    (gc-all)
    (sound-seq-list action4 0.001)))

;====================================================================================
(defun sound-mix-multi (sounds)
(let* (
    (action1 
          (mapcar (lambda (x) (string+ "Sound-mix-" x)) (mapcar (lambda (x) (list->string-fun (list x))) (om::arithm-ser 1 (length sounds) 1))))
    (action2 (ckn-make-mail-box action1))
    (action3 (let* ()
                    (loop 
                            :for sound-loop :in sounds
                            :for names-loop :in action1
                            :for mail-box-loop :in action2 
                        :do 
                        (mp:process-run-function names-loop () (lambda (x w) (mp:mailbox-send w (build-sound-mix-fun x))) sound-loop mail-box-loop)) 
                    (loop-until-finish-process action2) ;; espera todos os processos terminarem
                    (mapcar (lambda (x) (mp:mailbox-peek x)) action2))) ;; coleta os dados
    (action4 (loop :for fim :in action3 :collect (make-value-from-model 'sound fim nil))))
    (gc-all)
    (build-sound-mix-fun action4)))

;====================================================================================

(defun voice->samples-sound-fun (voice1 pan temp-files) 
 
(let* (
(action1
    (loop   :for ckn-LOOP1 :in (choose-to-rest voice1)
            :for ckn-LOOP2 :in (om6-true-durations voice1)
            :collect
                (let*
                    ((box-choose1 (choose (lmidic voice1) ckn-LOOP1))
                    (box-choose2 (choose (lchan voice1) ckn-LOOP1))
                    (box-choose3 (choose (lvel voice1) ckn-LOOP1))
                    (box-first1 (first box-choose3))
                    (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) '(-50 50)  (choose pan ckn-LOOP1))))

(if (plusp ckn-LOOP2) ;;silencio ou nao 
;; SE FOR NOTA 
    (om::sound-fade 
        (om::sound-stereo-pan (sound-mono-to-stereo
                                (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFONICO
            ;;;;; MONOFONICO
            (sound-vol 
                (sound-cut 
                    (samples-menores (om-abs (ms->sec ckn-LOOP2)) (make-value-from-model 'sound 
                                (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2))) 
                                    (print (FULL-SOL-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1))
                                    (sound-transpose-sox 
                                        (FULL-SOL-instruments
                                            (first (approx-m box-choose1 2))
                                            (first box-choose2)
                                            box-first1)
                                    (first (om- box-choose1 (approx-m box-choose1 2))))) nil)) 0.0 (om-abs (ms->sec ckn-LOOP2)))
                (om-scale box-first1 0.001 0.999 1 110))  ;;;;; FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

            ;;;;; POLIFONICO

            (sound-mix-list 
                        (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3))))
                        (first box-choose4)
                        (second box-choose4)) 0.01 0.01)
;;silencio 
            (sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))
(if temp-files (ckn-clear-temp-files)) ;;; ================= Apagar temp files
action1))

;;; ===========================================================

(defun voice->samples-sound-om6-fun (voice1 pan temp-files) 
 
(let* (

(action1
    (loop :for ckn-LOOP1 :in (choose-to-rest voice1)
        :for ckn-LOOP2 :in (om6-true-durations voice1)
        :collect
        (let*
            ((box-choose1 (choose (lmidic (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1)) ;; fazer isso virar um chord-seq
                (box-choose2 (choose (lchan (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1))
                (box-choose3 (choose (lvel (make-instance 'chord-seq :lmidic (chords voice1))) ckn-LOOP1))
                (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) '(-50 50)  (choose pan ckn-LOOP1)))
                (box-first1 (first box-choose3)))

(if (plusp ckn-LOOP2) ;;silencio ou nÃ£o 

;; NOTA 
(sound-fade 

        (sound-stereo-pan (sound-mono-to-stereo 
            (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFÃ”NICO

            ;;;;; MONOFONICO

;;;; COLOCAR MEIO PARA APAGAR ARQUIVOS TEMPORÃRIOS

(sound-vol 
    (sound-cut 
            (samples-menores 
                (om-abs (ms->sec ckn-LOOP2)) 
                    (make-instance 'sound nil
                            (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2))) 
                                (FULL-SOL-instruments
                                    (first (approx-m box-choose1 2))
                                    (first box-choose2)
                                    box-first1)
                                (sound-transpose-sox 
                                    (FULL-SOL-instruments
                                        (first (approx-m box-choose1 2))
                                        (first box-choose2)
                                        box-first1)
                                (first (om- box-choose1 (approx-m box-choose1 2))))) nil))
        0.0 
        (om-abs (ms->sec ckn-LOOP2)))
    
    (om-scale box-first1 0.001 0.999 1 110))  ;;;;; FIMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

;;;;; Com acorde

(sound-mix-list 
            (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3))))
            (first box-choose4)
            (second box-choose4)) 0.01 0.01)

;;silencio 

            (sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))

;;; ================= Apagar temp files

(if temp-files 
        (ckn-clear-temp-files))

;;; ================= Finalizar
                                            

action1))

;;; ============================ COM ITD ===========================================

(defun voice->samples-sound-ITD-fun (voice1 pan temp-files) 
 
(let* (

    (action1
            (loop   :for ckn-LOOP1 :in (choose-to-rest voice1)
                    :for ckn-LOOP2 :in (om6-true-durations voice1)
                    :collect
        (let*
            ((box-choose1 (choose (lmidic voice1) ckn-LOOP1))
                (box-choose2 (choose (lchan voice1) ckn-LOOP1))
                (box-choose3 (choose (lvel voice1) ckn-LOOP1))
                (box-first1 (first box-choose3))
                (box-choose4 (if (equal nil (choose pan ckn-LOOP1)) 15  (choose pan ckn-LOOP1))))

(if (plusp ckn-LOOP2) ;;silencio ou NAO 

;; NOTA 
(sound-fade 
        (ITD-Sound  (if (om< (length box-choose1) 2) ;; MONOFONICO OU POLIFONICO
                            (sound-vol 
                                    (sound-cut (samples-menores (om-abs (ms->sec ckn-LOOP2)) 
                                        (make-value-from-model 'sound
                                                (if (equal (list 0) (om- box-choose1 (approx-m box-choose1 2)))
                                                    (FULL-SOL-instruments (first (approx-m box-choose1 2)) (first box-choose2) box-first1)
                                                    (sound-transpose-sox 
                                                                (FULL-SOL-instruments 
                                                                        (first (approx-m box-choose1 2)) 
                                                                        (first box-choose2) box-first1)
                                        (first (om- box-choose1 (approx-m box-choose1 2))))) nil))
                                    0.0 
                                    (om-abs (ms->sec ckn-LOOP2)))
        
                            (om-scale box-first1 0.001 0.999 1 110))

                        ;;;;; Com acorde
                (sound-mix-list 
                        (acordes-de-samples (om::om-abs (ms->sec ckn-LOOP2)) box-choose1 box-choose2 box-choose3)))
                            box-choose4) 0.001 0.001)
;;silencio 
(sound-fade (sound-silence (om-abs (ms->sec ckn-LOOP2)) 2) 0.01 0.01))))))
;;; ================= Apagar temp files

(if temp-files (ckn-clear-temp-files))

;;; ================= Finalizar                        
action1))

;;; ================================================================================

(defun samples-menores (ckn-time ckn-sound)

(if (om::om> ckn-time (om::sound-dur (make-value-from-model 'sound ckn-sound nil))) 
    (om::sound-seq ckn-sound (sound-silence (om::om+ (om::om- ckn-time (sound-dur ckn-sound)) 0.03)) 0.01)
    ckn-sound))

;;; ================================================================================

(defun acordes-de-samples (ckn-time midic channel velocity)

    (loop :for ckn-LOOP1 :in midic
        :for ckn-LOOP2 :in channel
        :for ckn-LOOP3 :in velocity
        :collect (let* (
                    (action1 
                        (make-value-from-model 'sound
                            (if 
                                (equal 0 (om- ckn-LOOP1 (approx-m ckn-LOOP1 2)))
                                
                                    (FULL-SOL-instruments 
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                    (sound-transpose-sox
                                            (FULL-SOL-instruments
                                                (approx-m ckn-LOOP1 2)
                                                ckn-LOOP2
                                                ckn-LOOP3)
                                            (om- ckn-LOOP1 (approx-m ckn-LOOP1 2))))
                            nil)))          
                        (sound-vol (sound-cut (samples-menores ckn-time action1) 0.0 ckn-time) (om-scale ckn-LOOP3 0.001 1 1 127)))))



;; ====================================================== THIS IS A AUTO-PROMOTION ================================= 

(let* () 
      (eval (flat (get-slot-val 
                     (let
                         ((tb
                           (make-value-from-model 'textbuffer 
                                (probe-file (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN")))) nil)))
                       (setf (reader tb) :lines-cols) tb) "CONTENTS")))

    (if *first-time-load*
        (let* () 
            (save-as-text '(((defvar *first-time-load* nil))) (merge-pathnames "first-load.txt" (lib-resources-folder (find-library "OM-CKN"))))
            (hqn-web:browse "https://www.charlesneimog.com/")
            (print (format nil "
If you want to work with python you need:
      1. Download python 3.
      2. Download the pip for python 3.
      3. Put this code in your terminal: 
            pip install matplotlib 
            pip install numpy 
            pip install mplot3d
            pip install mpl_toolkits
            pip install pedalboard
            pip install soundfile
      And vo ala. 
")))))

;;; ================================================================================

(defun save-temp-sounds (sounds &optional if-needed) 
    (let* (
            (first-action1 
                (mapcar 
                    (lambda (x) (string+ "Sound-" if-needed x))
                        (mapcar (lambda (x) (format nil "~6,'0D" x)) (om::arithm-ser 1 (length sounds) 1)))))
      
            (loop :for loop-sound :in sounds
                :for loop-names :in first-action1
                :collect (ckn-gc-all (om:save-sound loop-sound (merge-pathnames "om-ckn/" (outfile (string+ loop-names ".wav"))))))))


;;; ================================================================================

(defun save-temp-sounds-index (sounds index) 
    (let* (
            (first-action1 
                (mapcar (lambda (x) (string+ "Sound-" x)) (mapcar (lambda (x) (format nil "~6,'0D" x)) (list index)))))
      
            (loop :for loop-sound :in (list sounds)
                :for loop-names :in first-action1
                :collect (om:save-sound loop-sound (print (merge-pathnames "om-ckn/" (outfile (string+ loop-names ".wav"))))))))

;;; ================================================================================

(defun sox-sequence (sounds sequence-name)

(let* (
(action1 (save-temp-sounds sounds))
(action2 (om::save-sound (first sounds) (merge-pathnames "om-ckn/" (outfile (string+ "Sound-001" ".wav")))))
(action3 (x-append action2 (first-n action1 (1- (length sounds)))))
(action4 (ckn-cmd-line 
          (string+ 
           (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
           " "
           "--combine sequence " 
           (list->string-fun (list (namestring (merge-pathnames "om-ckn/" (outfile "*.wav")))))
           " "
           (list->string-fun (list (namestring (outfile sequence-name)))))))
(action5 (loop-until-probe-file (outfile sequence-name))))
(sleep 5)
(ckn-clear-temp-files)
action5))

;;; ================================================================================

(defun sound-vol-sox-fun (sounds volume)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (outfile (string+ (first (om::string-to-list (name-of-file sound-in-path) ".")) "-" (ckn-int2string (om-random 1000 9999)) "-vol-correction" ".wav"))))))
  (action-sound-vol (format nil " -v ~d " volume))
  (line-command 
    (string+ sox-path " " action-sound-vol " " (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out)))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
  ;(ckn-clear-the-file sounds)
    (car sound-in-out)))

;;; ================================================================================

(defun sound-mix-sox-fun (sounds name)
    (let* (
            (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
            (sound-in-path (names-to-mix sounds))
            (sound-in-out 
                (list (namestring (merge-pathnames "om-ckn/" 
                    (outfile (string+ name "-" (ckn-int2string (om-random 1000 9999)) "-mix-sound" ".wav"))))))
            (line-command 
                (string+ sox-path " " " --combine mix " " "  sound-in-path " " (list->string-fun sound-in-out))))
            (ckn-cmd-line line-command)
            ;(ckn-clear-the-file sounds)
            (loop-until-probe-file (car sound-in-out))
            (car (om::list! sound-in-out))))

(compile 'sound-mix-sox-fun)
;;; ================================================================================

(defun sound-mix-sox-responsive (sounds nomes number)
(let*  (
       (action1 (loop-in-parts (flat sounds) 20 20))
       (names (arithm-ser 1 (length (flat action1)) 1))
       (after-number (+ number 1))
       (action2 (mapcar (lambda (x y) (sound-mix-sox (flat x) (string+ (ckn-int2string (om-random 1000 9999)) (ckn-int2string y) "-inside"))) action1 names)))
       (if (om::om= (length (flat action2)) 1)
           (car (flat action2))
         (if (om::om< (length (flat action2)) 20)
         (sound-mix-sox (flat action2) (string+ nomes "-finish"))
         (setf sounds (sound-mix-sox-responsive action2 nomes after-number))))))

(compile 'sound-mix-sox-responsive)

;;; ================================================================================

(defun sound-seq-sox-responsive (sounds nomes number)
(let*  (
       (action1 (loop-in-parts (flat sounds) 20 20))
       (names (arithm-ser 1 (length (flat action1)) 1))
       (after-number (+ number 1))
       (action2 (mapcar (lambda (x y) (sound-seq-sox (flat x) (string+ (ckn-int2string (om-random 1000 9999)) (ckn-int2string y) "-seq-inside"))) action1 names)))
       (if (om::om= (length (flat action2)) 1)
           (car (flat action2))
         (if (om::om< (length (flat action2)) 20)
         (sound-seq-sox (flat action2) (string+ nomes "-seq-finish"))
         (setf sounds (sound-seq-sox-responsive action2 nomes after-number))))))

(compile 'sound-seq-sox-responsive)

;;; ================================================================================

(defun sound-seq-sox-fun (sounds name)
    (let* (
            (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
            (sound-in-path (names-to-mix sounds))
            (sound-in-out 
                (list (namestring (merge-pathnames "om-ckn/" 
                    (outfile (string+ name (ckn-int2string (om-random 1000 9999)) "-seq-sound" ".wav"))))))
            (line-command 
                (string+ sox-path " " " --combine sequence " " "  sound-in-path " " (list->string-fun sound-in-out)))
            (the-command (ckn-cmd-line line-command))
            (loading (loop-until-probe-file (car sound-in-out))))
            ;(ckn-clear-the-file sounds)
                (car sound-in-out)))

;;; ================================================================================

(defun sound-fade-sox-fun (sounds fade)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (outfile (string+ (first (om::string-to-list (name-of-file sound-in-path) ".")) "-" (ckn-int2string (om-random 1000 9999)) "-with-fade" ".wav"))))))
  (action-sound-fade (format nil " fade p ~d ~d" (first fade) (second fade)))
  (line-command 
    (string+ sox-path " " (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out) " " action-sound-fade))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
    (car sound-in-out)))

;;; ================================================================================

(defun sound-cut-sox-fun (sounds in out)

(let* (
  (sox-path (string+ (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))))
  (sound-in-path sounds)
  (sound-in-out 
      (list (namestring (merge-pathnames "om-ckn/" 
        (outfile (string+ (first (om::string-to-list (name-of-file sound-in-path) ".")) "-" (ckn-int2string (om-random 1000 9999)) "-vol-correction" ".wav"))))))
  (action-sound-vol (format nil " trim ~d ~d " in out))
  (line-command 
    (string+ sox-path " " (list->string-fun (list (namestring sound-in-path))) " " (list->string-fun sound-in-out) " " action-sound-vol))
  (the-command (ckn-cmd-line line-command))
  (loading (loop-until-probe-file (car sound-in-out))))
  ;(ckn-clear-the-file sounds)
    (car sound-in-out)))

;;; ================================================================================

(defun sound-mono-to-stereo-sox-fun (x)

        (ckn-cmd-line (string+ 
                (list->string-fun (list (namestring (get-pref-value :externals :sox-exe))))
                " "
                (list->string-fun (list (namestring x)))
                " "
                (list->string-fun (list (namestring (outfile (string+ (name-of-file x) "-v-stereo.wav") :subdirs "om-ckn"))))
                " "
                " channels 2 "))
        (loop-until-probe-file (outfile (string+ (name-of-file x) "-v-stereo.wav") :subdirs "om-ckn"))
(outfile (string+ (name-of-file x) "-v-stereo.wav") :subdirs "om-ckn"))

(compile 'sound-mono-to-stereo-sox-fun)
;;; ================================================================================
(compile 'voice->samples-sound-fun)
(compile 'samples-menores)
(compile 'acordes-de-samples)
(compile 'build-sound-mix-fun)
(compile 'build-sound-sequence-fun)
(compile 'normalize-chord-seq)
(compile 'ckn-transpose-a-sound) 
(compile 'ckn-string-name)
(compile 'build-seq-of-sounds)
(compile 'sound-seq-list-multi-threading)
(compile 'voice->samples-sound-ITD-fun)

