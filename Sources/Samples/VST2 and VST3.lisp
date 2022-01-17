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
        (vst3-py :initform nil :initarg :vst3-py :accessor vst3-py)
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
      (ckn-clear-the-file (om::outfile "parameters-vst2-sound.py"))
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
      ;(ckn-clear-the-file (om::outfile "parameters-vst3-sound.py"))
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
      (ckn-clear-the-file (om::outfile "valid-vst3-parameter.py"))
      "Done! Check the listener"))

;  ==========================================

(defmethod! plugins-valid-parameters ((plugin-path vst3) (parameter-index list))
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


def valid_vst3_parameter(parameter):
    choosed_parameter = all_parameters[parameter]
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

for parameter in (~d):
    valid_vst3_parameter(parameter)

"                      
                                    (vst3-path plugin-path) (py-list parameter-index)))
      (save-python-code (om::save-as-text python-code (om::outfile "valid-vst3-parameter.py")))
      (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
      (om::om-cmd-line (string+ "python " prepare-cmd-code))
      (ckn-clear-the-file (om::outfile "valid-vst3-parameter.py"))
      "Done! Check the listener"))


;; ================================
(defmethod! plugins-process ((sound pathname) (sound-out pathname) (plugin-path vst2) (parameter_index list) &optional (verbose nil)) 
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

import dawdreamer as daw
import numpy as np
from scipy.io import wavfile
import librosa

SAMPLE_RATE = 44100
BUFFER_SIZE = 512
DURATION = 15

def load_audio_file(file_path, duration=None):
  sig, rate = librosa.load(file_path, duration=duration, mono=False, sr=SAMPLE_RATE)
  assert(rate == SAMPLE_RATE)
  return sig

engine = daw.RenderEngine(SAMPLE_RATE, BUFFER_SIZE)
meu_plugin = engine.make_plugin_processor('plugin', ~d)

meu_plugin.set_parameter(0, 45) # override a specific parameter.
print(meu_plugin.get_parameter(0))  # warning, is this actually 45? Probably not. VST parameters are normally between 0 and 1.
print(meu_plugin.get_parameter_name(0))

multi =  load_audio_file(~d)

playback_processor = engine.make_playback_processor('playback', multi)

graph = [
  (playback_processor , []),  # playback takes no inputs
  (meu_plugin, ['playback'])
]

## this also works
#graph = [
#  (playback_processor , []),  # playback takes no inputs
#  (meu_plugin, [playback_processor.get_name()])
#]

engine.load_graph(graph)
engine.render(DURATION)
audio = engine.get_audio()  

wavfile.write('my_song2.wav', SAMPLE_RATE, audio.transpose())

"                               
                                    (namestring (vst2-path plugin-path)) action1 sound (namestring (outfile sound-out))))

        (save-python-code (om::save-as-text python-code (om::outfile "process-vst2.py")))
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-cmd-line (string+ "python " prepare-cmd-code))
        (ckn-clear-the-file (om::outfile "process-vst2.py"))
        (outfile sound-out)))

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
                                    (namestring (vst3-path plugin-path)) action1 sound (namestring (outfile sound-out))))

        (save-python-code (om::save-as-text python-code (om::outfile "process-vst3.py")))
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-cmd-line (string+ "python " prepare-cmd-code))
        ;(ckn-clear-the-file (om::outfile "process-vst3.py"))
        (outfile sound-out)))

;; =======================================
(defmethod! plugins-process ((sound pathname) (sound-out pathname) (plugin-path VST3) (parameter_index  list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."


(plugins-process (namestring sound) (namestring sound-out) plugin-path parameter_index verbose))
 

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
if os.path.exists(r'~d'):
    audio, sample_rate = sf.read(r'~d')
    final_audio = plugin.process(audio, sample_rate)
    sf.write(r'~d', final_audio, sample_rate)
    os.remove(r'~d')


"                               
                action1 sound sound sound-out sound)))                              
(make-value 'vst3-code (list (list :plugin-path (namestring (vst3-path plugin-path))) (list :vst3-py python-code) (list :out-sound sound-out)))))

;  =====================================================

(defmethod! plugins-multi-processes ((sound pathname) (sound-out pathname) (plugin-path VST3) (parameter_index list) &optional (verbose nil)) 
:initvals '(nil)
:indoc '("With this object you can see the index parameters of some VST2 or VST3 plugin.") 
:icon '17359
:doc "With this object you can see the index parameters of some VST2 or VST3 plugin."


(plugins-multi-processes (namestring sound) (namestring sound-out) plugin-path parameter_index verbose))


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
import os

start_time = time.time()
from pedalboard import load_plugin
plugin = load_plugin(r'~d')
all_parameters = list(plugin.parameters.keys())
"                                 (namestring plugin-name)))


        (time_execution (format nil "
end_time = time.time()
time_elapsed = round((end_time - start_time), 2)
print(f'O tempo gasto foi de {time_elapsed} segundos')
"))
        (concat-parameters-codes 
            (concatString (loop :for y :in (main-code code) 
                                :collect (vst3-py y))))
        (concat-all-the-code (concatString (list load-plugin-once concat-parameters-codes time_execution)))
        (save-python-code (om::save-as-text concat-all-the-code (om::outfile "all-code-vst3.py")))
        (sleep 1)
        (prepare-cmd-code (list->string-fun (list (namestring save-python-code)))))
        (om::om-shell (string+ "python " prepare-cmd-code))
        (loop-until-probe-file (out-sound (car (last (main-code code)))))
        (ckn-clear-the-file (om::outfile "all-code-vst3.py"))
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
        (ckn-clear-the-file (om::outfile "voice2midi.py"))
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