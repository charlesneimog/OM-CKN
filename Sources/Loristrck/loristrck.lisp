(in-package :om)

;; ======================== Partial Tracking ===========================

(defmethod! loristrck-analysis ((sound pathname) (outfile string) &key (r 60) (w -1) (ht 0) (hop-o 0) (amp -90) (fd -1) (sl -1) (rbw 2000) (sdif-type 'rbep) (minbps 2) (minamp -90) (fade-time 0))
:initvals '(NIL nil 60 -1 0 0 -90 -1 -1 2000 'rbep 2 -90 0)
:indoc ' ("Sound" "Sdif out-file" "frequency resolution" "winsize" "hoptime" "hopoverlap" "ampfloor" "freqdrift" "sidelobe" "residuebw" "croptime" "sdiftype" "minimum amplitude" "fade time")
:icon '17359
:outdoc '("Sdiffile.")
:doc "
RESOLUTION: Only one partial will be found within this distance. Usual values range from 30 Hz to 200 Hz. As a rule of thumb, when tracking a monophonic source, resolution ~= min(f0) * 0.9. So if the source is a male voice dropping down to 70 Hz, resolution=60 Hz. The resolution determines the size of the fft.  
 
WINSIZE: Hz. Is the main lobe width of the Kaiser analysis window in Hz (main-lobe, zero to zero). If not given, a default value is calculated. In general window size should be higher than the resolution (default = resolution*2)  
 
HOPTIME: The time to shift the window after each analysis (default: 1/windowsize, which results in 1x overlap). For 4x overlap and a window size of 60 Hz, hoptime should be 0.00417. See also --hopoverlap  
 
OVERLAP: If given, sets the hoptime in terms of overlapping windows. A hop overlap of 2 will result in a hop time = 1/(2*windowsize)  
 
AMPFLOOR: min. amplitude of any breakpoint, in dB (default = -120)  
 
FREQ: Hz. The maximum variation of frecuency between two breakpoints to be considered to belong to the same partial. A sensible value is between 1/2 to 3/4 of resolution: freqdrift=0.62*resolution  
 
SIDELOBE: dB (default: 90 dB). A positive dB value, indicates the shape of the Kaiser window  
 
RESIDUE: Hz (default = 2000 Hz). Construct Partial bandwidth env. by associating residual energy with the selected spectral peaks that are used to construct Partials. The bandwidth is the width (in Hz) association regions used. Defaults to 2 kHz, corresponding to 1 kHz region center spacing.  
 
TIME: sec. Max. time correction beyond which a reassigned bp is considered unreliable, and not eligible. Default: the hop time.  
 
OUTFILE: The generated sdif file  
 
SDIFTYPE: One of 'rbep' or '1trk' (case is ignored)  
 
MINBPS: Min. number of breakpoints for a partial to not be discarded  
 
MINAMP: Min. amplitude of a partial (in average) to not be discarded  
 
FADETIME: fade time used when a partial does not end with a 0 amp breakpoint.  


"

(print (format nil "

RESOLUTION: ~d Hz
WINSIZE: ~d
HOPTIME: ~d 
OVERLAP: ~d
AMPFLOOR: ~d 
FREQ: ~d
SIDELOBE: ~d
RESIDUE: ~d
SDIFTYPE: ~d 
MINBPS: ~d
MINAMP: ~d
FADETIME: ~d

 " 
r w ht hop-o amp fd sl rbw sdif-type minbps minamp fade-time))

(let* (
    (py-script (list->string-fun (list (namestring (merge-pathnames "sources/loristrck/partialtracking.py" (mypathname (find-library "OM-CKN")))))))
    (sndfile (list->string-fun (list (namestring sound))))
    (sdif-out (string+ " --outfile " (list->string-fun (list (namestring (outfile outfile))))))
    (resolution (format nil " -r ~d " r))
    (winsize (format nil " -w ~d " w))
    (h-time (format nil " --hoptime ~d " ht))
    (hop-overlap (format nil " --hopoverlap ~d " ht))
    (ampfloor (format nil " --ampfloor ~d " amp))
    (freqdrift (format nil " --freqdrift ~d " fd))
    (sidelobe (format nil " --sidelobe ~d " sl))
    (residuebw (format nil " --residuebw ~d " rbw))
    (croptime (format nil " --croptime ~d " hop-o))
    (sdiftype (format nil " --sdiftype ~d " sdif-type))
    (minbps (format nil " --minbps ~d " minbps))
    (minamp (format nil " --minamp ~d " minamp))
    (fadetime (format nil " --fadetime ~d " fade-time))
    (cmd (string+ "python " py-script " " sndfile " " sdif-out resolution winsize h-time hop-overlap ampfloor freqdrift sidelobe residuebw croptime sdiftype minbps minamp fadetime)))
    (om-cmd-line cmd)
    (loop-until-probe-file (outfile outfile))
    (outfile outfile)))


;; ====
(defmethod! loristrck-analysis ((sound sound) (outfile string) &key (r 60) (w -1) (ht 0) (hop-o 0) (amp -90) (fd -1) (sl -1) (rbw 2000) (sdif-type 'rbep) (minbps 2) (minamp -90) (fade-time 0))
:initvals ' (NIL nil 60 -1 0 0 -90 -1 -1 2000 'rbep 2 -90 0)
:indoc ' ("Sound" "Sdif out-file" "frequency resolution" "winsize" "hoptime" "hopoverlap" "ampfloor" "freqdrift" "sidelobe" "residuebw" "croptime" "sdiftype" "minimum amplitude" "fade time")
:icon '17359
:outdoc '("sdiffile.")
:doc "
RESOLUTION: Only one partial will be found within this distance. Usual values range from 30 Hz to 200 Hz. As a rule of thumb, when tracking a monophonic source, resolution ~= min(f0) * 0.9. So if the source is a male voice dropping down to 70 Hz, resolution=60 Hz. The resolution determines the size of the fft.  

WINSIZE: Hz. Is the main lobe width of the Kaiser analysis window in Hz (main-lobe, zero to zero). If not given, a default value is calculated. In general window size should be higher than the resolution (default = resolution*2)  
 
HOPTIME: The time to shift the window after each analysis (default: 1/windowsize, which results in 1x overlap). For 4x overlap and a window size of 60 Hz, hoptime should be 0.00417. See also --hopoverlap  
 
OVERLAP: If given, sets the hoptime in terms of overlapping windows. A hop overlap of 2 will result in a hop time = 1/(2*windowsize)  
 
AMPFLOOR: min. amplitude of any breakpoint, in dB (default = -120)  
 
FREQ: Hz. The maximum variation of frecuency between two breakpoints to be considered to belong to the same partial. A sensible value is between 1/2 to 3/4 of resolution: freqdrift=0.62*resolution  
 
SIDELOBE: dB (default: 90 dB). A positive dB value, indicates the shape of the Kaiser window  
 
RESIDUE: Hz (default = 2000 Hz). Construct Partial bandwidth env. by associating residual energy with the selected spectral peaks that are used to construct Partials. The bandwidth is the width (in Hz) association regions used. Defaults to 2 kHz, corresponding to 1 kHz region center spacing.  
 
TIME: sec. Max. time correction beyond which a reassigned bp is considered unreliable, and not eligible. Default: the hop time.  
 
OUTFILE: The generated sdif file  
 
SDIFTYPE: One of 'rbep' or '1trk' (case is ignored)  
 
MINBPS: Min. number of breakpoints for a partial to not be discarded  
 
MINAMP: Min. amplitude of a partial (in average) to not be discarded  
 
FADETIME: fade time used when a partial does not end with a 0 amp breakpoint.  
"

(loristrck-analysis (car (list! (if (not (file-pathname sound)) (save-temp-sounds (list sound)) (file-pathname sound))))
                    outfile :r r :w w :ht ht :hop-o hop-o :amp amp :fd fd :sl sl :rbw rbw :sdif-type sdif-type :minbps minbps :minamp minamp :fade-time fade-time))

;; ======================== synth sdif ===========================

(defmethod! loristrck-synth ((sdif sdiffile) (outfile string) &key (speed 1) (transposition_cents 0))
:initvals ' (NIL "ckn-sdif.wav" 1 0)
:indoc ' ("Sdiffile" "outfile, string with .wav" "speed of the reproduction" "transposition in cents")
:icon '17359
:outdoc '("sdif")
:doc "
SDIFFILE: The sdif file to be synthesized

OUT: Save the samples. A .wav of .aif name to synthesize to that file (saved in outfiles).

SPEED: Playback speed. Pitch is not modified.

TRANSPOSITION: Transposition in cents (independent from playback speed).

NOISE: {uniform,gaussian} Noise type used for the residual part when synthesizing a .mtx file. The original implementation uses gaussian noise

QUALITY: Oscillator quality when playing a .mtx file. 
                    0: fast  
                    1: fast + freq. interpolation
                    2: linear interpolation
                    3: linear interpolation + freq. interpolation

"

(let* (
    (py-script (list->string-fun (list (namestring (merge-pathnames "sources/loristrck/loristrck_synth.py" (mypathname (find-library "OM-CKN")))))))
    (sdif-file (list->string-fun (list (namestring (file-pathname sdif)))))
    (sound-out (string+ " --out " (list->string-fun (list (namestring (outfile outfile))))))
    (sound-speed (format nil " --speed ~d " speed))
    (transposition (format nil " --transposition ~d " (/ transposition_cents 100)))
    (cmd (string+ "python " py-script " " sdif-file " " sound-out " " sound-speed transposition)))
    (ckn-cmd-line cmd)
    (loop-until-probe-file (outfile outfile))
    (outfile outfile)))

;; ======================== synth sdif ===========================

(defmethod! loristrck-synth ((sdif pathname) (outfile string) &key (speed 1) (transposition_cents 0))
:initvals ' (NIL "ckn-sdif.wav" 1 0 "gaussian")
:indoc ' ("Sdiffile" "outfile, string with .wav" "speed of the reproduction" "transposition in cents" "noise")
:icon '17359
:outdoc '("sdif")
:doc "
SDIFFILE: The sdif file to be synthesized

OUT: Save the samples. A .wav of .aif name to synthesize to that file (saved in outfiles).

SPEED: Playback speed. Pitch is not modified.

TRANSPOSITION: Transposition in cents (independent from playback speed).

NOISE: {uniform,gaussian} Noise type used for the residual part when synthesizing a .mtx file. The original implementation uses gaussian noise

QUALITY: Oscillator quality when playing a .mtx file. 
                    0: fast  
                    1: fast + freq. interpolation
                    2: linear interpolation
                    3: linear interpolation + freq. interpolation

"

(loristrck-synth (make-value-from-model 'sdiffile sdif nil) outfile :speed speed :transposition_cents transposition_cents))
