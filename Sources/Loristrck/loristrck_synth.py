#!/usr/bin/env python
"""
This scripts has following dependencies:
  * csound (>= 6.15) in order to play .mtx files (see loristrck_pack)
  * the python module `sounddevice` to play a sound directly

"""
import argparse
import os
import loristrck
import tempfile
import sys


parser = argparse.ArgumentParser()
parser.add_argument("--speed", default=1, type=float, 
                    help="Playback speed. Pitch is not modified")
parser.add_argument("--transposition", default=0, type=float, help="Transposition in semitones (independent from playback speed)")
parser.add_argument("--noise", default="gaussian", choices=['uniform', 'gaussian'],
                    help="Noise type used for the residual part when synthesizing a .mtx file. The original implementation uses "
                         "gaussian noise")
parser.add_argument('--quality', default=2, type=int, 
                    help="Oscillator quality when playing a .mtx file. 0: fast, 1: fast + freq. interpolation, 2: linear interpolation,"
                         " 3: linear interpolation + freq. interpolation")
# quality: 0 - fast
#          1 - fast + freq interpol
#          2 - oscil interpol
#          3 - oscil interpol + freq interpol

parser.add_argument("-o", "--out", default="dac", 
                    help="Play / Save the samples. Use dac to play in realtime (.mtx files only), or a .wav "
                         "of .aif path to synthesize to that file")
parser.add_argument("inputfile", help="A .sdif or .mtx file (as generated via loristrck_pack")
args = parser.parse_args()

flag = 0

if args.noise == 'gaussian':
    flag += 1

if args.quality == 1:
    flag += 4
elif args.quality == 2:
    flag += 2
elif args.quality == 3:
    flag += 6 

def printerr(s):
    print(s, file=sys.stderr)


def play_sdif(inputfile, out, sr=44100, speed=1, transposition=0):
    partials, labels = loristrck.read_sdif(inputfile)
    if speed != 1:
        partials = loristrck.util.partials_stretch(partials, factor=1/speed, inplace=True)
    if transposition != 0:
        partials = loristrck.util.partials_transpose(partials, transposition, inplace=True)
    print("Synthesizing samples via loristrck")
    samples = loristrck.synthesize(partials, sr)
    if out == "dac":
        play_samples(samples, sr=sr)
    else:
        loristrck.util.wavwrite(out, samples, sr)
        

def play_samples(samples, sr):
    try:
        import sounddevice as sd
        sd.play(samples, sr, blocksize=4096)
        sd.wait()
    except ImportError:
        play_samples_via_soundfile(samples, sr)


def play_samples_via_soundfile(samples, sr):
    outfile = tempfile.mktemp(suffix=".wav", prefix="loristrck-")
    loristrck.wavwrite(outfile, samples, sr)
    play_sndfile(outfile)


def play_sndfile(sndfile):
    loristrck.util.open_with_standard_app(sndfile) 


def csound_in_path():
    import shutil
    return shutil.which("csound") is not None


ext = os.path.splitext(args.inputfile)[1].lower()
if ext == '.sdif':
    play_sdif(args.inputfile, out=args.out, speed=args.speed, transposition=args.transposition)
elif ext == '.mtx':
    from loristrck import play
    try:
        play.play_mtx(mtxfile=args.inputfile, out=args.out, speed=args.speed, freqscale=loristrck.util.i2r(args.transposition),
                      noisetype=args.noise, linearinterp=args.quality>=2, freqinterp=args.quality==1 or args.quality==3)
    except KeyboardInterrupt:
        pass
else:
    print("Input should be a sdif file or a packed matrix (.mtx)", file=sys.stderr)
    sys.exit(-1)
