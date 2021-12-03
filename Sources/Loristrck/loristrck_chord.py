#!/usr/bin/env python3
import argparse
import os
import sys
import loristrck
from argparse import RawTextHelpFormatter
import csv
import io

description = """
TODO
"""

parser = argparse.ArgumentParser(description=description,
                                 formatter_class=RawTextHelpFormatter)

parser.add_argument("-t", "--time", default=0.3, type=float,
                    help="Time to extract the chord")

parser.add_argument("-n", "--maxcount", default=8, type=int,
                    help="Max. number of partials")

parser.add_argument("-a", "--minamp", default=-60, type=float,
                    help="Min. amplitude of a breakpoint to be considered (in dB)")

parser.add_argument("-d", "--mindur", default=0.1, type=float,
                    help="Min. dur of a partial to be considered")

parser.add_argument("--minfreq", default=40, type=int,
                    help="Min. freq of a partial")

parser.add_argument("--maxfreq", default=5000, type=int,
                    help="Max. freq of a partial")

parser.add_argument("--sortby", default="frequency", type=str,
                    choices={'frequency', 'amplitude'})

parser.add_argument("-s", "--sound", type=str, default="",
                    help="Play / Generate a soundfile with the extracted chord. Use 'dac' to play directly")

parser.add_argument("--sounddur", type=float, default=4,
                    help="Duration of the generated sound output")

parser.add_argument("--fade", type=float, default=0.1)

parser.add_argument("--a4", default=442, type=int)

parser.add_argument("sdiffile",
                    help="The input file, a sdif file with format 1TRC or RBEP")

args = parser.parse_args()

ext = os.path.splitext(args.sdiffile)[1].lower()

if ext != '.sdif':
    print("Expected a .sdif file")
    sys.exit(-1)

partials, labels = loristrck.read_sdif(args.sdiffile)

selected_partials, rest = loristrck.select(partials,
                                           mindur=args.mindur,
                                           minamp=args.minamp,
                                           minfreq=args.minfreq,
                                           maxfreq=args.maxfreq,
                                           t0=args.time-0.05,
                                           t1=args.time+0.05)

bps = loristrck.partials_at(selected_partials, t=args.time, maxcount=args.maxcount)
chord = loristrck.breakpoints_to_chord(bps, A4=args.a4)
if args.sortby == "frequency":
    chord.sort(key=lambda note: note[1])
elif args.sortby == "amplitude":
    chord.sort(key=lambda note: note[2])

s = io.StringIO()
w = csv.writer(s)
for row in chord:
    note, freq, amp = row
    print(f"{note},{freq:.1f},{amp:.1f}")
    # w.writerow(row)
#print(s.getvalue())


def makePartials(chord, dur, ampoverride=0, margin=0):
    if ampoverride == 0:
        chord2 = [(freq, loristrck.db2amp(amp)) for note, freq, amp in chord]
    else:
        chord2 = [(freq, ampoverride) for note, freq, amp in chord]
    partials = loristrck.chord_to_partials(chord2, dur=dur, startmargin=margin, fade=args.fade, endmargin=margin)
    return partials

if args.sound:
    if args.sound == "dac":
        partials = makePartials(chord, args.sounddur, margin=0.25)
    
        sr = 44100
        samples = loristrck.synthesize(partials, samplerate=sr)
        import sounddevice as sd
        sd.play(samples, sr, blocksize=4096)
        sd.wait()
        # time.sleep(0.2)
    else:
        partials = makePartials(chord, args.sounddur)
    
        sndfile = args.sound
        assert os.path.splitext(sndfile)[1] in {'.wav', '.aif', '.aiff'}
        loristrck.partials_render(partials, sr=44100, outfile=args.sound)

