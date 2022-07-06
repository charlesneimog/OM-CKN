#!/usr/bin/env python
import argparse
import loristrck
import os

description = """
Partial tracking analysis. Produces a sdif file
"""

parser = argparse.ArgumentParser(description=description)
parser.add_argument("-r", "--resolution", type=float, default=30)
parser.add_argument("-w", "--winsize", type=float, default=-1,
                    help="The size of the mainlobe, in Hz.")
parser.add_argument("--minbps", default=2, type=int, help="Min. number of breakpoints for a partial to be packed")
parser.add_argument("--minamp", default=-90, type=float)
parser.add_argument("--mindur", default=0, type=float)
parser.add_argument("--hoptime", default=0, type=float,
                    help="The time to move the window after each analysis (default: 1/windowsize)")
parser.add_argument("--hopoverlap", default=0, type=int,
                    help="An alternative to hoptime, calculates the hop time in terms of overlapping windows")
parser.add_argument("--ampfloor", default=-90, type=float, 
                    help="min. amplitude of any breakpoint")
parser.add_argument("--sidelobe", default=-1, type=float, 
                    help="A positive dB, indicates the shape of the Kaiser window")
parser.add_argument("--residuebw", default=2000, type=float)
parser.add_argument("--freqdrift", default=-1, type=float, 
                    help="max. variation of freq between two breakpoints")
parser.add_argument("--croptime", default=-1, type=float,
                    help="Max. time correction beyond which a reassigned bp is considered not eligible")
parser.add_argument("--outfile", default=None)
parser.add_argument("--sdiftype", default="rbep", help="One of 'rbep' or '1trk'")
parser.add_argument("--fadetime", default=0, type=float, 
                    help="fade time used when a partial does not end with a 0 amp breakpoint")
parser.add_argument("sndfile")

args = parser.parse_args()

samples, sr = loristrck.util.sndreadmono(args.sndfile)

if args.hoptime:
    hoptime = args.hoptime
elif args.hopoverlap:
    winsize = args.winsize if args.winsize > 0 else args.resolution * 2
    hoptime = 1/(winsize * args.hopoverlap)
else:
    hoptime = -1  

partials = loristrck.analyze(samples, sr=sr, 
                             resolution=args.resolution, 
                             windowsize=args.winsize,
                             hoptime=hoptime,
                             freqdrift=args.freqdrift,
                             sidelobe=args.sidelobe,
                             ampfloor=args.ampfloor,
                             croptime=args.croptime,
                             residuebw=args.residuebw)

partials, rest = loristrck.util.select(partials, minamp=args.minamp, minbps=args.minbps, mindur=args.mindur)

if args.outfile is None:
    outfile = os.path.splitext(args.sndfile)[0] + ".sdif"
else:
    outfile = args.outfile

sdiftype = args.sdiftype.lower()
assert sdiftype in ('1trc', 'rbep')


loristrck.write_sdif(partials, outfile, fmt=sdiftype, fadetime=args.fadetime)

print(outfile)
