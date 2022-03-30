import os,subprocess,glob,re
import music21.musicxml.fromMxObjects
from music21 import *
from numpy import mean,std,var,ceil

def convert(path):
	outpath = path.replace('.xml','.ly').replace('XML','LY')
	outdir = os.path.join(*outpath.split('/')[:-1])
	if not os.path.exists(outdir):
		os.mkdir(outdir)
	cmd = ["musicxml2ly",path,"-o",outpath]
	subprocess.call(cmd)
	return None


def clean_ly_string(string):
	clef = re.compile(r"\\\clef \"\w+\"")
	key = re.compile(r"\\\key g \W+\w+")
	time = re.compile(r"\\time \d+\/\d+")
	breaks = re.compile(r"\\break")
	sets = re.compile(r"\\set \w+ = \W+\d+")
	once = re.compile(r"\\o\w+ \\over\w+ \w+ \W+\w+ = \W+\w+")
	color = re.compile(r'\\color "#000000"')
	page = re.compile(r"\\pageBreak")
	brackets = re.compile(r"(\[|\])")
	string = "\n".join([x.strip() for x in string.split('\n') if len(x) > 0])
	removes = [clef,key,time,breaks,sets,once,brackets,color,page]
	out = ""
	for item in removes:
		string = item.sub("",string)
	string = "\n".join([x.strip() for x in string.split('\n') if len(x) > 0])
	string = [x.strip("{").strip("}").strip("\n").replace('\n',' ') for x in string.split("}{")][0]
	spaces = re.compile(r"(\s*\w+\'+)(\s+)(\d+)")
	string = spaces.sub(r"\1\3",string)
	return string

lastBeam = ''

def process_music_for_measure(ly_string,data):
	global lastBeam,slurs
	spanner_indices = []
	marks = []
	i = -1
	for n in data:
		if n.classes[0] in ["Note","Chord","Rest"]:
			i += 1
			if n.articulations:
				marks.append([i,n.articulations])
		try:
			ss = n.getSpannerSites()
			for thisSpanner in ss:
				if 'Slur' in thisSpanner.classes:
					spanner_indices.append(i)
		except:
			pass

	marks_indices = [x[0] for x in marks]
	removeKey = re.compile(r"\\key \w+ \\\w+")
	ly_string = removeKey.sub("",ly_string).strip()
	fixNotes = re.compile(r"(\w+['|,]*)(\s*)(\d+)")
	ly_string = fixNotes.sub(r"\1\3",ly_string)
	fixRests = re.compile(r"(r)(\s)(\d+)")
	ly_string = fixRests.sub(r"\1\3",ly_string)
	fixChords = re.compile(r"< ((\w+['|,]*\s+)*) > (\d+)")	
	ly_string = fixChords.sub(r"<\1>\3",ly_string)
	getChords = re.compile(r"<((\w+['|,]*\s+)*)>(\d+)")	
	fixTuplet = re.compile(r"(\\times)(\d+/\d+)")
	ly_string = fixTuplet.sub(r"\1 \2",ly_string)
	if getChords.search(ly_string):
		chords = getChords.findall(ly_string)
		for m in chords:
			m = "<{0}>{1}".format("".join(m[0:-2]),m[-1])
			# #print m
			subber = m.strip().replace(' ','_')
			getChord = re.compile(m)
			ly_string = getChord.sub(subber,ly_string)
	ly_notes  = ly_string.split()
	for i,item in enumerate(ly_notes):
		ly_notes[i] = item.replace('_',' ')
	grace_indices = []
	note_map = []
	for i,item in enumerate(ly_notes):
		isNote = re.compile("\w+['|,]*\d+")
		isRest = re.compile(r"[s|r]\d+")
		isChord = re.compile(r"<((\w+['|,]*\s+)*)>(\d+)")	
		if isNote.match(item) or isRest.match(item) or isChord.match(item):
			note_map.append(i)
	graceOn = False
	spannerOn = False
	new_notes = []
	beam_dict = {'start':'[','stop':']','continue':'','partial':''}
	tuplet_map = []
	for i,item in enumerate(ly_notes):
		if i in note_map:
			n = data.notesAndRests[note_map.index(i)]
			if n.duration.tuplets != None:
				if len(n.duration.tuplets):
					tuplet_map.append(note_map.index(i))
	tupletOn = False
	tupletCountLimit = 0
	for i,item in enumerate(ly_notes):
		if i in note_map:
			beamType = ''
			n = data.notesAndRests[note_map.index(i)]
			isLast = note_map[-1] == i
			# #print n.hideObjectOn#print
			# beaming
			if n.isNote or n.isChord or n.isGrace:

				if tupletOn != True and len(n.duration.tuplets) > 0:
					tupletCountLimit = n.duration.tuplets[0].numberNotesActual
					tupletNormal = n.duration.tuplets[0].numberNotesNormal
					new_notes.append('\\times %s/%s {' % (tupletNormal,tupletCountLimit))
					# use limit to count out when tuplet is over
					tupletOn = True


				if len(n.beams.beamsList) > 0:
					for beam in n.beams:
						beamType = n.beams.beamsList[0].type
						beamStr = beam_dict[beamType]

					if beamType == 'start' or beamType == 'stop':
						if beamStr != lastBeam:
							item += ' '+ beamStr
							lastBeam = beamStr

					ni = note_map.index(i)

					# beams
					if beamType == 'stop' or beamType == 'start':
						beamCount = 0
					else:
						beamCount = 1

					if ni+1 in tuplet_map and not ni in tuplet_map and not beamType == 'stop':
						item = "\\set stemRightBeamCount = #1 ".format(beamCount)  + item
					elif ni in tuplet_map and not ni-1 in tuplet_map and not beamType == 'start':
						item = "\\set stemLeftBeamCount = #{0} ".format(beamCount)  + item
					elif ni in tuplet_map and not ni+1 in tuplet_map and not beamType == 'stop':
						item = "\\set stemRightBeamCount = #{0} ".format(beamCount)  + item
					# elif ni-1 in tuplet_map and not ni in tuplet_map and not beamType == 'start':				
						# item = "\\set stemLeftBeamCount = #{0} ".format(beamCount) + item

			# grace notes
			if n.isGrace == True:
				if n.duration.slash:
					graceStr = "\slashedGrace"
				else:
					graceStr = "\grace"
				if graceOn == False:
					new_notes.append("%s {"%graceStr)
					# is grace last item in measure?
					graceOn = True
				new_notes.append(item)
				if isLast == True:
					new_notes.append("}")
			else:
				if graceOn == True:
					new_notes.append("}")
				new_notes.append(item)

				graceOn = False

			if tupletCountLimit != 0:
					if tupletCountLimit == 1:
						new_notes.append("}")
					tupletCountLimit -= 1

			# slurs --won't work at the moment as music21 doesn't read slurs proerly from chords

			# if note_map.index(i) in spanner_indices:
			# 	if spannerOn == False:
			# 		new_notes.append("(")
			# 		spannerOn = True
			# 	else:
			# 		new_notes.append(")")
			# 		spannerOn = False

			# articulations
			artic_lookup = {'Staccato':'-.','Tenuto':'--','Accent':'->','Staccatissimo':'-!','Spiccato':'-!','StrongAccent':'','Caesura':''}
			if note_map.index(i) in marks_indices:
				conv = lily.translate.LilypondConverter()
				mark_items = marks[marks_indices.index(note_map.index(i))][-1]
				for mark in mark_items:
					mtype = mark.classes[0]
					new_notes.append(artic_lookup[mtype])
		else:
			if graceOn == True and item != r"\fermata":
				new_notes.append("}")
				graceOn = False
			if item == '\\times':
				tupletOn = True

			# #print tupletCountLimit				
			new_notes.append(item)
	return " ".join(new_notes)	


effectiveTimeSignature = None

def make_ly_measure(data,path,partial=False):
	global effectiveTimeSignature
	timeSig =  data.timeSignature
	clefMark = data.clef 
	keySig = data.keySignature

	if len(data.voices) == 0:
		conv = lily.translate.LilypondConverter()		
		# hacky fix for spacing rest duration weirdness
		for i,item in enumerate(data.notesAndRests):			
				if item.isRest and item.hideObjectOnprint:
					if item.duration.quarterLength == 0.25 or item.duration.quarterLength == 0.5 or item.duration.quarterLength == 0.75:
						pass
					else:
						rounded = round(item.duration.quarterLength)
						if rounded == 0:
							data.remove(item)
						else:
							item.duration.quarterLength = rounded
		ly_string = clean_ly_string(conv.lySequentialMusicFromStream(data).stringOutput())
		ly_string = process_music_for_measure(ly_string,data)
	else:
		voices = []
		offsets = []
		min_offset = 0
		for v in data.voices:
			o = [x.offset for x in v]
			if min(o) < min_offset: min_offset = min(o)
			offsets.append(o)
		offsets = [[x+abs(min_offset) for x in o] for o in offsets]
		for i,voice_data in enumerate(data.voices):
			removeList = []
			for j,item in enumerate(voice_data.notesAndRests):
				if item.duration.isLinked:
					if item.isRest and item.hideObjectOnprint:
						if item.duration.quarterLength % 0.125 == 0:
							pass
						else:
							rounded = ceil(item.duration.quarterLength)
							if rounded == 0:
								voice_data.remove(item)
							else:
								item.duration.quarterLength = rounded
					elif item.isRest:
						if item.duration.quarterLength % 0.125 != 0:
							rounded = ceil(item.duration.quarterLength)
							item.duration.quarterLength = rounded
				else:
					voice_data.remove(item)
			if len(voice_data.notesAndRests) > 0:
				conv = lily.translate.LilypondConverter()
				ly_voice_string = clean_ly_string(conv.lySequentialMusicFromStream(voice_data).stringOutput())
				ly_string = process_music_for_measure(ly_voice_string,voice_data)
				voices.append(ly_string)
		if len(voices) > 1:
			voices = ["{{ {0} }}".format(x) for x in voices]
			ly_string = "<< {0} >>".format(" \\\\ ".join(voices))
		else:
			ly_string = voices[0]
	if partial != None:
		partial_duration = partial.split('\partial')[-1].strip()
		measure = "\t\t\partial {0} {{\n\t\t\t{1}\n\t\t}} |".format(partial_duration,ly_string)
	else:
		measure = "\t\t{0} |".format(ly_string)
	if clefMark:
		clefMark = clefMark.classes[0].split('Clef')[0].lower()
		measure = "\t\t\\clef {0}\n{1}".format(clefMark,measure)
	if timeSig:
		measure = "\t\t\\time {0}\n{1}".format(timeSig.ratioString,measure)
		effectiveTimeSignature = timeSig
	if keySig:
		keyData = list(keySig.pitchAndMode)
		if keyData[-1] == 'none':
			keyData[-1] = 'major'
		measure = "\t\t\\key {0} \\{1}\n{2}".format(str(keyData[0]).lower().replace('-','es').replace('#','is'),keyData[1].lower(),measure)
	# handle measures with "wrong" number of notes...ignore grace notes
	# #print data.barDurationProportion()
	m = stream.Measure()
	m.timeSignature = effectiveTimeSignature
	for item in data.flat:
		if item.classes[0] in ["Note","Chord","Rest"]:
			if not item.isGrace:
				m.append(item)

	if len(data.voices) > 0:
		isFull = m.barDurationProportion() / len(data.voices)
	else:
		isFull = m.barDurationProportion()


	allRests = True 
	for item in m.notesAndRests:
		if not item.isRest:
			allRests = False

	if allRests == True and isFull > 1.0 and partial == None:
		#print isFull
		lpc = lily.translate.LilypondConverter()
		dur = 'r'+str(lpc.lyMultipliedDurationFromDuration(effectiveTimeSignature.barDuration)).strip()
		measure = re.sub(r'(r)\d+',dur,measure)

	# why do I need to do this str() hack? 
	if str(isFull) != '1.0' and partial == None:
		measure = "\t\t\cadenzaOn\n {0} \n\t\t\cadenzaOff".format(measure).replace('|',"\\bar \"|\"")
	return measure


def make_ly_staff(measures):
	staff = "\\new Staff {\n"
	# subdivide beams
	# staff += "\t\t\set subdivideBeams = ##t\n"
	# staff += "\t\t\\autoBeamOff\n"

	measures_formatted = ""
	for i,m in enumerate(measures):
		measures_formatted += "\n{0} %{1}".format(m,i+1)
	staff += "\t\t{0}\n".format(measures_formatted.strip())
	staff += "\t}"
	return staff


def fix_ly_gracenote_bug(staves):
	# sidestep lilypond first grace note bug in piano music
	if len(staves) > 1:
		measures = staves[0].split("\\new Staff {")[-1].strip().rstrip("}").split("\n")
		lpc = lily.translate.LilypondConverter()
		c = 0
		for m in measures:
			m = m.strip().lstrip("<< {")
			if not m.strip().startswith('\partial') and not m.strip().startswith('\key') and not m.strip().startswith('\\time')and not m.strip().startswith('\\time') and not m.strip().startswith('\clef') and not m.strip().startswith('\set') and not m.strip().startswith('\cadenzaOn'):
				if c == 0 and m.strip().startswith('\grace'):
					graceNotes = re.search(r"\\grace { ([^}]*)}",m.strip()).group(1)
					graceNotes = filter(lambda x:re.match(r"\w+\W*\d+",x),graceNotes.split())
					totalDuration = sum([duration.Duration(4/float((re.sub(r'(\w+\W*)(\d+)',r'\2',g)))).quarterLength for g in graceNotes])
					graceDuration = " ".join(['s'+str(lpc.lyMultipliedDurationFromDuration(x)).strip() for x in duration.Duration(totalDuration).components])
					graceType = "grace"
					break
				elif c == 0 and m.strip().startswith('\slashedGrace'):
					graceNotes = re.search(r"\\slashedGrace { ([^}]*) }",m.strip()).group(1)
					graceNotes = filter(lambda x:re.match(r"\w+\W*\d+",x),graceNotes.split())
					totalDuration = sum([duration.Duration(4/float((re.sub(r'(\w+\W*)(\d+)',r'\2',g)))).quarterLength for g in graceNotes])
					graceDuration = " ".join(['s'+str(lpc.lyMultipliedDurationFromDuration(x)).strip() for x in duration.Duration(totalDuration).components])
					graceType = "slashedGrace"
					break					
				else:
					graceDuration = None
				c += 1
		if graceDuration:
			for i,staff in enumerate(staves[1:]):
				graceStr = "\%s { %s }" % (graceType,graceDuration)
				if re.search("(\clef) (\w+)(\n)\t\t<< {",staff) == None:
					staves[i+1] = re.sub("(\clef) (\w+)(\n)",r"\1 \2\3\t\t\%s\n"%graceStr,staff)
				else:
					staves[i+1] = re.sub("(\clef) (\w+)(\n\t\t<< {)",r"\1 \2\3 %s "%graceStr,staff)
	return staves

def make_ly_score(staves):
	header = "\header {tagline = \"\"}\n\\layout {\n\tindent = 0\cm\n\t\n}\n"
	font = """\paper {
	#(define fonts
	(set-global-fonts
	  #:music "sebastiano"
	  #:brace "sebastiano"
	  #:factor (/ staff-height pt 20)
	))
}"""
	font = ""
	score = "\\version \"2.19.5\"\n{0}\n{1}\n\score {{\n\t".format(header,font)
	staves = fix_ly_gracenote_bug(staves)
	if len(staves) == 1:
		score += staves[0]
	else:
		score += "\\new PianoStaff <<\n\t\t"
		for staff in staves:
			score += staff
			score += "\n\t\t"
		score += "\n\t>>"
	score += "\n}"
	return score

def write_ly(score,path):
	global outdir 
	outpath = os.path.join(outdir,path.split('/')[-1].replace('xml','ly'))
	if not os.path.exists(outdir):
		os.mkdir(outdir)
	with open(outpath,'w') as f:
		f.write(score)
	return 1

def write_empty_ly(path):
	global outdir 
	outpath = os.path.join(outdir,path.split('/')[-1].replace('xml','ly'))
	if not os.path.exists(outdir):
		os.mkdir(outdir)
	music = r"\new Staff \with {\omit TimeSignature} {s1 ^\markup {This score contains no music}}"
	score = make_ly_score([music])
	with open(outpath,'w') as f:
		f.write(score)
	return 1

def write_problem_ly(path):
	global outdir 
	outpath = os.path.join(outdir,path.split('/')[-1].replace('xml','ly'))
	if not os.path.exists(outdir):
		os.mkdir(outdir)
	music = r"\new Staff \with {\omit TimeSignature} {s1 ^\markup {This incipit is currently unavailable}}"
	score = make_ly_score([music])
	with open(outpath,'w') as f:
		f.write(score)
	return 1

def is_all_rests(measure):
	for item in measure.flat.notesAndRests:
		if not item.isRest:
			return False
	return True

def get_note_incipit(part,endPoint,partial):
	for i in part.flat.notes:
		startPoint = i.measureNumber
		if startPoint != 0:
			startPoint -= 1
		break 
	rests = 0
	notes = 0
	for i in part.getElementsByClass('Measure')[startPoint].flat.notesAndRests:
		if i.isNote:
			notes += i.duration.quarterLength
		elif i.isRest:
			rests += i.duration.quarterLength

	if rests >= notes:
		# startPoint += 1
		if endPoint == 2:
			endPoint += 1

	# #print startPoint,endPoint,rests,notes

	data = part.getElementsByClass('Measure')[startPoint:startPoint+endPoint]

	if partial==None:
		return data
	else:
		conv = lily.translate.LilypondConverter()
		d = data.getElementsByClass('Measure')[0].flat.notesAndRests.duration
		d = len(duration.partitionQuarterLength(d.quarterLength,0.125))
		partial = "\partial 32*%s" % d
		return data,partial

def process(path):	
	# if not os.path.exists(path.replace('.xml','.ly').replace('XML','LY')):
		#print path
		try:
			doc = musicxml.xmlHandler.Document()
			doc.open(path)
			data = musicxml.fromMxObjects.mxScoreToScore(doc.score)
			if len(data.parts.flat.notes) == 0:
				write_empty_ly(path)
				return None
		except:
			#print 'Cannot Read Music XML for %s' % path
			write_problem_ly(path)
			return None
		conv = lily.translate.LilypondConverter()
		try:
			ly_string = conv.lySequentialMusicFromStream(data).stringOutput()
		except:
			ly_string = ""
		if len(data.parts) <= 2:
			parts = sorted(data.parts,key=lambda x:float(x.flat.notes[0].offset))
		else:
			parts = data.parts
		remove_dodgy_spacer_rests(parts)
		measures = parts[0].getElementsByClass('Measure')
		if len(parts) > 1 and parts[0].measure(1).duration != parts[1].measure(1).duration:
			partial = True
			timeSig = parts[0].measure(1).timeSignature
		else:
			for item in measures:
				try:
					timeSig = item.timeSignature
					break
				except:
					pass
			try:
				if measures[0].duration != timeSig.barDuration:
					d = measures[0].duration
					#print d 
					partial = "\partial %s" % conv.lyMultipliedDurationFromDuration(d)
				else:
					partial = None
			except:
				if measures[0].barDuration == measures[0].timeSignature.barDuration:
					partial = None
				else:
					partial_re = re.compile(r"{ \\partial \d+\*\d+")
					partial_re = partial_re.search(ly_string)
					p
					if partial_re:
						partial = partial_re.group(0)
					else:
						partial = None
		newparts = []
		# for i in range(len(parts)):
		# only use top staff
		for i in range(1):
			part_incipit = []
			if partial != None:
				endPoint = 3
			else:
				endPoint = 2
			# will search for the position of first notes... rather than starting with rests
			if partial != None:
				part_incipit,partial = get_note_incipit(parts[i],endPoint,partial)
			else:
				part_incipit = get_note_incipit(parts[i],endPoint,partial)
			# if partial != None:
			# 	part_incipit[1].timeSignature = timeSig 
			# else:
			part_incipit[0].timeSignature = timeSig 
			newparts.append(part_incipit)
		staves = []
		for part_index,p in enumerate(newparts):
			ly_measures = []
			# don't waste space with completely empty staves
			# only use top staff for incipits
			if not is_all_rests(p) == True:
				for i,measure in enumerate(p):
					if partial != None and i == 0:
						hasPartial = True
						ly_measure = make_ly_measure(measure,path,partial);
					else:
						hasPartial = False
						ly_measure = make_ly_measure(measure,path,None);					
					if ly_measure != None:
						ly_measures.append(ly_measure)
				ly_staff = make_ly_staff(ly_measures)
				staves.append(ly_staff)
		score = make_ly_score(staves)
		#(score)
		write_ly(score,path)
		return None

def remove_dodgy_spacer_rests(partlist):
	for part in partlist:
		timeSig =  part.flat.getElementsByClass('TimeSignature')[0]
		for m in part.getElementsByClass('Measure'):
			n = stream.Stream()
			r = stream.Stream()
			for note in m.flat.notesAndRests:
				if note.isNote:
					n.append(note)		
				else:
					r.append(note)
			if n.notes.duration == timeSig.barDuration:
				if len(r) > 0:
					if r[0].offset == 0 and len(r) == 1:
						m.remove(r[0],shiftOffsets=True)
			elif r.duration == timeSig.barDuration and n.notes.duration > 0:
				if r[0].offset == 0 and len(r) == 1:
					m.remove(r[0],shiftOffsets=True)

def make_incipit_ly(source):
	global path,outdir 
	path = 'port_collection_assets/{0}'.format(source)
	outdir = path + '/{0}_ly'.format(source)
	data = glob.glob(path+'/{0}_xml/*.xml'.format(source))
	data.sort(key=lambda x: int(re.split('(\d+).',x)[-2]))
	for p in data:
		process(p)
	return None

if __name__ == '__main__':
	make_incipit_ly('bunting_vol_2')	