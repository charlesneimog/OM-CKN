#C:/Users/charl/Documents/OM#/temp-files/Python/Scripts/python.exe
import music21 

#xml_file = m21.converter.parse("C:/Users/charl/OneDrive_usp.br/Documents/OpenMusic/in-files/Turvo-Turvo.xml")
xml_file = music21.converter.parse("C:/Users/charl/OneDrive_usp.br/Documents/OpenMusic/in-files/test.xml")
    
quantas_partes = len(xml_file.parts)


for part_index in range(quantas_partes):
    measures = xml_file.parts[part_index].getElementsByClass(music21.stream.Measure)
    quantos_compassos = len(measures)
    for measure_index in range(quantos_compassos):
        notes = measures[measure_index].getElementsByClass(music21.note.Note)
        quantas_notas = len(notes)
        for note_index in range(quantas_notas):    
            note_with_index = notes[note_index]
            ratio_of_note = note_with_index.duration.aggregateTupletMultiplier() 
            pitch = '{}{}'.format(note_with_index.name, note_with_index.octave)
            print(ratio_of_note)
            dynamics = note_with_index

# TODO 
# 1. Conseguir informação de qual nota caberia normalmente (por exemplo, é a nota 32 ou 16, etc)
# 2. Conseguir informação sobre Dinamicas.
# 3. Conseguir informação sobre texto. (CENTS)

