import math

def f2mc(freq):
    ref_pitch = 440 
    diferenca_com_A4 =  6900 + (math.log(abs(freq / ref_pitch)) / math.log(2)) * 1200
    return round(diferenca_com_A4, 2)











