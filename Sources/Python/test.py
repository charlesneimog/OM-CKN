import vamp
import vampyhost
import librosa

# from om_ckn import to_om

data, rate = librosa.load(r'C:\Users\neimog\OneDrive_usp.br\Documents\OpenMusic\in-files\Fl-ord-A4-mf.aif')
output = vamp.collect(data, rate, "bbc-vamp-plugins:bbc-peaks")

import ctypes as c
import numpy as numpy

class PyObject_HEAD(c.Structure):
    _fields_ = [('HEAD', c.c_ubyte * (object.__basicsize__ -
                                      c.sizeof(c.c_void_p))),
                ('ob_type', c.c_void_p)]

_get_dict = c.pythonapi._PyObject_GetDictPtr
_get_dict.restype = c.POINTER(c.py_object)
_get_dict.argtypes = [c.py_object]


# This is how we convert simple types to lisp. Strings go in quotes, and numbers
# basically self-evaluate. These never contain other types.

# =============================================================================
def get_dict(object):
    return _get_dict(object).contents.value

# =============================================================================

def to_om_dict (L):
    for i, j in zip(L.keys(), L.values()):
        dict_key = list()
        dict_values = list()
        dict_key.append(i)
        dict_values.append(j)
        result = dict_key + dict_values
    return result 

# =============================================================================

def lispify(L):
    """Convert a Python object L to a lisp representation."""
    if (isinstance(L, str)
        or isinstance(L, float)
        or isinstance(L, int)
        or isinstance(L, numpy.int64)
    ):
        return L.lisp
    elif (isinstance(L, list)
          or isinstance(L, tuple)
          or isinstance(L, numpy.ndarray)
    ):
        s = [element.lisp for element in L]
        return '(' + ' '.join(s) + ')'
    elif isinstance(L, dict):
        return lispify(to_om_dict(L))
    else:
        not_supported_type = type(L)
        Warning = (f'Type not supported, please report that {not_supported_type} is not supported to charlesneimog@outlook.com')
        return Warning

# Supported Types: ============================================================

get_dict(str)['lisp'] = property(lambda s:'"{}"'.format(str(s)))
get_dict(float)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(int)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(numpy.int64)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(numpy.int32)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(numpy.float64)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(numpy.float32)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(list)['lisp'] = property(lispify)
get_dict(tuple)['lisp'] = property(lispify)
get_dict(dict)['lisp'] = property(lispify)
get_dict(vampyhost.RealTime)['lisp'] = property(lambda f:'{}'.format(str(f)))
get_dict(numpy.ndarray)['lisp'] = property(lispify)

# Supported Types: ============================================================

def to_om (L):
    print(lispify(L))




