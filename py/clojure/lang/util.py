from exceptions import AbstractMethodCall, InvalidArgumentException
from mapentry import MapEntry
import rt as RT

def hashCombine(hash, seed):
    seed ^= seed + 0x9e3779b9 + (seed << 6) + (seed >> 2)
    return seed


def hasheq(o):
    raise AbstractMethodCall()

def conjToAssoc(self, o):
    if isinstance(o, MapEntry):
        return self.assoc(o.getKey(), o.getValue())
    if hasattr(o, "__getitem__") and hasattr(o, "__len__"):
        if len(o) != 2:
            raise InvalidArgumentException("Vector arg must be a pair")
        return self.assoc(o[0], o[1])

    s = RT.seq(o)
    map = self
    for s in s.interator():
        m = s.first()
        map = map.assoc(m.getKey(), m.getValue())
    return map

def bitCount(int_type):
    count = 0
    while(int_type):
        int_type &= int_type - 1
        count += 1
    return(count)

def arrayCopy(src, srcPos, dest, destPos, length):
    src[srcPos:length] = dest[destPos:length]
