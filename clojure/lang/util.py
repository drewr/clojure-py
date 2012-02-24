from clojure.lang.cljexceptions import (AbstractMethodCall,
                                           InvalidArgumentException)
from clojure.lang.mapentry import MapEntry
import clojure.lang.rt as RT


def hashCombine(hash, seed):#FIXME - unused argument?
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


def bitCount(i):
    i -= ((i >> 1) & 0x55555555)
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333)
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24


def arrayCopy(src, srcPos, dest, destPos, length):
    dest[destPos:length] = src[srcPos:length]
