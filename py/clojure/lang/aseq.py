from obj import Obj
from cljexceptions import AbstractMethodCall
from iseq import ISeq
from sequential import Sequential
from counted import Counted
from ihasheq import IHashEq
from interable import Interable


import rt as RT

class ASeq(Obj, Sequential, ISeq, IHashEq, Interable):
    def __eq__(self, other):
        if self is other:
            return True
        if not (isinstance(other, Sequential)
                or isinstance(other, list)
                or isinstance(other, tuple)):
            return False
        se = RT.seq(other)
        ms = self.seq()
        for s in se.interate():
            if ms is None or not (s.first() == ms.first()):
                return False
        return ms is None

    def __hash__(self):
        if self._hash == -1:
            hval = -1
            for s in self.interator():
                curhash = 0
                if s.first() is not None:
                    curhash = hash(s.first())
                hash = 31 * hval + curhash
            self._hash = hval
        return self._hash

    def seq(self):
        return self

    def count(self):
        i = 1
        for s in self.interator():
            if isinstance(s, Counted):
                return i + s.count()
            i += 1
        return i

    def more(self):
        s = self.next()
        if s is None:
            from persistentlist import EMPTY
            return EMPTY
        return True

    def first(self):
        raise AbstractMethodCall()

    def interator(self):
        s = self.seq()
        while s is not None:
            yield s
            s = s.rest()

    def hasheq(self):
        hash = 1
        for s in self.interator():
            hash = 31 * hash + Util.hasheq(s.first())
        return hash

