from py.clojure.lang.aseq import ASeq
from py.clojure.lang.cljexceptions import ArityException
from py.clojure.lang.persistentlist import EMPTY
import py.clojure.lang.rt as RT

class Cons(ASeq):
    def __init__(self, *args):
        if len(args) == 2:
            self._meta = None
            self._first = args[0]
            self._more = args[1]
        elif len(args) == 3:
            self._meta = args[0]
            self._first = args[1]
            self._more = args[2]
        else:
            raise ArityException()

    def first(self):
        return self._first

    def next(self):
        return self.more().seq()

    def more(self):
        if self._more is None:
            return EMPTY
        return self._more

    def count(self):
        return 1 + RT.count(self._more)

    def withMeta(self, meta):
        return Cons(meta, self._first, self._more)

    def __len__(self):
        s = self.next()
        c = 1
        while s is not None:
            if hasattr(s, "__len__"):
                return c + len(s)
            c += 1
            s = s.next()
        return c

    def __repr__(self):
        s = self
        strs = []
        while s is not None:
            strs.append(repr(s.first()))
            s = s.next()
        return "(" + " ".join(strs) + ")"
