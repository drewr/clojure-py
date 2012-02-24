from clojure.lang.obj import Obj
from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.iseq import ISeq
from clojure.lang.sequential import Sequential
from clojure.lang.counted import Counted
from clojure.lang.ihasheq import IHashEq
from clojure.lang.iterable import Iterable
import clojure.lang.rt as RT


class ASeq(Obj, Sequential, ISeq, IHashEq, Iterable):
    def __eq__(self, other):
        if self is other:
            return True

        se = RT.seq(other)
        if isinstance(se, RT.NotSeq):
            return False
        ms = self.seq()
        while se is not None:
            if ms is None or not se.first() == ms.first():
                return False
            ms = ms.next()
            se = se.next()
        return ms is None

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
            from clojure.lang.persistentlist import EMPTY
            return EMPTY
        return s

    def first(self):
        raise AbstractMethodCall(self)

    def __iter__(self):
        s = self.seq()
        while s is not None:
            yield s.first()
            s = s.next()

    def hasheq(self):
        hash = 1
        for s in self:
            hash = 31 * hash + Util.hasheq(s.first())#FIXME: Util is... where?
        return hash
