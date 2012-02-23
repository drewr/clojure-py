from py.clojure.lang.obj import Obj
from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.iseq import ISeq
from py.clojure.lang.sequential import Sequential
from py.clojure.lang.counted import Counted
from py.clojure.lang.ihasheq import IHashEq
from py.clojure.lang.iterable import Iterable
import py.clojure.lang.rt as RT


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
            from py.clojure.lang.persistentlist import EMPTY
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
