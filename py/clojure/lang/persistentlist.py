from py.clojure.lang.ipersistentlist import IPersistentList
from py.clojure.lang.aseq import ASeq
from py.clojure.lang.ireduce import IReduce
from py.clojure.lang.counted import Counted
from py.clojure.lang.seqable import Seqable
from py.clojure.lang.iseq import ISeq
from py.clojure.lang.sequential import Sequential
from py.clojure.lang.obj import Obj
from py.clojure.lang.cljexceptions import ArityException, IllegalStateException
import py.clojure.lang.rt as RT

class PersistentList(ASeq, IPersistentList, IReduce, Counted):
    def __init__(self, *args):
        if len(args) == 1:
            self._first = args[0]
            self._rest = None
            self._count = 1
        elif len(args) == 4:
            self._meta = args[0]
            self._first = args[1]
            self._rest = args[2]
            self._count = args[3]
        else:
            raise ArityException
        self._hash = -1

    def next(self):
        if self._count == 1:
            return None
        return self._rest

    def first(self):
        return self._first

    def peek(self):
        return self.first()

    def pop(self):
        if self._rest is None:
            return EMPTY.withMeta(self._meta)
        return self._rest

    def __len__(self):
        return self._count

    def cons(self, o):
        return PersistentList(self.meta(), o, self, self._count + 1)

    def empty(self):
        return EMPTY.withMeta(self.meta())

    def reduce(self, *args):
        if len(args) == 1:
            ret = self.first()
        elif len(args) == 2:
            ret = args[0](args[1], self.first())
        else:
            raise ArityException()
        fn = args[0]
        s = self.next()
        while s is not None:
            ret = fn(ret, s.first())
        return ret

    def withMeta(self, meta):
        if meta is self.meta():
            return self
        return PersistentList(meta, self._first, self._rest, self._count)

    def __repr__(self):
        s = []
        for x in self:
            s.append(repr(x))
        return "(" + " ".join(s) + ")"

def create(lst):
    ret = EMPTY
    for x in range(len(lst) - 1, -1, -1):
        c = lst[x]
        ret = ret.cons(c)
    return ret

def creator(*args):
    ret = EMPTY
    for x in range(len(args) - 1, -1, -1):
        ret = ret.cons(args[x])
    return ret


class EmptyList(Obj, IPersistentList, ISeq, Counted):
    def __init__(self, meta = None):
        self._meta = meta

    def __hash__(self):
        return 1

    def __eq__(self, other):
        return isinstance(other, (Sequential, list, tuple)) and RT.seq(other) is None

    def __iter__(self):
        return

    def withMeta(self, meta):
        if self._meta == meta:
            return self
        return EmptyList(meta)

    def first(self):
        return None

    def next(self):
        return None

    def more(self):
        return self

    def cons(self, o):
        return PersistentList(self.meta(), o, None, 1)

    def empty(self):
        return self

    def peek(self):
        return None

    def pop(self):
        raise IllegalStateException("Can't pop an empty list")

    def count(self):
        return 0

    def seq(self):
        return None

    def __repr__(self):
        return "()"

    def __len__(self):
        return 0


EMPTY = EmptyList()
