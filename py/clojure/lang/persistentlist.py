from ipersistentlist import IPersistentList
from aseq import ASeq
from ireduce import IReduce
from counted import Counted
from seqable import Seqable
from iseq import ISeq
from sequential import Sequential
from obj import Obj
from cljexceptions import ArityException, IllegalStateException
import rt as RT

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

    @staticmethod
    def create(lst):
        ret = EMPTY
        for x in range(len(lst) - 1, -1, -1):
            c = lst[x]
            ret = ret.cons(c)
        return ret

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

    def count(self):
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

class EmptyList(Obj, IPersistentList, ISeq, Counted):
    def __init__(self, meta = None):
        self._meta = meta

    def __hash__(self):
        return 1
    def __eq__(self, other):
        return (isinstance(other, Sequential)
           or isinstance(other, list)
           or isinstance(other, tuple)) \
           and RT.seq(other) is None
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

EMPTY = EmptyList()

