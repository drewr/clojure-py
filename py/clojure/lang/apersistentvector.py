from py.clojure.lang.ipersistentvector import IPersistentVector
from py.clojure.lang.cljexceptions import AbstractMethodCall, ArityException
from py.clojure.lang.indexableseq import IndexableSeq


class APersistentVector(object, IPersistentVector):
    def __iter__(self):
        for x in range(len(self)):
            yield self.nth(x)

    def peek(self):
        if len(self):
            return self.nth(len(self) - 1)
        return None

    def __getitem__(self, item):
        return self.nth(item)

    def seq(self):
        if not len(self):
            return None
        return IndexableSeq(self, 0)

class SubVec(APersistentVector):
    def __init__(self, meta, v, start, end):
        self._meta = meta
        if isinstance(v, SubVec):
            start += v.start
            end += sv.start
            v = sv.v
        self.v = v
        self.start = start
        self.end = end
    def nth(self, i):
        if self.start + i >= end:
            raise Exception("Index out of range")
        return v.nth(self.start + i)
    def assocN(self, i, val):
        if start + i > end:
            raise Exception("Index out of range")
        elif start + i == end:
            return self.cons(val)
        return SubVec(self._meta,
                                        self.v.assocN(self.start + self.i, val),
                                        start,
                                        end)

    def __len__(self):
        return self.end - self.start

    def cons(self, o):
        return SubVec(self._meta,
                                        self.v.assocN(self.end, o),
                                        self.start,
                                        self.end + 1)
    def empty(self):
        from py.clojure.lang.persistentvector import EMPTY as EMPTY_VECTOR
        return EMPTY_VECTOR.wiothMeta(self.meta())

    def pop(self):
        from py.clojure.lang.persistentvector import EMPTY as EMPTY_VECTOR
        if self.end - 1 == self.start:
            return EMPTY_VECTOR
        return SubVec(self._meta, self.v, self.start, self.end -1)

    def withMeta(self, meta):
        if self.meta == meta:
            return self
        return SubVec(self._meta, self.v, self.start, self.end)

    def meta(self):
        return self.meta()