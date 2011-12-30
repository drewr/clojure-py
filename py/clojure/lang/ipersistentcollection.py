from cljexceptions import AbstractMethodCall
from seqable import Seqable


class IPersistentCollection(Seqable):
    def count(self):
        raise AbstractMethodCall(self)
    def cons(self, o):
        raise AbstractMethodCall(self)
    def empty(self):
        raise AbstractMethodCall(self)
    def __eq__(self, other):
        raise AbstractMethodCall(self)

