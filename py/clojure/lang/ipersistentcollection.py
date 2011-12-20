from exceptions import AbstractMethodCall
from seqable import Seqable


class IPersistentCollection(Seqable):
    def count(self):
        raise AbstractMethodCall()
    def cons(self, o):
        raise AbstractMethodCall()
    def empty(self):
        raise AbstractMethodCall()
    def __eq__(self, other):
        raise AbstractMethodCall()

