from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.seqable import Seqable

class IPersistentCollection(Seqable):
    def count(self):
        raise AbstractMethodCall(self)

    def cons(self, o):
        raise AbstractMethodCall(self)

    def empty(self):
        raise AbstractMethodCall(self)

