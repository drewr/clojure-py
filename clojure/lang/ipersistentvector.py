from clojure.lang.associative import Associative
from clojure.lang.sequential import Sequential
from clojure.lang.ipersistentstack import IPersistentStack
from clojure.lang.reversible import Reversible
from clojure.lang.indexed import Indexed
from clojure.lang.cljexceptions import AbstractMethodCall


class IPersistentVector(Associative, Sequential, IPersistentStack, Reversible, Indexed):
    def __len__(self):
        raise AbstractMethodCall(self)

    def assocN(self, i, val):
        raise AbstractMethodCall(self)

    def cons(self, o):
        raise AbstractMethodCall(self)
