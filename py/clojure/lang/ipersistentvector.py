from py.clojure.lang.associative import Associative
from py.clojure.lang.sequential import Sequential
from py.clojure.lang.ipersistentstack import IPersistentStack
from py.clojure.lang.reversible import Reversible
from py.clojure.lang.indexed import Indexed
from py.clojure.lang.cljexceptions import AbstractMethodCall


class IPersistentVector(Associative, Sequential, IPersistentStack, Reversible, Indexed):
    def __len__(self):
        raise AbstractMethodCall(self)

    def assocN(self, i, val):
        raise AbstractMethodCall(self)

    def cons(self, o):
        raise AbstractMethodCall(self)
