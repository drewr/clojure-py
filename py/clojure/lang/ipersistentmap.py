from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.associative import Associative
from py.clojure.lang.iterable import Iterable
from py.clojure.lang.counted import Counted

class IPersistentMap(Iterable, Associative, Counted):
    def without(self, key):
        raise AbstractMethodCall(self)
