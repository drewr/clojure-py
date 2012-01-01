from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.associative import Associative
from py.clojure.lang.interable import Interable
from py.clojure.lang.counted import Counted

class IPersistentMap(Interable, Associative, Counted):
    def without(self, key):
        raise AbstractMethodCall()
