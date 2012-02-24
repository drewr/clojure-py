from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.associative import Associative
from clojure.lang.iterable import Iterable
from clojure.lang.counted import Counted

class IPersistentMap(Iterable, Associative, Counted):
    def without(self, key):
        raise AbstractMethodCall(self)
