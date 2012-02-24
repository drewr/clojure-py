from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.itransientassociative import ITransientAssociative
from clojure.lang.counted import Counted

class ITransientMap(ITransientAssociative, Counted):
    def assoc(self, key, value):
        raise AbstractMethodCall(self)

    def without(self, key):
        raise AbstractMethodCall(self)

    def persistent(self):
        raise AbstractMethodCall(self)
