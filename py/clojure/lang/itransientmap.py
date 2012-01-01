from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.itransientassociative import ITransientAssociative
from py.clojure.lang.counted import Counted

class ITransientMap(ITransientAssociative, Counted):
    def assoc(self, key, value):
        raise AbstractMethodCall()
    def without(self, key):
        raise AbstractMethodCall()
    def persistent(self):
        raise AbstractMethodCall()

