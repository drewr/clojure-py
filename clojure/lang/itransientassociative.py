from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.itransientcollection import ITransientCollection
from clojure.lang.ilookup import ILookup

class ITransientAssociative(ITransientCollection, ILookup):
    def assoc(self, key, val):
        raise AbstractMethodCall(self)
