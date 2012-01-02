from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.itransientcollection import ITransientCollection
from py.clojure.lang.ilookup import ILookup

class ITransientAssociative(ITransientCollection, ILookup):
    def assoc(self, key, val):
        raise AbstractMethodCall(self)
