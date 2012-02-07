from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.ilookup import ILookup
from py.clojure.lang.ipersistentcollection import IPersistentCollection

class Associative(ILookup, IPersistentCollection):
    def containsKey(self, key):
        raise AbstractMethodCall(self)

    def entryAt(self, key):
        raise AbstractMethodCall(self)

    def assoc(self, key, val):
        raise AbstractMethodCall(self)
