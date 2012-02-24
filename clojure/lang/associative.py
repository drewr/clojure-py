from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.ilookup import ILookup
from clojure.lang.ipersistentcollection import IPersistentCollection


class Associative(ILookup, IPersistentCollection):
    def containsKey(self, key):
        raise AbstractMethodCall(self)

    def entryAt(self, key):
        raise AbstractMethodCall(self)

    def assoc(self, key, val):
        raise AbstractMethodCall(self)
