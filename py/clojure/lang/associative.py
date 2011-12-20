from exceptions import AbstractMethodCall
from ilookup import ILookup
from ipersistentcollection import IPersistentCollection

class Associative(ILookup, IPersistentCollection):
    def containsKey(self, key):
        raise AbstractMethodCall()
    def entryAt(self, key):
        raise AbstractMethodCall()
    def assoc(self, key, val):
        raise AbstractMethodCall()

