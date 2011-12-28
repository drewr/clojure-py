from cljexceptions import AbstractMethodCall
from ipersistentcollection import IPersistentCollection
from counted import Counted
class IPersistentSet(IPersistentCollection, Counted):
    def disjoin(self, key):
        raise AbstractMethodCall()
    def __contains__(self, item):
        raise AbstractMethodCall()
    def __getitem__(self, item):
        return self.get(item)