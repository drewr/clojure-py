from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.ipersistentcollection import IPersistentCollection

class IPersistentStack(IPersistentCollection):
    def peek(self):
        raise AbstractMethodCall()
    def pop(self):
        raise AbstractMethodCall()
