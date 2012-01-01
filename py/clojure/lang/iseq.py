from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.ipersistentcollection import IPersistentCollection

class ISeq(IPersistentCollection):
    def first(self):
        raise AbstractMethodCall()
    def next(self):
        raise AbstractMethodCall()
    def more(self):
        raise AbstractMethodCall()
    def cons(self, o):
        raise AbstractMethodCall()
