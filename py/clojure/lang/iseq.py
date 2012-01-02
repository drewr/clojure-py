from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.ipersistentcollection import IPersistentCollection

class ISeq(IPersistentCollection):
    def first(self):
        raise AbstractMethodCall(self)
    def next(self):
        raise AbstractMethodCall(self)
    def more(self):
        raise AbstractMethodCall(self)
    def cons(self, o):
        raise AbstractMethodCall(self)
