from py.clojure.lang.iobj import IObj
from py.clojure.lang.cljexceptions import AbstractMethodCall



class Obj(object, IObj):
    def meta(self):
        return self._meta
    def withMeta(self, meta):
        raise AbstractMethodCall(self)
