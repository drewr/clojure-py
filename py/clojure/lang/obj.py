from iobj import IObj
from cljexceptions import AbstractMethodCall



class Obj(object, IObj):
    def meta(self):
        return self._meta
    def withMeta(self, meta):
        raise AbstractMethodCall()
