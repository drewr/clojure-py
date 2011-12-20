from iobj import IObj
from exceptions import AbstractMethodCall



class Obj(object, IObj):
    def meta(self):
        return self.meta
    def withMeta(self, meta):
        raise AbstractMethodCall()
