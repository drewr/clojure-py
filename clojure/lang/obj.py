from clojure.lang.iobj import IObj
from clojure.lang.cljexceptions import AbstractMethodCall


class Obj(object, IObj):
    def meta(self):
        if not hasattr(self, "_meta"):
            return None
        return self._meta

    def withMeta(self, meta):
        raise AbstractMethodCall(self)
