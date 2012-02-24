from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.imeta import IMeta

class IReference(IMeta):
    def alterMeta(self, fn, args):
        raise AbstractMethodCall(self)

    def resetMeta(self, meta):
        raise AbstractMethodCall(self);
