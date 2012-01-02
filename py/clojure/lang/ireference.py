from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.imeta import IMeta

class IReference(IMeta):
    def alterMeta(self, fn, args):
        raise AbstractMethodCall()
    def resetMeta(self, meta):
        raise AbstractMethodCall();