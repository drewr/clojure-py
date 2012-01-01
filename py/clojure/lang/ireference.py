from py.clojure.lang.exceptions import AbstractMethodCall
from py.clojure.lang.imeta import IMeta

class IReference(object, IMeta):
    def alterMeta(self, fn, args):
        raise AbstractMethodCall()
    def resetMeta(self, meta):
        raise AbstractMethodCall();