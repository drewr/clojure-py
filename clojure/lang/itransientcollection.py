from clojure.lang.cljexceptions import AbstractMethodCall

class ITransientCollection(object):
    def conj(self, val):
        raise AbstractMethodCall(self)

    def persistent(self):
        raise AbstractMethodCall(self)
