from clojure.lang.cljexceptions import AbstractMethodCall

class IDeref(object):
    def deref(self):
        raise AbstractMethodCall(self)
