from py.clojure.lang.cljexceptions import AbstractMethodCall

class IFn():
    def __call__(self, *args):
        raise AbstractMethodCall(self)