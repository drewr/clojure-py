from py.clojure.lang.cljexceptions import AbstractMethodCall

class Iterable():
    def __iter__(self):
        raise AbstractMethodCall(self)
