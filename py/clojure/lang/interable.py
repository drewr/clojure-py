from py.clojure.lang.cljexceptions import AbstractMethodCall

class Interable():
    def __iter__(self):
        raise AbstractMethodCall(self)

