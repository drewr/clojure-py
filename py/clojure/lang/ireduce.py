from py.clojure.lang.cljexceptions import AbstractMethodCall

class IReduce:
    def reduce(self, *args):
        raise AbstractMethodCall(self)
