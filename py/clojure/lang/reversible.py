from py.clojure.lang.cljexceptions import AbstractMethodCall


class Reversible():
    def rseq(self):
        raise AbstractMethodCall(self)
