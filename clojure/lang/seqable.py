from clojure.lang.cljexceptions import AbstractMethodCall


class Seqable:
    def seq(self):
        raise AbstractMethodCall(self)
