from clojure.lang.cljexceptions import AbstractMethodCall


class Settable(object):
    def doSet(self, o):
        raise AbstractMethodCall(self)

    def doReset(self, o):
        raise AbstractMethodCall(self)
