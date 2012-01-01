from py.clojure.lang.cljexceptions import AbstractMethodCall

class Settable(object):
    def doSet(self, o):
        raise AbstractMethodCall()
    def doReset(self, o):
        raise AbstractMethodCall()