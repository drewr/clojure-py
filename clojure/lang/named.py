from clojure.lang.cljexceptions import AbstractMethodCall


class Named(object):
    def getNamespace(self):
        raise AbstractMethodCall(self)

    def getName(self):
        raise AbstractMethodCall(self)
