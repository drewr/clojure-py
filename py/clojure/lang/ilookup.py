from py.clojure.lang.cljexceptions import AbstractMethodCall

class ILookup():
    def valAt(self, key, notFound = None):
        raise AbstractMethodCall(self)


