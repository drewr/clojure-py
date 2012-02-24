from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.ideref import IDeref

class IRef(IDeref):
    def setValidator(self, fn):
        raise AbstractMethodCall(self)

    def getValidator(self):
        raise AbstractMethodCall(self)

    def getWatches(self):
        raise AbstractMethodCall(self)

    def addWatch(self, key, fn):
        raise AbstractMethodCall(self)

    def removeWatch(self, key):
        raise AbstractMethodCall(self)
