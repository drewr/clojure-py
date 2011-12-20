from exceptions import AbstractMethodCall
from ideref import IDeref

class IRef(IDeref):
    def setValidator(self, fn):
        raise AbstractMethodCall()
    def getValidator(self):
        raise AbstractMethodCall()
    def getWatches(self):
        raise AbstractMethodCall()
    def addWatch(self, key, fn):
        raise AbstractMethodCall()
    def removeWatch(self, key):
        raise AbstractMethodCall()