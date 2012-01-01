from py.clojure.lang.cljexceptions import AbstractMethodCall

class Named():
    def getNamespace(self):
        raise AbstractMethodCall()
    def getName(self):
        raise AbstractMethodCall()