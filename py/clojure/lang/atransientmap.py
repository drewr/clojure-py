from ifn import IFn
from exceptions import AbstractMethodCall
from itransientmap import ITransientMap
import rt as RT

class ATransientMap(IFn, ITransientMap):
    def ensureEditable(self):
        raise AbstractMethodCall()
    def doAssoc(self, key, val):
        raise AbstractMethodCall()
    def doWithout(self, key):
        raise AbstractMethodCall()
    def doValAt(self, key, notFound = None):
        raise AbstractMethodCall()
    def doCount(self):
        raise AbstractMethodCall()
    def doPersistent(self):
        raise AbstractMethodCall()

    def conj(self, val):
        self.ensureEditable()
        return RT.conjToAssoc(self, val)