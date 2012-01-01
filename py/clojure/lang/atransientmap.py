from py.clojure.lang.ifn import IFn
from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.itransientmap import ITransientMap
import py.clojure.lang.rt as RT

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

    def __call__(self, *args):
        return apply(self.valAt, args)

    def without(self, key):
        self.ensureEditable()
        return self.doWithout()

    def valAt(self, key, notFound = None):
        self.ensureEditable()
        return self.doValAt(key, notFound)
    def assoc(self, key, value):
        self.ensureEditable()
        return self.doAssoc(key, value)
    def count(self):
        self.ensureEditable()
        return self.count()
    def persistent(self):
        self.ensureEditable()
        return self.persistent()