from clojure.lang.areference import AReference
from clojure.lang.iref import IRef
import clojure.lang.rt as RT
from clojure.lang.cljexceptions import IllegalStateException, ArityException
from clojure.lang.threadutil import synchronized
from clojure.lang.persistenthashmap import EMPTY


class ARef(AReference, IRef):
    def __init__(self, meta=None):
        AReference.__init__(self, meta)
        self.validator = None
        self.watches = EMPTY

    def validate(self, *args):
        if len(args) == 1:
            val = args[0]
            vf = self.validator
        elif len(args) == 2:
            vf = args[0]
            val = args[1]
        else:
            raise ArityException()

        if vf is not None \
           and not RT.booleanCast(vf(val)):
            raise IllegalStateException("Invalid reference state")

    def setValidator(self, fn):
        self.validate(fn, self.deref())
        self.validator = fn

    def getValidator(self):
        if not hasattr(self, "validator"):
            return None
        return self.validator

    def getWatches(self):
        return self.watches

    @synchronized
    def addWatch(self, key, fn):
        self.watches = self.watches.assoc(key, fn)
        return self

    @synchronized
    def removeWatch(self, key):
        self.watches = self.watches.without(key)
        return self

    def notifyWatches(self, oldval, newval):
        ws = self.watches
        if len(ws) > 0:
            for s in ws.seq().interate():
                e = s.first()
                fn = e.getValue()
                if fn is not None:
                    fn(e.getKey(), self, oldval, newval)
