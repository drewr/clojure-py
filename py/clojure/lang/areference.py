from ireference import IReference
from cons import Cons
import rt as RT

class AReference(object, IReference):
    def __init__(self, meta = None):
        self._meta = meta
    def meta(self):
        return self._meta
    def alterMeta(self, fn, args):
        RT.applyTo(fn, Cons(self._meta, args))
    def resetMeta(self, meta):
        self._meta = meta