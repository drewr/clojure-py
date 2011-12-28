from symbol import Symbol
from persistenthashmap import EMPTY as EMPTY_MAP
from atomicreference import AtomicReference
from cljexceptions import InvalidArgumentException, ArityException
import weakref
from ifn import IFn
from named import Named

interned = AtomicReference(EMPTY_MAP)

class Keyword(IFn, Named):
    @staticmethod
    def intern(*args):
        if len(args) == 1:
            if isinstance(args[0], Symbol):
                sym = args[0]
                if sym.meta() is not None:
                    sym = sym.withMeta(None)
                k = Keyword(sym)

                interned.mutate(lambda old: old if sym in old else old.assoc(sym, weakref.ref(k)))

                return interned.get()[sym]()
            elif isinstance(args[0], str):
                return Keyword.intern(Symbol.intern(args[0]))
            else:
                raise InvalidArgumentException()
        elif len(args) == 2:
            return Keyword.intern(Symbol.intern(*args))
        else:
            raise ArityException()
    def __init__(self, sym):
        self.sym = sym
        self.hash = hash(sym) + 0x9e3779b9
    def __hash__(self):
        return self.hash

    def __call__(self, obj, notFound = None):
        if self not in obj:
            return notFound
        return obj[self]

    @staticmethod
    def find(self, *args):
        if len(args) == 1:
            if isinstance(args[0], Symbol):
                return interned.val()[args[0]]()
            if isinstance(args[0], str):
                return Keyword.find(Symbol.intern(args[0]))
        if len(args) == 2:
            return Keyword.find(Symbol.intern(*args))
        raise ArityException()

