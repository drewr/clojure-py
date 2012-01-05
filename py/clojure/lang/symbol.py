from py.clojure.lang.iobj import IObj
from py.clojure.lang.cljexceptions import ArityException


class Symbol(object, IObj):
    def __init__(self, *args):
        if len(args) == 2:
            self.ns = args[0].name if isinstance(args[0], Symbol) else args[0]
            self.name = args[1]
            self._meta = None
        elif len(args) == 3:
            self._meta = args[0]
            self.ns = args[1]
            self.name = args[2]
        else:
            raise ArityException()
        from namespace import Namespace
        if isinstance(self.ns, Namespace):
            pass

    def withMeta(self, meta):
        if meta is self.meta():
            return self
        return Symbol(meta, self.ns, self.name)

    def meta(self):
        return self._meta

    def __eq__(self, other):
        if self is other:
            return True
        if not isinstance(other, Symbol):
            return False
        return (self.ns is other.ns) and (self.name is other.name)

    def __hash__(self):
        return hash(self.name) ^ hash(self.ns)

    def __repr__(self):
        if self.ns is None:
            return self.name
        else:
            return self.ns + "/" + self.name

    @staticmethod
    def intern(*args):
        if len(args) == 1:
            a = args[0]
            if isinstance(a, Symbol):
                return a
            idx = a.rfind("/")
            if idx == -1 or a == "/":
                return Symbol(None, intern(a))
            else:
                return Symbol(a[idx:], a[:idx+1])

            return Symbol(null, )
        elif len(args) == 2:
            return Symbol(args[0], args[1])
        else:
            raise ArityException()
