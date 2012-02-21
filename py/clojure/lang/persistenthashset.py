from py.clojure.lang.apersistentset import APersistentSet
from py.clojure.lang.persistenthashmap import EMPTY as EMPTY_MAP
from py.clojure.lang.iobj import IObj

class PersistentHashSet(APersistentSet, IObj):
    def __init__(self, meta, impl):
        APersistentSet.__init__(self, impl)
        self._meta = meta

    def cons(self, o):
        if o in self:
            return self
        return PersistentHashSet(self._meta, self.impl.assoc(o, o))

    def meta(self):
        return self._meta

    def withMeta(self, meta):
        return PersistentHashSet(meta, self.impl)

    def empty(self):
        return EMPTY.withMeta(self.meta())

    def disjoin(self, key):
        if key not in self:
            return self
        return PersistentHashSet(self._meta, self.impl.without(key))

def create(*args):
    if not len(args):
        return EMPTY
    if len(args) == 1 and hasattr(args[0], "__iter__"):
        m = EMPTY
        s = args[0]
        for x in s:
            m = m.cons(x)
        return m
    m = EMPTY
    for x in args:
        m = m.cons(x)
    return m


EMPTY = PersistentHashSet(None, EMPTY_MAP)
