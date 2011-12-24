from ipersistentmap import IPersistentMap
from mapentry import MapEntry
from cljexceptions import AbstractMethodCall, ArityException, InvalidArgumentException
from aseq import ASeq
import rt as RT

class APersistentMap(IPersistentMap):
    def cons(self, o):
        return RT.conjToAssoc(self, o)
    def __eq__(self, other):
        return APersistentMap.mapEquals(self, other)

    def __getitem__(self, item):
        return self.valAt(item)

    @staticmethod
    def mapEquals(m1, m2):
        if m1 is m2:
            return True
        if not hasattr(m2, "__getitem__"):
            return False
        if not hasattr(m2, "__len__"):
            return False

        if len(m1) != len(m2):
            return False

        for s in m1.interator():
            e = s.first()
            found = m2.containsKey(e.getKey())
            if not found or e.getValue() != m2.get(e.getKey()):
                return False
        return True

    @staticmethod
    def mapHash(map):
        return reduce(lambda h, v: h + (0 if v.getKey() is None else hash(v.getKey()))
                                     ^ (0 if v.getValue() is None else hash(v.getValue())),
                      map.interator(),
                      0)

    def __hash__(self):
        return APersistentMap.mapHash(self)

    class KeySeq(ASeq):
        def __init__(self, *args):
            if len(args) == 1:
                self.seq = args[0]
            elif len(args) == 2:
                self._meta = args[0]
                self.seq = args[1]
            else:
                raise ArityException()

        @staticmethod
        def create(s):
            if s is None:
                return None
            return APersistentMap.KeySeq(s)

        def first(self):
            return self.first().getKey()
        def next(self):
            return APersistentMap.KeySeq.create(self.seq.next())
        def withMeta(self, meta):
            return APersistentMap.KeySeq(meta, self.seq)

    class ValueSeq(ASeq):
            def __init__(self, *args):
                if len(args) == 1:
                    self.seq = args[0]
                elif len(args) == 2:
                    self._meta = args[0]
                    self.seq = args[1]
                else:
                    raise ArityException()

            @staticmethod
            def create(s):
                if s is None:
                    return None
                return APersistentMap.ValueSeq(s)

            def first(self):
                return self.first().getValue()
            def next(self):
                return APersistentMap.ValueSeq.create(self.seq.next())
            def withMeta(self, meta):
                return APersistentMap.ValueSeq(meta, self.seq)

    def __call__(self, *args, **kwargs):
        return apply(self.valAt, args)

    def __getitem__(self, item):
        return self.valAt(item)

    def __contains__(self, item):
        return self.containsKey(item)







