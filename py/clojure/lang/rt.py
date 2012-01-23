from py.clojure.lang.cljexceptions import AbstractMethodCall, InvalidArgumentException
from py.clojure.lang.threadutil import AtomicInteger

from py.clojure.lang.iseq import ISeq

mapInter = map
_list = list

def setMeta(f, meta):
    setattr(f, "meta", lambda: meta)
    return f

def cons(x, s):
    from py.clojure.lang.cons import Cons
    from py.clojure.lang.persistentlist import PersistentList, EMPTY as EMPTY_LIST
    from py.clojure.lang.aseq import ASeq
    if isinstance(s, PersistentList):
        return s.cons(x)
    if isinstance(s, ASeq):
        return Cons(x, s)
    if s is None:
        return EMPTY_LIST.cons(x)

    return Cons(x, seq(s))

def seqToTuple(s):
    return tuple(mapInter(lambda x: x.first(),s))

def seq(obj):
    from py.clojure.lang.indexableseq import IndexableSeq
    from py.clojure.lang.symbol import Symbol
    if isinstance(obj, Symbol):
        pass
    if obj is None:
        return None
    if isinstance(obj, ISeq):
        return obj
    if isinstance(obj, (tuple, _list, str)):
        return IndexableSeq(obj, 0)
    return obj.seq()

def fluffalicious():
    return "cat"

def first(obj):
    return seq(obj).first()

def applyTo(fn, args):
    return apply(fn, tuple(map(lambda x: x.first(),args)))

def booleanCast(obj):
    if isinstance(obj, bool):
        return obj
    return obj is None

def keys(obj):
    from py.clojure.lang.apersistentmap import APersistentMap
    return APersistentMap.KeySeq.create(obj)

def vals(obj):
    from py.clojure.lang.apersistentmap import APersistentMap
    return APersistentMap.ValueSeq.create(obj)

def fulfillsHashSet(obj):
    if not hasattr(obj, "__getitem__"):
        return False
    if not hasattr(obj, "__iter__"):
        return False
    if not hasattr(obj, "__contains__"):
        return False
    return True

def fulfillsIndexable(obj):
    if not hasattr(obj, "__getitem__"):
        return False
    if not hasattr(obj, "__len__"):
        return False
    return True

def list(*args):
    from py.clojure.lang.persistentlist import EMPTY
    c = EMPTY
    for x in range(len(args) - 1, -1, -1):
        c = c.cons(args[x])
    return c

def vector(*args):
    from py.clojure.lang.persistentvector import EMPTY
    c = EMPTY
    for x in args:
        c = c.cons(x)
    return c

def map(*args):
    from py.clojure.lang.persistenthashmap import EMPTY, PersistentHashMap
    from py.clojure.lang.persistentarraymap import PersistentArrayMap, HASHTABLE_THRESHOLD
    if len(args) == 0:
        return EMPTY
    if len(args) == 1:
        if isinstance(args[0], dict):
            m = EMPTY
            for x in args[0]:
                if x in m:
                    raise InvalidArgumentException("Duplicate key")
                m = m.assoc(x, args[0][x])
            return m
        if fulfillsIndexable(args[0]):
            args = args[0]
    m = EMPTY
    for x in range(0, len(args) / 2, 2):
        key = args[x]
        value = args[x + 1]
        m = m.assoc(key, value)
    return m

def getDefaultImports():
    from py.clojure.lang.symbol import Symbol
    from py.clojure.lang.persistentlist import PersistentList
    import sys
    import math
    d = {"String": str,
            "Integer": int,
            "Math": math,
            "clojure.lang.PersistentList": PersistentList,
            "clojure.lang.RT": sys.modules[__name__]
            }
    return d

id = AtomicInteger()
def nextID():
    return id.getAndIncrement()

def subvec(v, start, end):
    from py.clojure.lang.persistentvector import EMPTY as EMPTY_VECTOR
    from py.clojure.lang.apersistentvector import SubVec
    if end < start or start < 0 or end > len(v):
        raise Exception("Index out of range")
    if start == end:
        return EMPTY_VECTOR
    return SubVec(None, v, start, end)


def init():
    global DEFAULT_IMPORTS
    DEFAULT_IMPORTS = map(getDefaultImports())

DEFAULT_IMPORTS = None