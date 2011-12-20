from iref import IRef
from ifn import IFn
from settable import Settable
from aref import ARef
from exceptions import ArityException, InvalidArgumentException
from persistenthashmap import EMPTY
from threadutil import ThreadLocal, synchronized
from symbol import Symbol

class Var(ARef, Settable, IFn, IRef ):

    class TBox(object):
        def __init__(self, thread, val):
            self.thread = thread
            self.val = val

    class Unbound(IFn):
        def __init__(self, v):
            self.v = v
        def __repr__(self):
            return "Unbound" + str(self.v)
        def __call__(self, *args, **kwargs):
            raise ArityException("Attempting to call unbound fn:" + str(self.v))

    class Frame(object):
        def __init__(self, bindings = EMPTY, prev = None):
            self.bindings = bindings
            self.prev = prev
        def clone(self):
            return Var.Frame(self.bindings)

    dvals = ThreadLocal()
    privateMeta = PersistentArrayMap.create([Var.privateKey, True])

    def __init__(self, ns, sym, root = Var.Unbound()):
        self.ns = ns
        self.sym = sym
        self.threadBound = false
        self.root = root
        self.setMeta(EMPTY)
        self.rev = 0
        self.dynamic = False
        if isinstance(self.root, Var.Unbound):
            self.rev += 1

    @staticmethod
    def getThreadBindingFrame():
        f = Val.dvals.get(lambda: Frame())
        return f

    @staticmethod
    def cloneThreadBindingFrame():
        f = Val.dvals.get(lambda: Frame()).clone()
        return f

    @staticmethod
    def resetThreadBindingFrame(val):
        Var.dvals.set(val)

    def setDynamic(self, val = True):
        self.dynamic = val

    def isDynamic(self):
        return self.dynamic

    @staticmethod
    def intern(ns, sym, root, replaceRoot = true):
        dvout = ns.intern(sym)
        if not dvout.hasRoot() or replaceRoot:
            dvout.bindRoot(root)
        return dvout

    def __repr__(self):
        if self.ns is not None:
            return "#" + str(self.ns.name) + "/" + str(self.sym)
        return "#<Var: " + (str(self.sym) if self.sym is not None else "--unnamed--") + ">"

    @staticmethod
    def find(sym):
        if sym.ns is None:
            raise InvalidArgumentException("Symbol must be namespace-qualified")
        ns = Namespace.find(Symbol.intern(sym.ns))
        if ns is None:
            raise InvalidArgumentException("No such namespace " + str(sym.ns))
        return ns.findInternedVar(Symbol.intern(sym.name))

    @staticmethod
    def intern(ns, name):
        if isinstance(ns, Namespace):
            return ns.intern(name)
        ns = Namespace.findOrCreate(Symbol.intern(ns))
        return Var.intern(ns, name)

    @staticmethod
    def internPrivate(nsName, sym):
        ns = Namespace.findOrCreate(Symbol.intern(nsName))
        ret = Var.intern(ns, Symbol.intern(sym))
        ret.setMeta(Var.privateMeta)
        return ret





