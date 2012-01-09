from py.clojure.lang.iref import IRef
from py.clojure.lang.ifn import IFn
from py.clojure.lang.settable import Settable
from py.clojure.lang.aref import ARef
from py.clojure.lang.cljexceptions import ArityException, InvalidArgumentException, IllegalStateException
from py.clojure.lang.persistenthashmap import EMPTY
from py.clojure.lang.threadutil import ThreadLocal, synchronized, currentThread
from py.clojure.lang.symbol import Symbol
from py.clojure.lang.cljkeyword import Keyword
from persistentarraymap import PersistentArrayMap

privateKey = Keyword.intern(Symbol.intern("private"))
macrokey = Keyword.intern(Symbol.intern("macro"))
dvals = ThreadLocal()
privateMeta = PersistentArrayMap.create([privateKey, True])
UKNOWN = Symbol.intern("UNKNOWN")

def pushThreadBindings(bindings):
    f = dvals.get(lambda: Var.Frame())
    bmap = f.bindings
    bs = bindings.seq()
    while bs is not None:
        e = bs.first()
        v = e.getKey()
        if not v.dynamic:
            raise IllegalStateException("Can't dynamically bind non-dynamic var: " + str(v.ns) + "/" + str(v.sym))
        v.validate(v.getValidator(), e.getValue())
        v.threadBound = True
        bmap = bmap.assoc(v, Var.TBox(currentThread(), e.getValue()))
        bs = bs.next()
    dvals.set(Var.Frame(bmap, f))

def popThreadBindings():
    f = dvals.get(Var.Frame())
    if f.prev is None:
        raise IllegalStateException("Pop without matching push")
    dvals.set(f.prev)



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



    def __init__(self, ns, sym, root = UKNOWN):
        if root == UKNOWN:
            self.root = Var.Unbound(self)
        self.ns = ns
        self.sym = sym
        self.threadBound = False
        self.root = root
        self._meta = EMPTY
        self.rev = 0
        self.dynamic = False
        if isinstance(self.root, Var.Unbound):
            self.rev += 1

    @staticmethod
    def create(root = UKNOWN):
        if root is not UKNOWN:
            return Var(None, None, root)
        else:
            return Var(None, None)

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
        return self

    def isDynamic(self):
        return self.dynamic

    def set(self, val):
        self.validate(self.getValidator(), val);
        b = self.getThreadBinding()
        if b is not None:
            if currentThread() != b.thread:
                raise IllegalStateException("Can't set!: " + str(sym) + " from non-binding thread")
            b.val = val
            return self

        raise IllegalStateException(str("Can't change/establish root binding of: "+ str(sym) +" with set"))

    def hasRoot(self):
        return not isinstance(self.root, Var.Unbound)

    def bindRoot(self, root):
        import rt as RT
        self.validate(self.getValidator(), root)
        oldroot = self.root
        self.root = root
        self.rev += 1
        #self.alterMeta(lambda o, k: o.dissoc(k), RT.list(macrokey))
        #self.notifyWatches(oldroot, self.root)

    @staticmethod
    def internWithRoot(ns, sym, root, replaceRoot = True):
        from namespace import intern as namespaceIntern
        dvout = namespaceIntern(ns, sym)
        if not dvout.hasRoot() or replaceRoot:
            dvout.bindRoot(root)
        return dvout

    def __repr__(self):
        if self.ns is not None:
            return "#" + str(self.ns.name) + "/" + str(self.sym)
        return "#<Var: " + (str(self.sym) if self.sym is not None else "--unnamed--") + ">"

    @staticmethod
    def find(sym):
        from py.clojure.lang.namespace import find as findNamespace
        if sym.ns is None:
            raise InvalidArgumentException("Symbol must be namespace-qualified")
        ns = findNamespace(Symbol.intern(sym.ns))
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


    def deref(self):
        b = self.getThreadBinding()
        if b is not None:
            return b.val
        return self.root

    def getThreadBinding(self):
        if self.threadBound:
            e = dvals.get(lambda: Var.Frame()).bindings.entryAt(self)
            if e is not None:
                return e.getValue()
        return None

    def setMeta(self, meta):
        self._meta = meta




