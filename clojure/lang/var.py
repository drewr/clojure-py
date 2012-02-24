from clojure.lang.iref import IRef
from clojure.lang.ifn import IFn
from clojure.lang.settable import Settable
from clojure.lang.aref import ARef
from clojure.lang.cljexceptions import (ArityException,
                                           InvalidArgumentException,
                                           IllegalStateException)
from clojure.lang.persistenthashmap import EMPTY
from clojure.lang.threadutil import ThreadLocal, currentThread
from clojure.lang.symbol import symbol
from clojure.lang.cljkeyword import keyword
import persistentarraymap

privateKey = keyword(symbol("private"))
macrokey = keyword(symbol(":macro"))
dvals = ThreadLocal()
privateMeta = persistentarraymap.create([privateKey, True])
UKNOWN = symbol("UNKNOWN")


def pushThreadBindings(bindings):
    f = dvals.get(lambda: Frame())
    bmap = f.bindings
    bs = bindings.seq()
    while bs is not None:
        e = bs.first()
        v = e.getKey()
        if not v.dynamic:
            raise IllegalStateException("Can't dynamically bind non-dynamic "
                                        "var: " + str(v.ns) + "/"
                                        + str(v.sym))
        v.validate(v.getValidator(), e.getValue())
        v.threadBound = True
        bmap = bmap.assoc(v, TBox(currentThread(), e.getValue()))
        bs = bs.next()
    dvals.set(Frame(bmap, f))


def popThreadBindings():
    f = dvals.get(lambda: Frame())
    if f.prev is None:
        raise IllegalStateException("Pop without matching push")
    dvals.set(f.prev)


class Var(ARef, Settable, IFn, IRef):
    def __init__(self, ns, sym, root=UKNOWN):
        if root == UKNOWN:
            self.root = Unbound(self)
        self.ns = ns
        self.sym = sym
        self.threadBound = False
        self.root = root
        self._meta = EMPTY
        self.rev = 0
        self.dynamic = False
        if isinstance(self.root, Unbound):
            self.rev += 1

    def setDynamic(self, val=True):
        self.dynamic = val
        return self

    def isDynamic(self):
        return self.dynamic

    def set(self, val):
        self.validate(self.getValidator(), val)
        b = self.getThreadBinding()
        if b is not None:
            if currentThread() != b.thread:
                raise IllegalStateException("Can't set!: " + str(self.sym) +
                                            " from non-binding thread")
            b.val = val
            return self

        raise IllegalStateException("Can't change/establish root binding "
                                    "of: %s with set" % str(self.sym))

    def hasRoot(self):
        return not isinstance(self.root, Unbound)

    def bindRoot(self, root):
        import rt as RT
        self.validate(self.getValidator(), root)
        oldroot = self.root
        self.root = root
        self.rev += 1

    def deref(self):
        b = self.getThreadBinding()
        if b is not None:
            return b.val
        return self.root

    def getThreadBinding(self):
        if self.threadBound:
            e = dvals.get(lambda: Frame()).bindings.entryAt(self)
            if e is not None:
                return e.getValue()
        return None

    def setMeta(self, meta):
        self._meta = meta
        return self

    def setMacro(self):
        self.alterMeta(lambda x, y, z: x.assoc(y, z), macrokey, True)

    def __call__(self, *args):
        return self.deref()(*args)

    def __getitem__(self, item):
        return self.deref().__getitem__(item)

    def __str__(self):
        return str(self.deref())

    def __repr__(self):
        if self.ns is not None:
            return "#" + str(self.ns.__name__) + "/" + str(self.sym)
        return ("#<Var: " +
                (str(self.sym)
                 if self.sym is not None else "--unnamed--") + ">")


def var(root=UKNOWN):
    if root is not UKNOWN:
        return Var(None, None, root)
    else:
        return Var(None, None)


def getThreadBindingFrame():
    f = Val.dvals.get(lambda: Frame())#FIXME: Val undefined
    return f


def cloneThreadBindingFrame():
    f = Val.dvals.get(lambda: Frame()).clone()#FIXME: Val undefined
    return f


def resetThreadBindingFrame(val):
    Var.dvals.set(val)


def internWithRoot(ns, sym, root, replaceRoot=True):
    from namespace import intern as namespaceIntern
    dvout = namespaceIntern(ns, sym)
    if not dvout.hasRoot() or replaceRoot:
        dvout.bindRoot(root)
    return dvout


def find(sym):
    from clojure.lang.namespace import find as findNamespace
    if sym.ns is None:
        raise InvalidArgumentException("Symbol must be namespace-qualified")
    ns = findNamespace(symbol(sym.ns))
    if ns is None:
        raise InvalidArgumentException("No such namespace " + str(sym.ns))
    return ns.findInternedVar(symbol(sym.name))


def intern(ns, name):
    if isinstance(ns, Namespace):#FIXME: Namespace doesn't appear to be defined anywhere
        return ns.intern(name)
    ns = Namespace.findOrCreate(symbol(ns))#FIXME: undefined Namespace
    return intern(ns, name)


def internPrivate(nsName, sym):
    ns = Namespace.findOrCreate(symbol(nsName))#FIXME: undefined Namespace
    ret = intern(ns, symbol(sym))
    ret.setMeta(Var.privateMeta)
    return ret


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
    def __init__(self, bindings=EMPTY, prev=None):
        self.bindings = bindings
        self.prev = prev

    def clone(self):
        return Frame(self.bindings)
