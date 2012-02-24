from clojure.lang.areference import AReference
from clojure.lang.atomicreference import AtomicReference
from clojure.lang.persistenthashmap import EMPTY as EMPTY_MAP
from clojure.lang.cljexceptions import (InvalidArgumentException,
                                        IllegalStateException,
                                        ArityException,
                                        IllegalArgumentException)
import clojure.lang.rt as RT
import sys, new

namespaces = AtomicReference(EMPTY_MAP)

def areDifferentInstancesOfSameClassName(o1, o2):
    return o1.__class__ is o2.__class__

def addDefaultImports(mod):
    import clojure.lang.rt as RT
    import clojure.standardimports as stdimps
    for i in dir(stdimps):
        if i.startswith("_"):
            continue
        setattr(mod, i, getattr(stdimps, i))
    if "clojure" in sys.modules:
        core = sys.modules["clojure"].core
        setattr(getattr(mod, "clojure"), "core", core)
        for i in dir(core):
            if i.startswith("_"):
                continue
            setattr(mod, i, getattr(core, i))
    return mod

def findOrCreateIn(module, parts):
    if not parts:
        return module
    part = parts[0]
    parts = parts[1:]
    if hasattr(module, part):
        return findOrCreateIn(getattr(module, part), parts)
    mod = new.module(module.__name__ + "." + part)
    setattr(module, part, mod)
    return findOrCreateIn(mod, parts)

def findOrCreate(name):
    from clojure.lang.symbol import Symbol
    if isinstance(name, Symbol) and name.name is not None:
        name = name.name
    parts = name.split(".")

    if parts[0] in sys.modules:
        mod = sys.modules[parts[0]]
    else:
        mod = new.module(parts[0])
        sys.modules[parts[0]] = mod

    mod = findOrCreateIn(mod, parts[1:])
    addDefaultImports(mod)
    return mod

def remove(name):
    if name.equals(RT.CLOJURE_NS.name):
        raise IllegalArgumentException("Cannot remove clojure namespace");

    while name in namespaces.get():
        newns = namespaces.get().without(name)
        namespaces.compareAndSet(namespaces, newns)

def find(name):
    return namespaces.get()[name]

def findItem(ns, sym):
    from clojure.lang.symbol import Symbol
    if isinstance(sym, Symbol):
        if sym.ns == ns.__name__:
            if not hasattr(ns, sym.name):
                return None
            return getattr(ns, sym.name)
        if sym.ns is not None:
            mod = findModule(sym.ns)
            if hasattr(mod, sym.name):
                return getattr(mod, sym.name)
            return None
        if not hasattr(ns, str(sym)):
            return None
        return getattr(ns, str(sym))
    return getattr(ns, sym)

def findModule(sym, module = None):
    if module is None:
        sym = sym.split(".")
        parts = sym[1:]
        name = sym[0]
        if name not in sys.modules:
            return None
        if len(parts):
            return findModule(parts, sys.modules[name])
        return sys.modules[name]

    name = sym[0]
    parts = sym[1:]
    if not hasattr(module, name):
        return None
    if len(parts):
        return findModule(parts, getattr(module, name))
    return getattr(module, name)

def intern(ns, sym):
    from clojure.lang.var import Var

    if sym.ns is not None:
        raise InvalidArgumentException("Can't intern namespace-qualified symbol")
    map = ns
    v = Var(ns, sym)
    setattr(ns, sym.name, v)
    return v
