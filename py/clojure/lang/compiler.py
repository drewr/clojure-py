from py.clojure.lang.symbol import Symbol
from py.clojure.lang.namespace import findOrCreate as findOrCreateNamespace
from py.clojure.lang.cljexceptions import CompilerException
from py.clojure.lang.var import Var


def compileNS(comp, form):
    rest = form.next()
    if len(rest) != 1:
        raise CompilerException("ns only supports one item", rest)
    comp.setNS(rest.first())
    return []

def compileDef(comp, form):
    if len(form) not in [2, 3]:
        raise CompilerException("Only 2 or 3 arguments allowed to def", form)
    sym = form.next().first()
    value = None
    if len(form) == 3:
        value = form.next().next().first()
    if sym.ns is None:
        ns = comp.getNS()
    else:
        ns = sym.ns
    return Var.internWithRoot(ns, sym, value)



builtins = {Symbol.intern("ns"): compileNS,
            Symbol.intern("def"): compileDef}




class Compiler():
    def compileForm(self, form):
        if form.first() in builtins:
            return builtins[form.first()](self, form)
        raise CompilerException("Unknown function " + str(form.first()), form)
    def compile(self, itm):
        from py.clojure.lang.persistentlist import PersistentList
        if isinstance(itm, Symbol):
            return Var.find(itm)
        if isinstance(itm, PersistentList):
            return self.compileForm(itm)
        raise CompilerException("Don't know how to compile", itm)

    def setNS(self, ns):
        self.ns = findOrCreateNamespace(ns)
    def getNS(self):
        if self.ns is not None:
            return self.ns