from py.clojure.lang.symbol import Symbol
from py.clojure.lang.namespace import findOrCreate as findOrCreateNamespace
from py.clojure.lang.cljexceptions import CompilerException
from py.clojure.lang.var import Var
from py.clojure.util.byteplay import *


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

    code = [(LOAD_CONST, Var.internWithRoot),
            (LOAD_CONST, ns),
            (LOAD_CONST, sym)]
    code.extend(comp.compile(value))
    code.append((CALL_FUNCTION, 3));

    return code


def compileDot(comp, form):
    from py.clojure.lang.persistentlist import PersistentList

    if len(form) != 3:
        raise CompilerException(". form must have two arguments", form)
    clss = form.next().first()
    member = form.next().next().first()
    code = comp.compile(clss)

    if isinstance(member, Symbol):
        attr = member.name
        args = []
    elif isinstance(member, PersistentList):
        if not isinstance(member.first(), Symbol):
            raise CompilerException("Member name must be symbol")
        attr = member.first().name
        args = []
        if len(member) > 1:
            f = member.next()
            while f is not None:
                args.append(comp.compile(f.first()))
                f = f.next()

    code.append((LOAD_ATTR, attr))
    for x in args:
        code.extend(x)
    code.append((CALL_FUNCTION, len(args)))

    return code



builtins = {Symbol.intern("ns"): compileNS,
            Symbol.intern("def"): compileDef,
            Symbol.intern("."): compileDot}




class Compiler():
    def compileForm(self, form):
        if form.first() in builtins:
            return builtins[form.first()](self, form)
        raise CompilerException("Unknown function " + str(form.first()), form)
    def compileSymbol(self, sym):
        if sym.ns is None:
            sym = Symbol.intern(self.getNS().name.name, sym.name)
        loc = Var.find(sym)
        if loc is None:
            raise CompilerException("Can't find " + str(sym), None)

        return [(LOAD_CONST, loc)]
    def compile(self, itm):
        from py.clojure.lang.persistentlist import PersistentList
        if isinstance(itm, Symbol):
            return self.compileSymbol(itm)
        if isinstance(itm, PersistentList):
            return self.compileForm(itm)
        if itm is None:
            return self.compileNone(itm)
        raise CompilerException("Don't know how to compile", itm)

    def compileNone(self, itm):
        return [(LOAD_CONST, None)]

    def setNS(self, ns):
        self.ns = findOrCreateNamespace(ns)
    def getNS(self):
        if self.ns is not None:
            return self.ns
    def executeCode(self, code):
        if code == []:
            return None
        newcode = code[:]
        newcode.append((RETURN_VALUE, None))
        c = Code(newcode, [], [], False, False, False, "<string>", "<string>", 0, None)
        exec(c.to_code(), dict())