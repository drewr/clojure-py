from py.clojure.lang.symbol import Symbol
from py.clojure.lang.namespace import findOrCreate as findOrCreateNamespace
from py.clojure.lang.cljexceptions import CompilerException, AbstractMethodCall
from py.clojure.lang.persistentvector import PersistentVector
from py.clojure.lang.var import Var
from py.clojure.util.byteplay import *
from py.clojure.lang.cljkeyword import Keyword
import new
import py.clojure.lang.rt as RT
from py.clojure.lang.lispreader import _AMP_
from py.clojure.lang.namespace import findItem
from py.clojure.lang.lispreader import LINE_KEY
import re



_MACRO_ = Keyword.intern(Symbol.intern(":macro"))

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

    code = [(LOAD_GLOBAL, "Var"),
            (LOAD_ATTR, "internWithRoot"),
            (LOAD_GLOBAL, "sys"),
            (LOAD_ATTR, "modules"),
            (LOAD_GLOBAL, "__name__"),
            (BINARY_SUBSCR, None),
            (LOAD_CONST, sym)]
    code.extend(comp.compile(value))
    code.append((CALL_FUNCTION, 3))

    if sym.meta() is not None:
        code.extend([(LOAD_ATTR, 'setMeta'),
                    (LOAD_CONST, sym.meta()),
                    (CALL_FUNCTION, 1)])
    return code

def compileGet(comp, form):
    if len(form) != 3:
        raise CompilerException("get requires 2 arguments", form)
    col = form.next().first()
    itm = form.next().next().first()
    code = comp.compile(col)
    code.extend(comp.compile(itm))
    code.append((BINARY_SUBSCR, None))
    return code

def compileLoopStar(comp, form):
    if len(form) < 3:
        raise CompilerException("loop* takes at least two args")
    form = form.next()
    if not isinstance(form.first(), PersistentVector):
        raise CompilerException("loop* takes a vector as it's first argument")
    s = form.first()
    args = []
    code = []
    idx = 0
    while idx < len(s):
        if len(s) - idx < 2:
            raise CompilerException("loop* takes a even number of bindings")
        local = s[idx]
        if not isinstance(local, Symbol) or local.ns is not None:
            raise CompilerException("bindings must be non-namespaced symbols")

        idx += 1

        body = s[idx]
        if local in comp.locals:
            newlocal = Symbol.intern(str(local)+"_"+str(RT.nextID()))
            code.extend(comp.compile(body))
            comp.locals[local] = comp.locals[local].cons(newlocal)
            args.append(local)
            local = newlocal
        else:
            comp.locals[local] = RT.list(local)
            args.append(local)
            code.extend(comp.compile(body))


        code.append((STORE_FAST, str(local)))

        idx += 1

    form = form.next()
    recurlabel = Label("recurLabel")
    recur = {"label": recurlabel,
             "args": map(lambda x: x.name, args)}
    code.append((recurlabel, None))
    comp.pushRecur(recur)
    code.extend(compileImplcitDo(comp, form))
    comp.popRecur()
    comp.popLocals(args)
    return code

def compileLetStar(comp, form):
    if len(form) < 3:
        raise CompilerException("let* takes at least two args")
    form = form.next()
    if not isinstance(form.first(), PersistentVector):
        raise CompilerException("let* takes a vector as it's first argument")
    s = form.first()
    args = []
    code = []
    idx = 0
    while idx < len(s):
        if len(s) - idx < 2:
            raise CompilerException("let* takes a even number of bindings")
        local = s[idx]
        if not isinstance(local, Symbol) or local.ns is not None:
            raise CompilerException("bindings must be non-namespaced symbols")

        idx += 1

        body = s[idx]
        if local in comp.locals:
            newlocal = Symbol.intern(str(local)+"_"+str(RT.nextID()))
            code.extend(comp.compile(body))
            comp.locals[local] = comp.locals[local].cons(newlocal)
            args.append(local)
            local = newlocal
        else:
            comp.locals[local] = RT.list(local)
            args.append(local)
            code.extend(comp.compile(body))


        code.append((STORE_FAST, str(local)))

        idx += 1

    form = form.next()

    code.extend(compileImplcitDo(comp, form))
    comp.popLocals(args)
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

def compileQuote(comp, form):
    if len(form) != 2:
        raise CompilerException("Quote must only have one argument", form)
    return [(LOAD_CONST, form.next().first())]

def compileIf(comp, form):
    if len(form) != 3 and len(form) != 4:
        raise CompilerException("if takes 2 or 3 args", form)
    cmp = comp.compile(form.next().first())
    body = comp.compile(form.next().next().first())
    if len(form) == 3:
        body2 = [(LOAD_CONST, None)]
    else:
        body2 = comp.compile(form.next().next().next().first())

    elseLabel = Label("IfElse")
    endlabel = Label("IfEnd")
    code = cmp
    code.append((POP_JUMP_IF_FALSE, elseLabel))
    code.extend(body)
    code.append((JUMP_ABSOLUTE, endlabel))
    code.append((elseLabel, None))
    code.extend(body2)
    code.append((endlabel, None))
    return code

def unpackArgs(form):
    locals = {}
    args = []
    lastisargs = False
    argsname = None
    for x in form:
        if x == _AMP_:
            lastisargs = True
            continue
        if lastisargs and argsname is not None:
            raise CompilerException("variable length argument must be the last in the function")
        if lastisargs:
            argsname = x
        if not isinstance(x, Symbol) or x.ns is not None:
            raise CompilerException("fn* arguments must be non namespaced symbols", form)
        locals[x] = RT.list(x)
        args.append(x.name)

    return locals, args, lastisargs, argsname

def compileDo(comp, form):
    if len(form) < 2:
        raise CompilerException("at least 1 arg to do required")
    return compileImplcitDo(comp, form.next())

def compileFn(comp, name, form, orgform):
    locals, args, lastisargs, argsname = unpackArgs(form.first())

    comp.setLocals(locals)
    if orgform.meta() is not None:
        line = orgform.meta()[LINE_KEY]
    else:
        line = 0
    code = [(SetLineno,line if line is not None else 0)]
    recurlabel = Label("recurLabel")
    recur = {"label": recurlabel,
             "args": args}
    code.append((recurlabel, None))
    comp.pushRecur(recur)
    code.extend(compileImplcitDo(comp, form.next()))
    comp.popRecur()

    code.append((RETURN_VALUE,None))

    c = Code(code, [], args, lastisargs, False, True, str(Symbol.intern(comp.getNS().__name__, name.name)), "./clj/clojure/core.clj", 0, None)

    fn = new.function(c.to_code(), comp.ns.__dict__, name.name)

    return [(LOAD_CONST, fn)]

class MultiFn(object):
    def __init__(self, comp, form):
        if len(form) < 2:
            raise CompilerException("FN defs must have at least two vars", form)
        argv = form.first()
        if not isinstance(argv, PersistentVector):
            raise CompilerException("FN arg list must be a vector", form)
        body = form.next()

        self.locals, self.args, self.lastisargs, self.argsname = unpackArgs(argv)
        endLabel = Label("endLabel")
        code = [(LOAD_FAST, '__argsv__'),
                (LOAD_ATTR, '__len__'),
                (CALL_FUNCTION, 0),
                (LOAD_CONST, len(self.args)),
                (COMPARE_OP, ">=" if self.lastisargs else "=="),
                (POP_JUMP_IF_FALSE, endLabel)]
        for x in range(len(self.args)):
            if self.lastisargs and x == len(self.args) - 1:
                offset = len(self.args) - 1
                code.extend([(LOAD_FAST, '__argsv__'),
                             (LOAD_CONST, offset),
                             (SLICE_1, None),
                             (STORE_FAST, self.argsname.name)])
            else:
                code.extend([(LOAD_FAST, '__argsv__'),
                             (LOAD_CONST, x),
                             (BINARY_SUBSCR, None),
                             (STORE_FAST, self.args[x])])

        comp.pushLocals(self.locals)
        recurlabel = Label("recurLabel")
        recur = {"label": recurlabel,
                 "args": self.args}
        code.append((recurlabel, None))
        comp.pushRecur(recur)
        code.extend(compileImplcitDo(comp, body))
        code.append((RETURN_VALUE, None))
        code.append((endLabel, None))
        comp.popRecur()
        comp.popLocals(self.locals)





        self.code = code


def compileMultiFn(comp, name, form):
    s = form
    argdefs = []
    while s is not None:
        argdefs.append(MultiFn(comp, s.first()))
        s = s.next()
    argdefs = sorted(argdefs, lambda x, y: len(x.args) < len(y.args))
    if len(filter(lambda x: x.lastisargs, argdefs)) > 1:
        raise CompilerException("Only one function overload may have variable number of arguments", form)


    code = []
    args = []
    for x in argdefs:
        code.extend(x.code)
        for x in x.args:
            if x not in args:
                args.append(x)

    code.append((LOAD_CONST, Exception))
    code.append((CALL_FUNCTION, 0))
    code.append((RAISE_VARARGS, 1))

    c = Code(code, [], ["__argsv__"], True, False, True, str(Symbol.intern(comp.getNS().__name__, name.name)), "./clj/clojure/core.clj", 0, None)

    fn = new.function(c.to_code(), comp.ns.__dict__, name.name)

    return [(LOAD_CONST, fn)]

def compileImplcitDo(comp, form):
    code = []
    s = form
    while s is not None:
        code.extend(comp.compile(s.first()))
        s = s.next()
        if s is not None:
            code.append((POP_VALUE, None))
    return code


def compileFNStar(comp, form):
    orgform = form
    if len(form) < 2:
        raise CompilerException("2 or more arguments to fn* required", form)
    form = form.next()
    name = form.first()
    pushed = False
    if not isinstance(name, Symbol):
        comp.pushName(name)
        pushed = True
        name = Symbol.intern(comp.getNamesString() + "fn" + str(RT.nextID()))
    else:
        form = form.next()

    if isinstance(form.first(), PersistentVector):
        code = compileFn(comp, name, form, orgform)
    else:
        code = compileMultiFn(comp, name, form)

    if pushed:
        comp.popName()

    return code

def compileVector(comp, form):
    code = []
    code.extend(comp.compile(Symbol.intern("clojure.lang.rt.vector")))
    for x in form:
        code.extend(comp.compile(x))
    code.append((CALL_FUNCTION, len(form)))
    return code

def compileRecur(comp, form):
    s = form.next()
    idx = 0
    code = []
    while s is not None:
        code.extend(comp.compile(s.first()))
        if idx >= len(comp.recurPoint.first()["args"]):
            raise CompilerException("to many arguments to recur", form)
        local = comp.recurPoint.first()["args"][idx]
        local = comp.locals[Symbol.intern(local)].first()
        code.append((STORE_FAST, local.name ))
        idx += 1
        s = s.next()
    code.append((JUMP_ABSOLUTE, comp.recurPoint.first()["label"]))
    return code

def compileIs(comp, form):
    if len(form) != 3:
        raise CompilerException("is requires 2 arguments", form)
    fst = form.next().first()
    itm = form.next().next().first()
    code = comp.compile(fst)
    code.extend(comp.compile(itm))
    code.append((COMPARE_OP, "is"))
    return code


builtins = {Symbol.intern("ns"): compileNS,
            Symbol.intern("def"): compileDef,
            Symbol.intern("."): compileDot,
            Symbol.intern("fn*"): compileFNStar,
            Symbol.intern("quote"): compileQuote,
            Symbol.intern("if"): compileIf,
            Symbol.intern("recur"): compileRecur,
            Symbol.intern("do"): compileDo,
            Symbol.intern("let*"): compileLetStar,
            Symbol.intern("get"): compileGet,
            Symbol.intern("loop*"): compileLoopStar,
            Symbol.intern("is?"): compileIs}




class Compiler():
    def __init__(self):
        self.locals = {}
        self.recurPoint = RT.list()
        self.names = RT.list()

    def pushRecur(self, label):
        self.recurPoint = RT.cons(label, self.recurPoint)
    def popRecur(self):
        self.recurPoint = self.recurPoint.next()

    def pushName(self, name):
        if self.names is None:
            self.names = RT.list((name))
        self.names = self.names.cons(name)

    def popName(self):
        self.names = self.names.next()

    def getNamesString(self):
        n = []
        for x in self.names:
            n.append(str(x.first()))
        return "_".join(n)

    def compileMethodAccess(self, form):
        attrname = form.first().name[1:]
        if len(form) < 2:
            raise CompilerException("Method access must have at least one argument", form)
        c = self.compile(form.next().first())
        c.append((LOAD_ATTR, attrname))
        s = form.next().next()
        while s is not None:
            c.extend(self.compile(s.first()))
            s = s.next()
        c.append((CALL_FUNCTION, (len(form) - 2)))
        return c

    def compileForm(self, form):
        if form.first() in builtins:
            return builtins[form.first()](self, form)
        if isinstance(form.first(), Symbol):
            macro = findItem(self.getNS(), form.first())
            if macro is not None:

                if hasattr(macro, "meta") and macro.meta()[_MACRO_]:
                    mresult = macro(macro, self, *RT.seqToTuple(form.next()))
                    s = repr(mresult)
                    return self.compile(mresult)
        c = None
        if isinstance(form.first(), Symbol):
            if form.first().name.startswith(".") and form.first().ns is None:
                c = self.compileMethodAccess(form)
                return c
        if c is None:
            c = self.compile(form.first())
        f = form.next()
        acount = 0
        while f is not None:
            c.extend(self.compile(f.first()))
            acount += 1
            f = f.next()
        c.append((CALL_FUNCTION, acount))

        return c

    def compileAccessList(self, sym):
        c = []
        first = True
        for x in self.getAccessList(sym):
            if first:
                c.append((LOAD_GLOBAL, x))
                first = False
            else:
                c.append((LOAD_ATTR, x))
        return c

    def getAccessList(self, sym):
        return re.split('[\./]', str(sym))


    def compileSymbol(self, sym):


        if sym in self.locals:
            return self.compileLocal(sym)
        return self.compileAccessList(sym)

    def compileLocal(self, sym):
        return [(LOAD_FAST, self.locals[sym].first().name)]

    def compile(self, itm):
        from py.clojure.lang.persistentlist import PersistentList
        from py.clojure.lang.cons import Cons

        if isinstance(itm, Symbol):
            return self.compileSymbol(itm)
        if isinstance(itm, PersistentList) or isinstance(itm, Cons):
            return self.compileForm(itm)
        if itm is None:
            return self.compileNone(itm)
        if itm.__class__ in [str, int]:
            return [(LOAD_CONST, itm)]

        if isinstance(itm, PersistentVector):
            return compileVector(self, itm)

        raise CompilerException("Don't know how to compile" + str(itm.__class__), None)


    def setLocals(self, locals):
        self.locals = locals

    def compileNone(self, itm):
        return [(LOAD_CONST, None)]

    def setNS(self, ns):
        self.ns = findOrCreateNamespace(ns)

    def getNS(self):
        if self.ns is not None:
            return self.ns

    def executeCode(self, code):
        import sys
        if code == []:
            return None
        newcode = code[:]
        newcode.append((RETURN_VALUE, None))
        c = Code(newcode, [], [], False, False, False, str(Symbol.intern(self.getNS().__name__, "<string>")), "./clj/clojure/core.clj", 0, None)
        globs = {}
        for x in dir(self.ns):
            globs[x] = getattr(self.ns, x)
        retval = eval(c.to_code(), globs)
        for x in globs:
            setattr(self.ns, x, globs[x])
        return retval

    def pushLocals(self, locals):
        for x in locals:
            if x in self.locals:
                self.locals[x] = RT.cons(x, self.locals[x])
            else:
                self.locals[x] = RT.cons(x, None)

    def popLocals(self, locals):
        for x in locals:
            self.locals[x] = self.locals[x].next()

    def standardImports(self):
        return [(LOAD_CONST, -1),
                (LOAD_CONST, None),
                (IMPORT_NAME, "py.clojure.standardimports"),
                (IMPORT_STAR, None)]

    def executeModule(self, code):
        code.append((RETURN_VALUE, None))
        c = Code(code, [], [], False, False, False, str(Symbol.intern(self.getNS().__name__, "<string>")), "./clj/clojure/core.clj", 0, None)
        import marshal
        import pickle
        import py_compile
        import time
        import dis

        dis.dis(c)
        codeobject = c.to_code()
        print codeobject.__class__ is compileDef.__class__

        with open('output.pyc', 'wb') as fc:
            fc.write(py_compile.MAGIC)
            py_compile.wr_long(fc, long(time.time()))
            marshal.dump(c, fc)

