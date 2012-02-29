from clojure.lang.symbol import Symbol, symbol
from clojure.lang.namespace import findOrCreate as findOrCreateNamespace
from clojure.lang.cljexceptions import CompilerException, AbstractMethodCall
from clojure.lang.persistentvector import PersistentVector
from clojure.lang.ipersistentvector import IPersistentVector
from clojure.lang.ipersistentmap import IPersistentMap
from clojure.lang.ipersistentlist import IPersistentList
from clojure.lang.var import Var, define, intern as internVar, var as createVar
from clojure.util.byteplay import *
import clojure.util.byteplay as byteplay
from clojure.lang.cljkeyword import Keyword, keyword
from clojure.lang.namespace import find as findNamespace
import new
import clojure.lang.rt as RT
from clojure.lang.lispreader import _AMP_
from clojure.lang.namespace import findItem
from clojure.lang.lispreader import LINE_KEY, garg
import re
import new
import sys

_MACRO_ = keyword(symbol(":macro"))
version = (sys.version_info[0] * 10) + sys.version_info[1]

def emitJump(label):
    if version == 26:
        return [(JUMP_IF_FALSE, label),
                (POP_TOP, None)]
    else:
        return [(POP_JUMP_IF_FALSE, label)]

def emitLanding(label):
    if version == 26:
        return [(label, None),
                (POP_TOP, None)]
    else:
        return [(label, None)]

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

    comp.pushName(sym.name)

    code = []
    v = internVar(comp.getNS(), sym)
    v.setMeta(sym.meta())
    code.append((LOAD_CONST, v))
    code.append((LOAD_ATTR, "bindRoot"))
    code.extend(comp.compile(value))
    code.append((CALL_FUNCTION, 1))


    comp.popName()
    return code

def compileBytecode(comp, form):
    codename = form.first().name
    if not hasattr(byteplay, codename):
        raise CompilerException("bytecode " + codename + " unknown", form)
    bc = getattr(byteplay, codename)
    hasarg = bc in byteplay.hasarg
    form = form.next()
    arg = None
    if hasarg:
        arg = form.first()
        if not isinstance(arg, (int, str, unicode)) \
           and bc is not LOAD_CONST:
            raise CompilerException("first argument to "+ codename + " must be int, unicode, or str", form)
        
        arg = evalForm(arg, comp.getNS().__name__)
        form = form.next()
        
    se = byteplay.getse(bc, arg)
    if form != None and se[0] != 0:
        if (se[0] != len(form) or se[1] > 1):
            raise CompilerException("literal bytecode " + codename + " not supported", form)
    s = form
    code = []
    while s is not None:
        code.extend(comp.compile(s.first()))
        s = s.next()
    code.append((bc, arg))
    if se[1] == 0:
        code.append((LOAD_CONST, None))
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
        if local in comp.aliases:
            newlocal = symbol(str(local)+"_"+str(RT.nextID()))
            code.extend(comp.compile(body))
            comp.pushAlias(local, RenamedLocal(newlocal))
            args.append(local)
        else:
            comp.pushAlias(local, RenamedLocal(local))
            args.append(local)
            code.extend(comp.compile(body))


        code.extend(comp.getAlias(local).compileSet(comp))

        idx += 1

    form = form.next()
    recurlabel = Label("recurLabel")
    recur = {"label": recurlabel,
             "args": map(lambda x: comp.getAlias(x).compileSet(comp), args)}
    code.append((recurlabel, None))
    comp.pushRecur(recur)
    code.extend(compileImplcitDo(comp, form))
    comp.popRecur()
    comp.popAliases(args)
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
        if comp.getAlias(local) is not None:
            code.extend(comp.compile(body))
            newlocal = symbol(str(local)+"_"+str(RT.nextID()))
            comp.pushAlias(local, RenamedLocal(newlocal))
            args.append(local)
        else:
            code.extend(comp.compile(body))
            comp.pushAlias(local, RenamedLocal(local))
            args.append(local)

        code.extend(comp.getAlias(local).compileSet(comp))

        idx += 1

    form = form.next()

    code.extend(compileImplcitDo(comp, form))
    comp.popAliases(args)
    return code




def compileDot(comp, form):
    from clojure.lang.persistentlist import PersistentList
    from clojure.lang.iseq import ISeq

    if len(form) != 3:
        raise CompilerException(". form must have two arguments", form)
    clss = form.next().first()
    member = form.next().next().first()


    if isinstance(member, Symbol):
        attr = member.name
        args = []
    elif isinstance(member, ISeq):
        if not isinstance(member.first(), Symbol):
            raise CompilerException("Member name must be symbol")
        attr = member.first().name
        args = []
        if len(member) > 1:
            f = member.next()
            while f is not None:
                args.append(comp.compile(f.first()))
                f = f.next()

    alias = comp.getAlias(clss)
    if alias:
        code = alias.compile(comp)
        code.append((LOAD_ATTR, attr))
    else:
        code = comp.compile(symbol(clss, attr))
        
    for x in args:
        code.extend(x)
    code.append((CALL_FUNCTION, len(args)))

    return code

def compileQuote(comp, form):
    if len(form) != 2:
        raise CompilerException("Quote must only have one argument", form)
    return [(LOAD_CONST, form.next().first())]

def compilePyIf(comp, form):
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
    code.extend(emitJump(elseLabel))
    code.extend(body)
    code.append((JUMP_ABSOLUTE, endlabel))
    code.extend(emitLanding(elseLabel))
    code.extend(body2)
    code.append((endlabel, None))
    return code

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
    condition_name = garg(0).name
    code = cmp
    code.append((STORE_FAST, condition_name))
    code.append((LOAD_FAST, condition_name))
    code.append((LOAD_CONST, None))
    code.append((COMPARE_OP, 'is not'))
    code.extend(emitJump(elseLabel))
    code.append((LOAD_FAST, condition_name))
    code.append((LOAD_CONST, False))
    code.append((COMPARE_OP, '!='))
    code.extend(emitJump(elseLabel))
    code.extend(body)
    code.append((JUMP_ABSOLUTE, endlabel))
    code.extend(emitLanding(elseLabel))
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
            raise CompilerException("fn* arguments must be non namespaced symbols " +
                                    " got " + str(form) + " instead", form)
        locals[x] = RT.list(x)
        args.append(x.name)

    return locals, args, lastisargs, argsname

def compileDo(comp, form):
    if len(form) < 2:
        raise CompilerException("at least 1 arg to do required")
    return compileImplcitDo(comp, form.next())

def compileFn(comp, name, form, orgform):
    locals, args, lastisargs, argsname = unpackArgs(form.first())

    for x in locals:
        comp.pushAlias(x, FnArgument(x))

    if orgform.meta() is not None:
        line = orgform.meta()[LINE_KEY]
    else:
        line = 0
    code = [(SetLineno,line if line is not None else 0)]
    if lastisargs:
        code.extend(cleanRest(argsname.name))

    recurlabel = Label("recurLabel")

    recur = {"label": recurlabel,
    "args": map(lambda x: comp.getAlias(symbol(x)).compileSet(comp), args)}

    code.append((recurlabel, None))
    comp.pushRecur(recur)
    code.extend(compileImplcitDo(comp, form.next()))
    comp.popRecur()

    code.append((RETURN_VALUE,None))

    comp.popAliases(locals)

    clist = map(lambda x: x.sym.name, comp.closureList())
    c = Code(code, clist, args, lastisargs, False, True, str(symbol(comp.getNS().__name__, name.name)), comp.filename, 0, None)
    if not clist:
        c = new.function(c.to_code(), comp.ns.__dict__, name.name)

    return [(LOAD_CONST, c)], c
    
def cleanRest(name):
    label = Label("isclean")
    code = []
    code.append((LOAD_GLOBAL, "len"))
    code.append((LOAD_FAST, name))
    code.append((CALL_FUNCTION, 1))
    code.append((LOAD_CONST, 0))
    code.append((COMPARE_OP, "=="))
    code.extend(emitJump(label))
    code.append((LOAD_CONST, None))
    code.append((STORE_FAST, name))
    if version == 26:
        code.append((LOAD_CONST, None))
    code.extend(emitLanding(label))
    return code

class MultiFn(object):
    def __init__(self, comp, form):
        form = RT.seq(form)
        if len(form) < 2:
            raise CompilerException("FN defs must have at least two vars", form)
        argv = form.first()
        if not isinstance(argv, PersistentVector):
            raise CompilerException("FN arg list must be a vector", form)
        body = form.next()

        self.locals, self.args, self.lastisargs, self.argsname = unpackArgs(argv)
        endLabel = Label("endLabel")
        argcode = [(LOAD_FAST, '__argsv__'),
            (LOAD_ATTR, '__len__'),
            (CALL_FUNCTION, 0),
            (LOAD_CONST, len(self.args) - (1 if self.lastisargs else 0)),
            (COMPARE_OP, ">=" if self.lastisargs else "==")]
        argcode.extend(emitJump(endLabel))
        for x in range(len(self.args)):
            if self.lastisargs and x == len(self.args) - 1:
                offset = len(self.args) - 1
                argcode.extend([(LOAD_FAST, '__argsv__'),
                    (LOAD_CONST, offset),
                    (SLICE_1, None),
                    (STORE_FAST, self.argsname.name)])
                argcode.extend(cleanRest(self.argsname.name))
            else:
                argcode.extend([(LOAD_FAST, '__argsv__'),
                    (LOAD_CONST, x),
                    (BINARY_SUBSCR, None),
                    (STORE_FAST, self.args[x])])

        for x in self.locals:
            comp.pushAlias(x, FnArgument(x))

        recurlabel = Label("recurLabel")

        recur = {"label": recurlabel,
        "args": map(lambda x: comp.getAlias(symbol(x)).compileSet(comp), self.args)}
        
        bodycode = [(recurlabel, None)]
        comp.pushRecur(recur)
        bodycode.extend(compileImplcitDo(comp, body))
        bodycode.append((RETURN_VALUE, None))
        bodycode.extend(emitLanding(endLabel))
        comp.popRecur()
        comp.popAliases(self.locals)

        self.argcode = argcode
        self.bodycode = bodycode


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
    if len(argdefs) == 1 and not argdefs[0].lastisargs:
        hasvararg = False
        argslist = argdefs[0].args
        code.extend(argdefs[0].bodycode)
    else:
        hasvararg = True
        argslist = ["__argsv__"]
        for x in argdefs:
            code.extend(x.argcode)
            code.extend(x.bodycode)

        code.append((LOAD_CONST, Exception))
        code.append((CALL_FUNCTION, 0))
        code.append((RAISE_VARARGS, 1))

    clist = map(lambda x: x.sym.name, comp.closureList())
    c = Code(code, clist, argslist, hasvararg, False, True, str(symbol(comp.getNS().__name__, name.name)), comp.filename, 0, None)

    if not clist:
        c = new.function(c.to_code(), comp.ns.__dict__, name.name)

    
    return [(LOAD_CONST, c)], c

def compileImplcitDo(comp, form):
    code = []
    s = form
    while s is not None:
        code.extend(comp.compile(s.first()))
        s = s.next()
        if s is not None:
            code.append((POP_TOP, None))
    if not code:
        code.append((LOAD_CONST, None))
    return code


def compileFNStar(comp, form):
    haslocalcaptures = False
    aliases = []
    if len(comp.aliases) > 0: # we might have closures to deal with
        for x in comp.aliases:
            
            comp.pushAlias(x, Closure(x))
            aliases.append(x)
        haslocalcaptures = True
        

    orgform = form
    if len(form) < 2:
        raise CompilerException("2 or more arguments to fn* required", form)
    form = form.next()
    name = form.first()
    pushed = False
    
    if not isinstance(name, Symbol):
        name = comp.getNamesString()
    else:
        comp.pushName(name.name)
        pushed = True
        form = form.next()

    name = symbol(name)
    
    # This is fun stuff here. The idea is that we want closures to be able
    # to call themselves. But we can't get a pointer to a closure until after
    # it's created, which is when we actually run this code. So, we're going to
    # create a tmp local that is None at first, then pass that in as a possible
    # closure cell. Then after we create the closure with MAKE_CLOSURE we'll 
    # populate this var with the correct value
    
    selfalias = Closure(name)
    
    comp.pushAlias(name, selfalias)

    if isinstance(form.first(), IPersistentVector):
        code, ptr = compileFn(comp, name, form, orgform)
    else:
        code, ptr = compileMultiFn(comp, name, form)

    if pushed:
        comp.popName()

    clist = comp.closureList()
    fcode = []

    if haslocalcaptures:
        comp.popAliases(aliases)

    if clist:
        for x in clist:
            if x is not selfalias:   #we'll populate selfalias later
                fcode.extend(comp.getAlias(x.sym).compile(comp))  # Load our local version
                fcode.append((STORE_DEREF, x.sym.name))            # Store it in a Closure Cell
            fcode.append((LOAD_CLOSURE, x.sym.name))           # Push the cell on the stack
        fcode.append((BUILD_TUPLE, len(clist)))
        fcode.extend(code)
        fcode.append((MAKE_CLOSURE, 0))
        code = fcode

    if selfalias in clist:
        prefix = []
        prefix.append((LOAD_CONST, None))
        prefix.extend(selfalias.compileSet(comp))
        prefix.extend(code)
        code = prefix
        code.append((DUP_TOP, None))
        code.extend(selfalias.compileSet(comp))

    comp.popAlias(symbol(name)) #closure


    return code

def compileVector(comp, form):
    code = []
    code.extend(comp.compile(symbol("clojure.lang.rt", "vector")))
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

        idx += 1
        s = s.next()
        
    sets = comp.recurPoint.first()["args"][:]
    sets.reverse()
    for x in sets:
        code.extend(x)
    code.append((JUMP_ABSOLUTE, comp.recurPoint.first()["label"]))
    return code

def compileIs(comp, form):
    if len(form) != 3:
        raise CompilerException("is? requires 2 arguments", form)
    fst = form.next().first()
    itm = form.next().next().first()
    code = comp.compile(fst)
    code.extend(comp.compile(itm))
    code.append((COMPARE_OP, "is"))
    return code


def compileMap(comp, form):
    s = form.seq()
    c = 0
    code = []
    code.extend(comp.compile(symbol("clojure.lang.rt", "map")))
    while s is not None:
        kvp = s.first()
        code.extend(comp.compile(kvp.getKey()))
        code.extend(comp.compile(kvp.getValue()))
        c += 2
        s = s.next()
    code.append([CALL_FUNCTION, c])
    return code

def compileKeyword(comp, kw):
    return [(LOAD_CONST, kw)]

def compileBool(comp, b):
    return [(LOAD_CONST, b)]

def compileThrow(comp, form):
    if len(form) != 2:
        raise CompilerException("throw requires two arguments", form)
    code = comp.compile(form.next().first())
    code.append((RAISE_VARARGS, 1))
    return code

def compileApply(comp, form):
    s = form.next()
    code = []
    while s is not None:
        code.extend(comp.compile(s.first()))

        s = s.next()
    code.append((LOAD_CONST, RT.seqToTuple))
    code.append((ROT_TWO, None))
    code.append((CALL_FUNCTION, 1))
    code.append((CALL_FUNCTION_VAR, len(form) - 3))
    return code

def compileBuiltin(comp, form):
    if len(form) != 2:
        raise CompilerException("throw requires two arguments", form)
    name = str(form.next().first())
    return [(LOAD_CONST, getBuiltin(name))]

def getBuiltin(name):
    import __builtin__
    if hasattr(__builtin__, name):
        return getattr(__builtin__, name)

    raise CompilerException("Python builtin not found " + name, name)

def compileLetMacro(comp, form):
    if len(form) < 3:
        raise CompilerException("alias-properties takes at least two args", form)

    form = form.next()


    s = RT.seq(form.first())
    syms = []
    while s is not None:
        sym = s.first()
        syms.append(sym)
        s = s.next()
        if s is None:
            raise CompilerException("let-macro takes a even number of bindings")
        macro = s.first()

        comp.pushAlias(sym, LocalMacro(sym, macro))

        s = s.next()

    body = form.next()

    code = compileImplcitDo(comp, body)

    comp.popAliases(syms)
    return code

def compileCompiler(comp, form):
    return [(LOAD_CONST, comp)]


builtins = {symbol("ns"): compileNS,
            symbol("def"): compileDef,
            symbol("."): compileDot,
            symbol("fn*"): compileFNStar,
            symbol("quote"): compileQuote,
            symbol("py", "if"): compilePyIf,
            symbol("if"): compileIf,
            symbol("recur"): compileRecur,
            symbol("do"): compileDo,
            symbol("let*"): compileLetStar,
            symbol("loop*"): compileLoopStar,
            symbol("is?"): compileIs,
            symbol("throw"): compileThrow,
            symbol("apply"): compileApply,
            symbol("let-macro"): compileLetMacro,
            symbol("__compiler__"): compileCompiler}


"""
We should mention a few words about aliases. Aliases are created when the
user uses closures, fns, loop, let, or let-macro. For some forms like
let or loop, the alias just creates a new local variable in which to store the
data. In other cases, closures are created. To handle all these cases, we have
a base AAlias class which provides basic single-linked list abilites. This will
allow us to override what certain symbols resolve to.

For instance:

(fn bar [a b]
    (let [b (inc b)
          z 1]
        (let-macro [a (fn [fdecl& env& decl] 'z)]
            (let [o (fn [a] a)]
                 [a o b]))))

As each new local is created, it is pushed onto the stack, then only the
top most local is executed whenever a new local is resolved. This allows
the above example to resolve exactly as desired. lets will never stop on
top of eachother, let-macros can turn 'x into (.-x self), etc.




"""

class AAlias():
    """Base class for all aliases"""
    def __init__(self, rest = None):
        self.rest = rest
    def compile(self, comp):
        raise AbstractMethodCall(self)
    def compileSet(self, comp):
        raise AbstractMethodCall(self)
    def next(self):
        return self.rest

class FnArgument(AAlias):
    """An alias provided by the arguments to a fn*
       in the fragment (fn [a] a) a is a FnArgument"""
    def __init__(self, sym, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
    def compile(self, comp):
        return [(LOAD_FAST, self.sym.name)]
    def compileSet(self, comp):
        return [(STORE_FAST, self.sym.name)]

class RenamedLocal(AAlias):
    """An alias created by a let, loop, etc."""
    def __init__(self, sym, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
        self.newsym = symbol(sym.name + str(RT.nextID()))
    def compile(self, comp):
        return [(LOAD_FAST, self.newsym.name)]
    def compileSet(self, comp):
        return [(STORE_FAST, self.newsym.name)]

class Closure(AAlias):
    """Represents a value that is contained in a closure"""
    def __init__(self, sym, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
        self.isused = False  ## will be set to true whenever this is compiled
    def isUsed(self):
        return self.isused
    def compile(self, comp):
        self.isused = True
        return [(LOAD_DEREF, self.sym.name)]
    def compileSet(self, comp):
        return [(STORE_DEREF, self.sym.name)]


class LocalMacro(AAlias):
    """represents a value that represents a local macro"""
    def __init__(self, sym, macroform, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
        self.macroform = macroform
    def compile(self, comp):
        code = comp.compile(self.macroform)
        return code
        
class SelfReference(AAlias):
    def __init__(self, var, rest = None):
        AAlias.__init__(self, rest)
        self.var = var
        self.isused = False
    def compile(self, comp):
        self.isused = True
        return [(LOAD_CONST, self.var),
                (LOAD_ATTR, "deref"),
                (CALL_FUNCTION, 0)]

class Name(object):
    """Slot for a name"""
    def __init__(self, name, rest=None):
        self.name = name
        self.isused = False
        self.rest = rest
    
    def __str__(self):
        v = []
        r = self
        while r is not None:
            v.append(r.name)
            r = r.rest
        v.reverse()
        s = "_".join(v)
        if self.isused:
            s = s + str(RT.nextID())
        return s


def evalForm(form, ns):
    comp = Compiler()
    comp.setNS(ns)
    code = comp.compile(form)
    return comp.executeCode(code)
    
    
def ismacro(macro):
    if not isinstance(macro, type) \
            and (hasattr(macro, "meta") and macro.meta() and macro.meta()[_MACRO_])\
            or (hasattr(macro, "macro?") and getattr(macro, "macro?")):
            return True
            
    return False
        
def macroexpand(form, comp, one = False):
    if isinstance(form.first(), Symbol):
        if form.first().ns == 'py' or form.first().ns == "py.bytecode":
            return form, False
       
        itm = findItem(comp.getNS(), form.first())
        dreffed = itm
        if isinstance(dreffed, Var):
            dreffed = itm.deref()
            
        
        # Handle macros here
        # TODO: Break this out into a seperate function
        if ismacro(itm) or ismacro(dreffed):
            macro = dreffed
            args = RT.seqToTuple(form.next())
            
            macroform = macro
            if hasattr(macro, "_macro-form"):
                macroform = getattr(macro, "_macro-form")

            mresult = macro(macroform, None, *args)
            
            if hasattr(mresult, "withMeta") \
               and hasattr(form, "meta"):
                mresult = mresult.withMeta(form.meta())
            mresult = comp.compile(mresult)   
            return mresult, True

    return form, False
    
class Compiler():
    def __init__(self):
        self.recurPoint = RT.list()
        self.names = None
        self.ns = None
        self.lastlineno = -1
        self.aliases = {}
        self.filename = "<unknown>"

    def setFile(self, filename):
        self.filename = filename

    def pushAlias(self, sym, alias):
        """ Pushes this alias onto the alias stack for the entry sym.
            if no entry is found, a new one is created """
        if sym in self.aliases:
            alias.rest = self.aliases[sym]
            self.aliases[sym] = alias
        else:
            self.aliases[sym] = alias

    def getAlias(self, sym):
        """ Retreives to top alias for this entry """
        if sym in self.aliases:
            return self.aliases[sym]
        return None

    def popAlias(self, sym):
        """ Removes the top alias for this entry. If the entry would be
            empty after this pop, the entry is deleted """
        if sym in self.aliases and self.aliases[sym].rest is None:
            del self.aliases[sym]
            return
        self.aliases[sym] = self.aliases[sym].rest
        return

    def popAliases(self, syms):
        for x in syms:
            self.popAlias(x)

    def pushRecur(self, label):
        """ Pushes a new recursion label. All recur calls will loop back to this point """
        self.recurPoint = RT.cons(label, self.recurPoint)
    def popRecur(self):
        """ Pops the top most recursion point """
        self.recurPoint = self.recurPoint.next()

    def pushName(self, name):
        if self.names is None:
            self.names = Name(name)
        else:
            self.names = Name(name, self.names)

    def popName(self):
        self.names = self.names.rest

    def getNamesString(self, markused=True):
        if self.names is None:
            return "fn_"+str(RT.nextID())
        s = str(self.names)
        if markused and self.names is not None:
            self.names.isused = True
        return s

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

    def compilePropertyAccess(self, form):
        attrname = form.first().name[2:]
        if len(form) != 2:
            raise CompilerException("Property access must have at only one argument", form)
        c = self.compile(form.next().first())
        c.append((LOAD_ATTR, attrname))
        return c

    def compileForm(self, form):
        if form.first() in builtins:
            return builtins[form.first()](self, form)
        form, ret = macroexpand(form, self)
        if ret:
            return form
        if isinstance(form.first(), Symbol):
            if form.first().ns == "py.bytecode":
                return compileBytecode(self, form)
            if form.first().name.startswith(".-"):
                return self.compilePropertyAccess(form)
            if form.first().name.startswith(".") and form.first().ns is None:
                return self.compileMethodAccess(form)
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
        if sym.ns == 'py':
            return [(LOAD_CONST, getBuiltin(sym.name))]

        code = self.getAccessCode(sym)
        return code

    def getAccessCode(self, sym):
        if (sym.ns is not None and sym.ns == self.getNS().__name__) \
           or sym.ns is None:
            if not hasattr(self.getNS(), sym.name):
                raise CompilerException("could not resolve " + str(sym) + " " \
                                        + sym.name + " not found in " + self.getNS().__name__, sym)
            var = getattr(self.getNS(), sym.name)
            if isinstance(var, Var):
                return [(LOAD_CONST, var),
                        (LOAD_ATTR, "deref"),
                        (CALL_FUNCTION, 0)]
            return [(LOAD_GLOBAL, sym.name)]
                
        splt = []
        if sym.ns is not None:
            module = findNamespace(sym.ns) 
            if not hasattr(module, sym.name):
                raise CompilerException(str(module) + " does not define " + sym.name, None)
            attr = getattr(module, sym.name)
            if isinstance(attr, Var):
                splt.append((LOAD_CONST, attr))
                splt.append((LOAD_ATTR, "deref"))
                splt.append((CALL_FUNCTION, 0))
            else:
                splt.append((LOAD_CONST, module))
                splt.append((LOAD_ATTR, sym.name))
            return splt
            
        code = LOAD_ATTR if sym.ns else LOAD_GLOBAL
        if not sym.ns and sym.name.find(".") != -1 and sym.name != "..":
            raise CompilerException("unqualified dotted forms not supported: " + str(sym), sym)
        
        if len(sym.name.replace(".", "")):
            splt.extend((code, attr) for attr in sym.name.split("."))
        else:
            splt.append((code, sym.name))
        return splt

    def compileSymbol(self, sym):
        """ Compiles the symbol. First the compiler tries to compile it
            as an alias, then as a global """

        if sym in self.aliases:
            return self.compileAlias(sym)

        return self.compileAccessList(sym)

    def compileAlias(self, sym):
        """ Compiles the given symbol as an alias. """
        alias = self.getAlias(sym)
        if alias is None:
            raise CompilerException("Unknown Local " + str(sym))
        return alias.compile(self)

    def closureList(self):
        closures = []
        for x in self.aliases:
            alias = self.aliases[x]
            if isinstance(alias, Closure) and alias.isUsed():
                closures.append(alias)
        return closures

    def compile(self, itm):
        from clojure.lang.persistentlist import PersistentList, EmptyList
        from clojure.lang.cons import Cons
        c = []
        lineset = False
        if hasattr(itm, "meta") and itm.meta() is not None:
            line = itm.meta()[LINE_KEY]
            if line is not None and line > self.lastlineno:
                lineset = True
                self.lastlineno = line
                c.append([SetLineno, line])

        if isinstance(itm, Symbol):
            c.extend(self.compileSymbol(itm))
        elif isinstance(itm, PersistentList) or isinstance(itm, Cons):
            c.extend(self.compileForm(itm))
        elif itm is None:
            c.extend(self.compileNone(itm))
        elif type(itm) in [str, int, new.classobj, type]:
            c.extend([(LOAD_CONST, itm)])
        elif isinstance(itm, IPersistentVector):
            c.extend(compileVector(self, itm))
        elif isinstance(itm, IPersistentMap):
            c.extend(compileMap(self, itm))
        elif isinstance(itm, Keyword):
            c.extend(compileKeyword(self, itm))
        elif isinstance(itm, bool):
            c.extend(compileBool(self, itm))
        elif isinstance(itm, EmptyList):
            c.append((LOAD_CONST, itm))
        elif isinstance(itm, unicode):
            c.append((LOAD_CONST, itm))
        else:
            raise CompilerException("Don't know how to compile" + str(type(itm)), None)

        if len(c) < 2 and lineset:
            return []
        return c


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
        c = Code(newcode, [], [], False, False, False, str(symbol(self.getNS().__name__, "<string>")), self.filename, 0, None)
        retval = eval(c.to_code(), self.getNS().__dict__)
        return retval

    def pushPropertyAlias(self, mappings):
        locals = {}
        for x in mappings:
            if x in self.aliasedProperties:
                self.aliasedProperties[x].append(mappings[x])
            else:
                self.aliasedProperties[x] = [mappings[x]]

    def popPropertyAlias(self, mappings):
        dellist = []
        for x in mappings:
            self.aliasedProperties[x].pop()
            if not len(self.aliasedProperties[x]):
                dellist.append(x)
        for x in dellist:
            del self.aliasedProperties[x]



    def standardImports(self):
        return [(LOAD_CONST, -1),
            (LOAD_CONST, None),
            (IMPORT_NAME, "clojure.standardimports"),
            (IMPORT_STAR, None)]

    def executeModule(self, code):
        code.append((RETURN_VALUE, None))
        c = Code(code, [], [], False, False, False, str(symbol(self.getNS().__name__, "<string>")), self.filename, 0, None)
        import marshal
        import pickle
        import py_compile
        import time
        import dis

        dis.dis(c)
        codeobject = c.to_code()

        with open('output.pyc', 'wb') as fc:
            fc.write(py_compile.MAGIC)
            py_compile.wr_long(fc, long(time.time()))
            marshal.dump(c, fc)

