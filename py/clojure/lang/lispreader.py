from py.clojure.lang.fileseq import FileSeq, MutatableFileSeq
from var import Var
from py.clojure.lang.cljexceptions import ReaderException, IllegalStateException
#from py.clojure.lang.gmp import Integer, Rational, Float
import py.clojure.lang.rt as RT
from py.clojure.lang.cljkeyword import LINE_KEY
from py.clojure.lang.symbol import Symbol
import re

def read1(rdr):
    rdr.next()
    if rdr is None:
        return ""
    return rdr.first()

_AMP_ = Symbol.intern("&")
_FN_ = Symbol.intern("fn")
_VAR_ = Symbol.intern("var")
_QUOTE_ = Symbol.intern("quote")

ARG_ENV = Var.create(None).setDynamic()

WHITESPACE = [',', '\n', '\t', '\r', ' ']

symbolPat = re.compile("[:]?([\\D^/].*/)?([\\D^/][^/]*)")
intPat = re.compile("([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
ratioPat = re.compile("([-+]?[0-9]+)/([0-9]+)")
floatPat = re.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

def isWhitespace(c):
    return c in WHITESPACE

def isMacro(c):
    return c in macros

def isTerminatingMacro(ch):
    return ch != "#" and ch != "\'" and isMacro(ch)

def forIter(start, whileexpr, next):
    cur = start
    while whileexpr(cur):
        yield cur
        cur = next()

DIGITS = "0123456789abcdefghijklmnopqrstuvwxyz"
def digit(d, base = 10):

    idx = DIGITS.index(d)
    if idx == -1 or idx >= base:
        return -1
    return idx

def isDigit(d):
    return d in DIGITS and DIGITS.index(d) < 10



chrLiterals = {'t': '\t',
               'r': '\r',
               'n': '\n',
               'b': '\b',
               '\\': '\\',
               '"': '"',
               "f": '\f'}

def read(rdr, eofIsError, eofValue, isRecursive):
    while True:
        ch = read1(rdr)

        while isWhitespace(ch):
            ch = read1(rdr)

        if ch == "":
            raise ReaderException("EOF while reading", rdr)

        if isDigit(ch):
            return readNumber(rdr, ch)

        m = getMacro(ch)
        if m is not None:
            ret = m(rdr, ch)
            if ret is rdr:
                continue
            return ret

        if ch in ["+", "-"]:
            ch2 = read1(rdr)
            if isDigit(ch2):
                rdr.back()
                n = readNumber(rdr, ch)
                return n
            rdr.back()

        token = readToken(rdr, ch)
        return interpretToken(token)


def stringReader(rdr, doublequote):
    sb = []
    ch = read1(rdr)
    while ch != '\"':
        if ch == "":
            raise ReaderException("EOF while reading string")
        if ch == "\\":
            ch = read1(rdr)
            if ch in chrLiterals:
                ch = chrLiterals[ch]
            elif ch == 'u':
                ch = read1(rdr)
                if digit(ch, 16) == -1:
                    raise ReaderException("Invalid unicode escape: \\u" + ch)
                ch = readUnicodeChar(rdr, ch, 16, 4, True)
            else:
                if digit(ch, 8) == -1:
                    raise ReaderException("Unsupported escape character: \\" + ch)
                ch = readUnicodeChar(rdr, ch, 8, 4, True)
                if ch > 0377:
                    raise ReaderException("Octal escape sequence must be in range [0, 377].")
        sb.append(ch)
        ch = read1(rdr)

    return "".join(sb)

def readToken(rdr, initch):
    sb = [initch]
    while True:
        ch = read1(rdr)
        if ch == "" or isWhitespace(ch) or isTerminatingMacro(ch):
            rdr.back()
            break
        sb.append(ch)

    s = "".join(sb)

    return s

INTERPRET_TOKENS = {"nil": None,
                    "true": True,
                    "false": False,
                    }
def interpretToken(s):
    if s in INTERPRET_TOKENS:
        return INTERPRET_TOKENS[s]
    if s == ":added":
        pass
    ret = matchSymbol(s)
    if ret is None:
        if s == ":added":
            pass
        raise ReaderException("Unknown symbol " + str(s))
    return ret


def readNumber(rdr, initch):
    sb = [initch]
    while True:
        ch = read1(rdr)
        if ch == "" or isWhitespace(ch) or isMacro(ch):
            rdr.back()
            break
        sb.append(ch)

    s = "".join(sb)
    n = matchNumber(s)
    if n is None:
        raise ReaderException("Invalid number: " + s)
    return n

def matchNumber(s):
    return int(float(s))
    m = intPat.match(s)
    if m is not None:
        #TODO add radix
        #i = Integer()
        #i.set(s)
        return int(str(float(s)))
    m = floatPat.match(s)
    if m is not None:
        f = Float()
        f.set(s)
        return f
    return None

def getMacro(ch):
    return macros[ch] if ch in macros else None

def commentReader(rdr, semicolon):
    while True:
        chr = read1(rdr)
        if chr == -1 or chr == '\n' or chr == '\r':
            break
    return rdr

def discardReader(rdr, underscore):
    read(rdr, True, None, True)
    return rdr


class wrappingReader():
    def __init__(self, sym):
        self.sym = sym
    def __call__(self, rdr, quote):
        o = read(rdr, True, None, True)
        return RT.list(self.sym, o)

def varReader():
    return wrappingReader(THE_VAR)

def dispatchReader(rdr, hash):
    ch = read1(rdr)
    if ch == "":
        raise ReaderException("EOF while reading character")

    if ch not in dispatchMacros:
        #rdr.back()
        #result = fn(rdr, ch)
        #if result is not None:
        #    return result
        #else:
        #    raise ReaderException("No dispatch macro for: "+ ch)
        raise ReaderException("No dispatch macro for: ("+ ch + ")")
    return dispatchMacros[ch](rdr, ch)

def listReader(rdr, leftparen):
    startline = rdr.lineCol()[0]
    lst = readDelimitedList(')', rdr, True)
    lst = apply(RT.list, lst)
    return lst.withMeta(RT.map(LINE_KEY, startline))

def vectorReader(rdr, leftbracket):
    startline = rdr.lineCol()[0]
    lst = readDelimitedList(']', rdr, True)
    lst = apply(RT.vector, lst)
    return lst

def mapReader(rdr, leftbrace):
    startline = rdr.lineCol()[0]
    lst = readDelimitedList('}', rdr, True)
    lst = apply(RT.map, lst)
    return lst

def unmatchedDelimiterReader(rdr, un):
    raise ReaderException("Unmatched Delimiter " + un + "at " + str(rdr.lineCol()))

def readDelimitedList(delim, rdr, isRecursive):
    firstline = rdr.lineCol()[0]

    a = []

    while True:
        ch = read1(rdr)
        while isWhitespace(ch):
            ch = read1(rdr)
        if ch == "":
            raise ReaderException("EOF while reading starting at line " + str(firstline))

        if ch == delim:
            break

        macrofn = getMacro(ch)
        if macrofn is not None:
            mret = macrofn(rdr, ch)
            if mret is not None:
                a.append(mret)
        else:
            rdr.back()
            o = read(rdr, True, None, isRecursive)
            a.append(o)

    return a

def regexReader(rdr, doubleQuote):
    s = []
    ch = -1
    while ch != '\"':
        ch = read1(rdr)
        if ch == "":
            raise ReaderException("EOF while reading string", rdr)
        s.append(ch)
        if ch == "\\":
            ch = read1(rdr)
            if ch == "":
                raise ReaderException("EOF while reading regex", rdr)
            s.append(ch)

    return re.compile("".join(s))

def metaReader(rdr, caret):
    from py.clojure.lang.symbol import Symbol
    from py.clojure.lang.cljkeyword import Keyword, TAG_KEY, T
    from py.clojure.lang.ipersistentmap import IPersistentMap
    line = rdr.lineCol()[0]
    meta = read(rdr, True, None, True)
    if isinstance(meta, Symbol) or isinstance(meta, str):
        meta = RT.map(TAG_KEY, meta)
    elif isinstance(meta, Keyword):
        meta = RT.map(meta, T)
    elif not isinstance(meta, IPersistentMap):
        raise ReaderException("Metadata must be Symbol,Keyword,String or Map")
    o = read(rdr, True, None, True)
    if not hasattr(o, "withMeta"):
        raise ReaderException("Cannot attach meta to a object without .withMeta")
    return o.withMeta(meta)

def matchSymbol(s):
    from py.clojure.lang.symbol import Symbol
    from py.clojure.lang.cljkeyword import Keyword
    m = symbolPat.match(s)
    if m is not None:
        ns = m.group(1)
        name = m.group(2)
        if ns is not None and ns.endswith(":/") or name.endswith(":") \
            or s.find("::") != -1:
            return None
        if s.startswith("::"):
            return "FIX"
        iskeyword = s.find(':') == 0
        sym = Symbol.intern(s[(1 if iskeyword else 0):])
        if iskeyword:
            return Keyword.intern(s)
        else:
            return sym
    return None

def setReader(rdr, leftbrace):
    from persistenthashset import PersistentHashSet
    return PersistentHashSet.create(readDelimitedList("}", rdr,  True))

def argReader(rdr, perc):
    if ARG_ENV.deref() is None:
        return interpretToken(readToken(rdr, '%'))
    ch = read1(rdr)
    rdr.back()
    if ch == "" or isWhitespace(ch) or isTerminatingMacro(ch):
        return registerArg(1)
    n = read(rdr, True, None, True)
    if isinstance(n, Symbol) and n == _AMP_:
        return registerArg(-1)
    if not isinstance(n, int):
        raise IllegalStateException("arg literal must be %, %& or %integer")
    return registerArg(n)

def varQuoteReader(rdr, singlequote):
    line = rdr.lineCol()[0]
    form = read(rdr, True, None, True)
    return RT.list(_VAR_, form).withMeta(RT.map(LINE_KEY, line))

def registerArg(arg):
    argsyms = ARG_ENV.deref()
    if argsyms is None:
        raise IllegalStateException("arg literal not in #()")
    ret = argsyms[arg]
    if ret is None:
        ret = garg(arg)
        ARG_ENV.set(argsyms.assoc(arg, ret))
    return ret

def fnReader(rdr, lparen):
    from py.clojure.lang.persistenthashmap import EMPTY
    from py.clojure.lang.var import popThreadBindings, pushThreadBindings

    if ARG_ENV.deref() is not None:
        raise IllegalStateException("Nested #()s are not allowed")
    #try:
    pushThreadBindings(RT.map(ARG_ENV, EMPTY))
    rdr.back()
    form = read(rdr, True, None, True)
    drefed = ARG_ENV.deref()
    sargs = sorted(list(filter(lambda x: x != -1, drefed)))
    args = []
    if len(sargs):
        for x in range(int(str(sargs[-1]))):
            if x in drefed:
                args.append(drefed[x])
            else:
                args.append(garg(x))
        retsym = drefed[-1]
        if retsym is not None:
            args.append(_AMP_)
            args.append(retsym)

    vargs = RT.vector(args)
    popThreadBindings()
    return RT.list(_FN_, vargs, form)
    #finally:






def garg(n):
    from symbol import Symbol
    return Symbol.intern(None,  "rest" if n == -1 else  ("p" + str(n)) + "__" + str(RT.nextID()) + "#")

macros = {'\"': stringReader,
          "\'": wrappingReader(_QUOTE_),
          "(": listReader,
          ")": unmatchedDelimiterReader,
          "[": vectorReader,
          "]": unmatchedDelimiterReader,
          "{": mapReader,
          "}": unmatchedDelimiterReader,
          ";": commentReader,
          "#": dispatchReader,
          "^": metaReader,
          "%": argReader}

dispatchMacros = {"\"": regexReader,
                  "{": setReader,
                  "!": commentReader,
                  "_": discardReader,
                  "(": fnReader,
                  "'": varQuoteReader,
                  "^": metaReader}


if __name__ == '__main__':
    from StringIO import StringIO
    from fileseq import StringReader
    def rdr(s):
        #return MutatableFileSeq(FileSeq(StringIO(s)))
        return StringReader(s)

    fl = open("/home/tim/core.clj")
    s = fl.read()
    fl.close()
    import sys
    sys.path = ["."] + sys.path

    r = rdr(" "+s)
    try:
        while r.fs:
            s = read(rdr, True, None, True)
            #print '-' + repr(s) + '-'
    except IOError:
        pass

