from fileseq import FileSeq, MutatableFileSeq
from cljexceptions import ReaderException
from gmp import Integer, Rational, Float
import rt as RT
import re

def read1(rdr):
    rdr.next()
    if rdr.fs == None:
        return ""
    return rdr.first()

WHITESPACE = [',', '\n', '\t', '\r', ' ']

symbolPat = re.compile("[:]?([\\D&&[^/]].*/)?([\\D&&[^/]][^/]*)")
intPat = re.compile("([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
ratioPat = re.compile("([-+]?[0-9]+)/([0-9]+)")
floatPat = re.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

def isWhitespace(chr):
    return chr in WHITESPACE

def isMacro(chr):
    return chr in macros

def isTerminatingMacro(chr):
    return ch != "#" and ch != "\'" and isMacro(chr)

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
    return d in DIGITS


macros = {}

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
            raise ReaderException("EOF while reading")

        if isDigit(ch):
            return readNumber(rdr, ch)

        m = getMacro(ch)
        if m is not None:
            ret = m(rdr, ch)
            if ret is rdr:
                continue
            return ret

        if ch in ["+", "-"]:
            ch2 = read(rdr)
            if isDigit(ch2):
                unread(rdr, ch2)
                n = readNumber(rdr, ch)
                return n
            unread(rdr, ch2)

        token = readToken(r, ch)
        return interpretToken(token)


def stringReader(rdr, doublequote):
    sb = []
    ch = rdr.first()
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
                ch = readUnicodeChar(r, ch, 16, 4, True)
            else:
                if digit(ch, 8) == -1:
                    raise ReaderException("Unsupported escape character: \\" + ch)
                ch = readUnicodeChar(r, ch, 8, 4, True)
                if ch > 0377:
                    raise ReaderException("Octal escape sequence must be in range [0, 377].")
        sb.append(ch)
        ch = read1(rdr)

    return "".join(sb)

def readToken(rdr, initch):
    sb = [initch]
    while True:
        ch = read1(rdr)
        if ch == "" or isWhitespace(ch) or isMacro(ch):
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
    return s


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
    m = intPat.match(s)
    if m is not None:
        #TODO add radix
        i = Integer()
        i.set(s)
        return i
    m = floatPat.match(s)
    if m is not None:
        f = Float()
        f.set(s)
        return f
    return None

def commentReader(rdr, semicolon):
    while True:
        chr = read1(rdr)
        if chr == -1 or chr == '\n' or chr == '\r':
            break
    return r

def discardReader(rdr, underscore):
    read(rdr, True, None, True)
    return rdr


class wrappingReader():
    def __init__(self, sym):
        self.sym = sym
    def __call__(self, rdr, quote):
        o = read(r, True, None, True)
        return RT.list(self.sym, o)

def varReader():
    return wrappingReader(THE_VAR)

def dispatchReader(rdr, hash):
    ch = read1(rdr)
    if ch == "":
        raise ReaderException("EOF while reading character")

    if ch not in dispatchMacros:
        fn = dispatchMacros[ch]
        rdr.back()
        result = fn(rdr, ch)
        if result is not None:
            return result
        else:
            raise ReaderException("No dispatch macro for: "+ ch)
    return dispatchMacros[ch](rdr, ch)

def listReader(rdr, leftparen):
    startline = rdr.lineCol()[0]
    lst = readDelimitedList(')', rdr, True)
    lst = apply(RT.list, lst)
    return lst.withMeta(RT.map(RT.LINE_KEY, startline))

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

    return a



if __name__ == '__main__':
    from StringIO import StringIO
    def rdr(s):
        return MutatableFileSeq(FileSeq(StringIO(s)))

    ## stringReader
    print "running tests..."
    tst = '\\"This is \\t\\n\\ra test\\"'
    #tst = "\"12\""
    r = rdr(tst)
    s = stringReader(r, read1(r))
    print s
    #assert(s == "\"This is \t\n\ra test\"")


    ## readNumber
    tst = '110'
    r = rdr(tst)
    s = readNumber(r, r.first())
    print s
    assert(s == 110)

    tst = "(a True False (1 2 3))"
    r = rdr(tst)
    s = read(r, True, None, True)
    print s


