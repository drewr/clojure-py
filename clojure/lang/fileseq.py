from clojure.lang.aseq import ASeq
from clojure.lang.cljexceptions import IllegalAccessError, ArityException, InvalidArgumentException

def isReader(rdr):
    if not hasattr(rdr, "read"):
        return False
    if not hasattr(rdr, "tell"):
        return False
    return True


class FileSeq(ASeq):
    def __init__(self, *args):
        if len(args) == 1:
            if not isReader(args[0]):
                raise InvalidArgumentException("must pass in a object with a read() method")
            FileSeq.__init__(self, args[0], 1, 1, args[0].read(1))
            return
        elif len(args) == 4:
            self.rdr, self.line, self.col, self.ccur = args
            self._next = None

    def first(self):
        return self.ccur

    def reuseNext(self, nxt):
        c = self.rdr.read(1)
        if c == "":
            return None

        newline = self.line + 1 if c == '\n' else self.line
        newcol = 1 if newline != self.line else self.col + 1

        nxt.rdr = self.rdr
        nxt.line = newline
        nxt.col = newcol
        nxt.ccur = c
        nxt._next = None
        return nxt

    def next(self):
        if self._next is not None:
            return self._next
        c = self.rdr.read(1)
        if c == "":
            return None

        newline = self.line + 1 if c == '\n' else self.line
        newcol = 1 if newline != self.line else self.col + 1

        # Nasty mutation here please don't use this
        # with threads
        self._next = FileSeq(self.rdr, newline, newcol, c)
        return self._next

    def lineCol(self):
        return [self.line, self.col]

    def tell(self):
        return self.rdr.tell()

    def atLineStart(self):
        return self.col == 1

    def atLineEnd(self):
        nxt = self.next()
        return True if nxt is None else nxt.atLineStart()

    def __eq__(self, other):
        if isinstance(other, str):
            return self.ccur == other
        if other is None:
            return False
        if self is other:
            return True
        if self.rdr is other.rdr and \
           self.tell() == other.tell():
            return True
        return False


class MutatableFileSeq(ASeq):
    def __init__(self, fs):
        self.fs = fs
        self.old = None
        self.d = dir

    def first(self):
        return self.fs.first()

    def next(self):
        o = self.old
        self.old = self.fs
        if o is not None:
            ret = self.fs.reuseNext(o)
        else:
            ret = self.fs.next()
        self.fs = ret
        return ret

    def back(self):
        if self.old is None:
            raise InvalidArgumentException("Can only go back once")
        self.fs = self.old
        self.old = None

    def lineCol(self):
        return self.fs.lineCol() if self.fs is not None else [None, None]


class StringReader(object):
    def __init__(self, s):
        self.line = 1;
        self.col = 1
        self.idx = -1
        self.s = s
        self.lastline = -1
        self.lastcol = -1
        self.haslast = False

    def read(self):
        self.lastcol = self.col
        self.lastline = self.line
        self.haslast = True
        self.idx += 1
        if self.idx >= len(self.s):
            return ""

        cc = self.s[self.idx]
        if cc == '\n':
            self.line += 1;
            self.col = 1
        return cc

    def next(self):
        self.read()
        return self

    def first(self):
        return self.s[self.idx] if self.idx < len(self.s) else ""

    def lineCol(self):
        return [self.line, self.col]

    def back(self):
        if not self.haslast:
            raise IllegalAccessError()
        self.idx -= 1
        self.haslast = False
        self.line = self.lastline
        self.col = self.lastcol
