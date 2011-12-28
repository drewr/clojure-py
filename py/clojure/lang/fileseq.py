from aseq import ASeq
from cljexceptions import IllegalAccessError, ArityException, InvalidArgumentException

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
        if self.fs is None:
            return None
        ret = self.fs.next()
        self.old = self.fs
        self.fs = ret
        return ret

    def back(self):
        if self.old is None:
            raise InvalidArgumentException("Can only go back once")
        self.fs = self.old
        self.old = None

if __name__ == '__main__':
    print "running tests..."
    f = open("fileseq.py", "r")
    fstart = FileSeq(f)
    s = []
    for x in fstart:
        s.append(x.first())
        if x.atLineEnd():
            print x.lineCol() , "".join(s).strip("\n\r")
            s = []

    oldpos = f.tell()

    for x in fstart:
        s.append(x.first())
        if x.atLineEnd():
            print x.lineCol() , "".join(s).strip("\n\r")
            s = []

    m = MutatableFileSeq(fstart)
    m.next()
    print m.first(), fstart.next().first()
    assert(m.first() is fstart.next().first())

    assert(f.tell() == oldpos)

