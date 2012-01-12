from StringIO import StringIO
import sys
sys.path.append("/home/tim/clojure-py")
from py.clojure.lang.lispreader import *
from py.clojure.lang.fileseq import StringReader
import py.clojure.lang.rt as RT

def rdr(s):
    return StringReader(s)


fl = open("./clj/clojure/core.clj")
data = fl.read()
fl.close()

c = 0
from py.clojure.lang.compiler import Compiler
RT.init()
comp = Compiler()
from py.clojure.util.byteplay import PRINT_ITEM
r = rdr(data)
code = comp.standardImports()
while True:
    c += 1
    oldl = r.lineCol()
    s = read(r, True, None, True)
    try:
        res = comp.compile(s)
        code.extend(res)
    except IOError as exp:
        print s
        raise exp

    if c > 1:
        break

    while True:
        ch = r.read()
        if ch == "":
            raise IOError()
        if ch not in [" ", "\t", "\n", "\r"]:
            r.back()
            break

comp.executeModule(code)


while(True):
    line = raw_input(comp.getNS().__name__ + "=>")
    r = rdr(line)
    s = read(r, True, None, True)
    try:
        res = comp.compile(s)
    except Exception as exp:
        print s
        raise exp
    print comp.executeCode(res)

print x

