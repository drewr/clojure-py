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
try:
    while True:
        c += 1
        oldl = r.lineCol()
        s = read(r, True, None, True)
        try:
            print s
            print c
            res = comp.compile(s)

            comp.executeCode(res)
            code.extend(res)
        except IOError as exp:
            print s
            raise exp

        #if c > 0:
        #    break

        while True:
            ch = r.read()
            if ch == "":
                raise IOError()
            if ch not in [" ", "\t", "\n", "\r"]:
                r.back()
                break
except IOError as e:
    pass

#comp.executeModule(code)


while(True):
    line = raw_input(comp.getNS().__name__ + "=>")
    r = rdr(line)
    s = read(r, True, None, True)
    res = comp.compile(s)
    print comp.executeCode(res)

print x

