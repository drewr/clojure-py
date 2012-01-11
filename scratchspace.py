from StringIO import StringIO
from py.clojure.lang.lispreader import *
from py.clojure.lang.fileseq import StringReader
import py.clojure.lang.rt as RT

def rdr(s):
    return StringReader(s)


fl = open("./clj/clojure/core.clj")
data = fl.read()
fl.close()
import sys
sys.path = ["."] + sys.path

c = 0
from py.clojure.lang.compiler import Compiler
RT.init()
comp = Compiler()
from py.clojure.util.byteplay import PRINT_ITEM
r = rdr(data)
try:
    while True:
        c += 1
        oldl = r.lineCol()
        s = read(r, True, None, True)
        print s
        res = comp.compileForm(s)
        print comp.executeCode(res)
        print res

        while True:
            ch = r.read()
            if ch == "":
                raise IOError()
            if ch not in [" ", "\t", "\n", "\r"]:
                r.back()
                break;


        #print '-' ,len(s), '-', oldl, r.lineCol()

except IOError:
    print "error"

while(True):
    line = raw_input(comp.getNS().__name__ + "=>")
    r = rdr(line)
    s = read(r, True, None, True)
    res = comp.compileForm(s)
    comp.executeCode(res)

print x

