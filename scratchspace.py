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

for x in range(1):
    #r = rdr("["+data +"]")
    r = rdr(data)
    try:
        for x in range(10):
            c += 1
            oldl = r.lineCol()
            s = read(r, True, None, True)
            print s
            res = comp.compileForm(s)
            comp.executeCode(res)
            print res

            #print '-' ,len(s), '-', oldl, r.lineCol()

    except IOError:
        print "error"
    print x

