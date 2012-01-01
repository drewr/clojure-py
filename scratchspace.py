from StringIO import StringIO
from py.clojure.lang.lispreader import *
from py.clojure.lang.fileseq import StringReader

def rdr(s):
    return StringReader(s)


fl = open("/home/tim/core.clj")
data = fl.read()
fl.close()
import sys
sys.path = ["."] + sys.path

c = 0
for x in range(1):
    r = rdr("["+data +"]")
    try:
        while True:
            c += 1
            s = read(r, True, None, True)
            print '-' ,len(s), '-'
    except IOError:
        print "error"
    print x

