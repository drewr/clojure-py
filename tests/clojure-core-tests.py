from itertools import chain, repeat
import pprint
from textwrap import dedent
import unittest

from clojure import requireClj
from py.clojure.lang.compiler import Compiler
from py.clojure.lang.fileseq import StringReader
from py.clojure.lang.globals import currentCompiler
from py.clojure.lang.lispreader import read
import py.clojure.lang.rt as RT
from py.clojure.lang.symbol import Symbol
from py.clojure.util.byteplay import Code, Label, SetLineno


requireClj('./clj/clojure/core.clj')


class ClojureCoreTests(unittest.TestCase):
    def eval(self, code):
        r = StringReader(code)
        s = read(r, True, None, True)
        res = self.comp.compile(s)
        return self.comp.executeCode(res)

    def setUp(self):
        RT.init()
        self.comp = Compiler()
        currentCompiler.set(self.comp)
        self.comp.setNS(Symbol.intern('clojure.core'))

    def testList(self):
        self.assertEqual(self.eval('(list 1 2 3)'), [1, 2, 3])