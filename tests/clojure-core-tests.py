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
from py.clojure.lang.persistentlist import PersistentList
from py.clojure.lang.persistentvector import PersistentVector

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
        self.assertEqual(self.eval('(list)'), [])
        self.assertIsInstance(self.eval('(list 1 2 3)'), PersistentList)

    def testVector(self):
        self.assertEqual(self.eval('(vector 1 2 3)'), [1, 2, 3])
        self.assertEqual(self.eval('(vector)'), [])
        self.assertIsInstance(self.eval('(vector 1 2 3)'), PersistentVector)

    def testCons(self):
        self.assertEqual(self.eval('(cons 1 nil)'), [1])
        self.assertEqual(self.eval('(cons 1 [2])'), [1, 2])
        self.assertEqual(self.eval('(cons 1 (cons 2 (cons 3 nil)))'), [1, 2, 3])

    def testSeq(self):
        self.assertEqual(self.eval('(seq [])'), None)
        self.assertEqual(self.eval('(seq nil)'), None)

    def testFirst(self):
        self.assertEqual(self.eval('(first [1 2])'), 1)
        self.assertEqual(self.eval('(first nil)'), None)

    def testNext(self):
        self.assertEqual(self.eval('(next [1 2])'), [2])
        self.assertEqual(self.eval('(next nil)'), None)

