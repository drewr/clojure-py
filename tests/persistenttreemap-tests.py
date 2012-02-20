from random import randint, shuffle
import unittest

from py.clojure.lang.persistenttreemap import PersistentTreeMap


class PersistentTreeMapTests(unittest.TestCase):
    def testScaling(self):
        m = PersistentTreeMap()
        ints = range(1000)
        shuffle(ints)
        for i in ints:
            m = m.assoc(i, randint(1, 10))
        self.assertEqual(m.count(), 1000)

    def testAssoc(self):
        m = PersistentTreeMap().assoc('a', 1)
        self.assertTrue(m.containsKey('a'))

    def testWithout(self):
        m = PersistentTreeMap().assoc('a', 1).without('a')
        self.assertFalse(m.containsKey('a'))

    def testSeq(self):
        self.assertEqual(PersistentTreeMap().seq(), None)

        m = PersistentTreeMap().assoc('a', 1).assoc('b', 2)
        self.assertEqual(m.seq().first().key(), 'a')
        self.assertEqual(m.seq().next().first().key(), 'b')

    def testKeys(self):
        m = PersistentTreeMap().assoc('a', 1).assoc('b', 2)
        keys = m.keys()
        self.assertEqual(keys.next(), 'a')
        self.assertEqual(keys.next(), 'b')

    def testVals(self):
        m = PersistentTreeMap().assoc('a', 1).assoc('b', 2)
        vals = m.vals()
        self.assertEqual(vals.next(), 1)
        self.assertEqual(vals.next(), 2)

    def testMinKey(self):
        m = PersistentTreeMap().assoc('a', 1).assoc('b', 2)
        self.assertEqual(m.minKey(), 'a')

    def testMaxKey(self):
        m = PersistentTreeMap().assoc('a', 1).assoc('b', 2)
        self.assertEqual(m.maxKey(), 'b')

    def testValAt(self):
        m = PersistentTreeMap().assoc('a', 1)
        self.assertEqual(m.valAt('a'), 1)
