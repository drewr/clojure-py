import unittest

from py.clojure.lang.persistenttreemap import PersistentTreeMap


class PresistentTreeMapTests(unittest.TestCase):
    def testAssoc(self):
        m = PersistentTreeMap()
        m2 = m.assoc('a', 1)
        self.assertTrue(m2.containsKey('a'))
