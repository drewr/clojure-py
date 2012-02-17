import unittest

from py.clojure.lang.persistenttreemap import PersistentTreeMap


class PresistentTreeMapTests(unittest.TestCase):
    def test_assoc(self):
        m = PersistentTreeMap()
        m.assoc('a', 1)
        self.assertTrue(m.containsKey('a'))
