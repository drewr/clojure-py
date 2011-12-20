from exceptions import AbstractMethodCall
from associative import Associative
from interable import Interable
from counted import Counted

class IPersistentMap(Interable, Associative, Counted):
    def without(self, key):
        raise AbstractMethodCall()
