from associative import Associative
from sequential import Sequential
from ipersistentstack import IPersistentStack
from reversible import Reversible
from indexed import Indexed
from cljexceptions import AbstractMethodCall


class IPersistentVector(Associative, Sequential, IPersistentStack, Reversible, Indexed):
    def __len__(self):
        raise AbstractMethodCall()
    def assocN(self, i, val):
        raise AbstractMethodCall()
    def cons(self, o):
        raise AbstractMethodCall()