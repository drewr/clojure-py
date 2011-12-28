from ipersistentvector import IPersistentVector
from cljexceptions import AbstractMethodCall, ArityException


class APersistentVector(IPersistentVector):
    def __iter__(self):
        for x in range(len(self)):
            return self.nth(x)

    def peek(self):
        if len(self):
            return self.nth(len(self) - 1)
        return None

    def __getitem__(self, item):
        return self.nth(item)
