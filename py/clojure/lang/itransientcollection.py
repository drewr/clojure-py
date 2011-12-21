from exceptions import AbstractMethodCall

class ITransientCollection(object):
    def conj(self, val):
        raise AbstractMethodCall()
    def persistent(self):
        raise AbstractMethodCall()