from exceptions import AbstractMethodCall
from itransientassociative import ITransientAssociative

class ITransientMap(ITransientAssociative, Counted):
    def assoc(self, key, value):
        raise AbstractMethodCall()
    def without(self, key):
        raise AbstractMethodCall()
    def persistent(self):
        raise AbstractMethodCall()

