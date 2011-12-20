from exceptions import AbstractMethodCall
from imeta import IMeta

class IReference(object, IMeta):
    def alterMeta(self, fn, args):
        raise AbstractMethodCall()
    def resetMeta(self, meta):
        raise AbstractMethodCall();