from exceptions import AbstractMethodCall

class IHashEq(object):
    def hasheq(self):
        raise AbstractMethodCall()