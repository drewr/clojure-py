from cljexceptions import AbstractMethodCall

class IHashEq(object):
    def hasheq(self):
        raise AbstractMethodCall()