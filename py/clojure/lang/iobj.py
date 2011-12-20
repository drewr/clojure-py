from exceptions import AbstractMethodCall

class IObj(object):
    def withMeta(self, meta):
        raise AbstractMethodCall()