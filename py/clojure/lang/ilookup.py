from exceptions import AbstractMethodCall

class ILookup(object):
    def valAt(self, key, notFound = None):
        raise AbstractMethodCall()


