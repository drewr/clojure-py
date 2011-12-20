from exceptions import AbstractMethodCall

class IReduce(object):
    def reduce(self, *args):
        raise AbstractMethodCall()