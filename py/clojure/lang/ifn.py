from exceptions import AbstractMethodCall

class IFn(object):
    def __call__(self, *args):
        raise AbstractMethodCall()