from exceptions import AbstractMethodCall

class IFn(object):
    def __call__(self, *args, **kwargs):
        raise AbstractMethodCall()