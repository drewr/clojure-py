from exceptions import AbstractMethodCall

class IMeta(object):
    def meta(self):
        raise AbstractMethodCall()