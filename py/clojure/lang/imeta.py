from cljexceptions import AbstractMethodCall

class IMeta(object):
    def meta(self):
        raise AbstractMethodCall()