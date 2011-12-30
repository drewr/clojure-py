from cljexceptions import AbstractMethodCall

class Interable(object):
    def __iter__(self):
        raise AbstractMethodCall()

