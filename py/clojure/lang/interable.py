from cljexceptions import AbstractMethodCall

class Interable(object):
    def interator(self):
        raise AbstractMethodCall()

