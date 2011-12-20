from exceptions import AbstractMethodCall

class Seqable(object):
    def seq(self):
        raise AbstractMethodCall()
