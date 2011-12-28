from cljexceptions import AbstractMethodCall

class Reversible():
    def rseq(self):
        raise AbstractMethodCall()
