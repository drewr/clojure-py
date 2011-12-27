from cljexceptions import AbstractMethodCall

class Counted(object):
    def __len__(self):
        raise AbstractMethodCall()