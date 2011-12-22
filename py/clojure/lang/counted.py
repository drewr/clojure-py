from cljexceptions import AbstractMethodCall

class Counted(object):
    def count(self):
        raise AbstractMethodCall()
    def __len__(self):
        return self.count()