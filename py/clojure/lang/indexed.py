from cljexceptions import AbstractMethodCall
from counted import Counted

class Indexed(Counted):
    def nth(self, i, notFound = None):
        raise AbstractMethodCall()