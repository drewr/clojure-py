from py.clojure.lang.cljexceptions import AbstractMethodCall
from py.clojure.lang.counted import Counted

class Indexed(Counted):
    def nth(self, i, notFound = None):
        raise AbstractMethodCall(self)