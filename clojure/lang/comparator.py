from clojure.lang.cljexceptions import AbstractMethodCall

class Comparator(object):
    def compare(self, a, b):
        raise AbstractMethodCall()
