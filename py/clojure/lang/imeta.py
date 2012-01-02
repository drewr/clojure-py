from py.clojure.lang.cljexceptions import AbstractMethodCall

class IMeta():
    def meta(self):
        raise AbstractMethodCall()