from clojure.lang.cljexceptions import AbstractMethodCall

class Counted():
    def __len__(self):
        raise AbstractMethodCall(self)