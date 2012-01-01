from py.clojure.lang.ipersistentstack import IPersistentStack
from py.clojure.lang.sequential import Sequential

class IPersistentList(Sequential, IPersistentStack):
    pass
