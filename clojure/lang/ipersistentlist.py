from clojure.lang.ipersistentstack import IPersistentStack
from clojure.lang.sequential import Sequential

class IPersistentList(Sequential, IPersistentStack):
    pass
