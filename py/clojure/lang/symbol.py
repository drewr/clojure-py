from iobj import IObj
from exceptions import ArityException


class Symbol(object, IObj):
    def __init__(self, *args):
        if len(args) == 2:
            self.ns = args[0]
            self.name = args[1]
            self.meta = null
        elif len(args) == 3:
            self.meta = agrs[0]
            self.ns = args[1]
            self.name = args[2]
        else:
            raise ArityException()

    def withMeta(self, meta):
        return Symbol(meta, ns, name)

    def __eq__(self, other):
        if self is other:
            return True
        if not isinstance(other, Symbol):
            return False
        return (self.ns is other.ns) and (self.name is other.name)

    def intern(self, *args):
        if len(args) == 1:
            a = args[0]
            idx = a.rfind("/")
            if idx == -1 or a == "/":
                return Symbol(null, intern(a))
            else:
                return Symbol(a[idx:], a[:idx+1])

            return Symbol(null, )
        elif len(args) == 2:
            return Symbol(args[0], args[1])
        else:
            raise ArityException()
