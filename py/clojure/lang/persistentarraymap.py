from exceptions import ArityException, InvalidArgumentException
from apersistentmap import APersistentMap
from ieditablecollection import IEditableCollection
from mapentry import MapEntry
from aseq import ASeq
from counted import Counted

HASHTABLE_THRESHOLD = 16

class PersistentArrayMap(APersistentMap, IEditableCollection):

    def __init__(self, *args):
        if len(args) == 0:
            self.array = []
            self._meta = None
        elif len(args) == 1:
            self.array = args[0]
            self._meta = None
        elif len(args) == 2:
            self._meta = args[0]
            self.array = args[1]
        else:
            raise ArityException()

    def withMeta(self, meta):
        return PersistentArrayMap(meta, self.array)

    def create(self, *args):
        return PersistentArrayMap(self.meta(), args)

    def createHT(self, init):
        return PersistentArrayMap(self.meta(), init)

    @staticmethod
    def createWithCheck(self, init):
        for i in range(0, len(init), 2):
            for j in range(i+2, len(init), 2):
                if init[i] == init[j]:
                    raise InvalidArgumentException("Duplicate Key" + str(init[i]))
        return PersistentArrayMap(init)

    def assoc(self, key, val):
        i = self.indexOf(key)
        if i >= 0: # allready have the key
            if self.array[i + 1] == val:
                return this #no op
            newarray = self.array[:]
            newarray[i + 1] = val
        else:
            if len(self.array) > HASHTABLE_THRESHOLD:
                return self.createHT(self.array).assoc(key, value)
            newarray = self.array[:]
            newarray.append(key)
            newarray.append(value)

        return self.create(newarray)

    def without(self, key):
        i = self.indexOf(key)
        if i >= 0:
            newlen = len(self.array) - 2
            if not newlen:
                return self.empty()
            newarr = self.array[:i]
            newarr.extend(self.array[i+2:])
            return self.create(newarr)
        return self

    def empty(self):
        return EMPTY.withMeta(self.meta())

    def containsKey(self, key):
        return self.indexOf(key) >= 0

    def count(self):
        return len(self.array) / 2

    def indexOf(self, key):
        for x in range(len(self.array), 2):
            if self.array[x] == key:
                return x
        return -1

    def entryAt(self, key):
        i = self.indexOf(key)
        if i >= 0:
            return MapEntry(self.array[i], self.array[i+1])
        return None

    def valAt(self, key, notFound = None):
        i = self.indexOf(key)
        if i >= 0:
            return self.array[i + 1]
        return notFound

    def seq(self):
        return PersistentArrayMap.Seq(self.array, 0)

    def meta(self):
        return self._meta

    def interator(self):
        for x in range(0, len(self.array), 2):
            yield MapEntry(self.array[x], self.array[x + 1])




    class Seq(ASeq, Counted):
        def __init__(self, *args):
            if len(args) == 2:
                self._meta = None
                self.array = args[1]
                self.i = args[2]
            elif len(args) == 3:
                self._meta = args[0]
                self.array = args[1]
                self.i = args[2]
            else:
                raise ArityException()

        def first(self):
            return MapEntry(self.array[self.i], self.array[self.i + 1])
        def next(self):
            return PersistentArrayMap.Seq(self.array, self.i + 2)
        def count(self):
            return (len(self.array) - self.i) / 2
        def withMeta(self, meta):
            return PersistentArrayMap.Seq(meta, self.array, self.i)

    def asTransient(self):
        return TransientArrayMap(self.array)


