from py.clojure.lang.aseq import ASeq
from py.clojure.lang.counted import Counted

class IndexableSeq(ASeq, Counted):
    def __init__(self, array, i):
        self.array = array
        self.i = i

    def first(self):
        return self.array[self.i]
    def next(self):
        if self.i >= len(self.array) - 1:
            return None
        return IndexableSeq(self.array, self.i + 1)
    def __len__(self):
        return len(self.array) - self.i
