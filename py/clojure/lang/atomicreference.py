
class AtomicReference(object):
    def __init__(self, val = None):
        self.val = val
    def get(self):
        return self.val
    def set(self, val):
        self.val = val
    def mutate(self, fn):
        self.val = fn(self.val)
