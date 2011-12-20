from threading import Lock, local


def synchronized(f):
    """ Synchronization decorator. """
    lock = Lock()

    def synchronized_closure(*args, **kw):
        lock.acquire()
        try:
            return f(*args, **kw)
        finally:
            lock.release()
    return synchronized_closure


def ThreadLocal(local):
    def __init__(self):
        pass
    def get(self, defaultfn):
        if not hasattr(self, "value"):
            self.value = defaultfn()
        return self.value
    def set(self, value):
        self.value = value