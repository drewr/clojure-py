

class AbstractMethodCall(Exception):
    def __init__(self, cls = None):
        if cls is not None:
            Exception.__init__(self, "in " + cls.__class__.__name__)
        else:
            Exception.__init__(self)

class ArityException(TypeError):
    def __init__(self, s = None):
        TypeError.__init__(self, s)

class IllegalStateException(Exception):
    def __init__(self, s = None):
        Exception.__init__(self, s)

class InvalidArgumentException(Exception):
    def __init__(self, s = None):
        Exception.__init__(self, s)

class IllegalAccessError(Exception):
    def __init__(self, s = None):
        Exception.__init__(self, s)

class IndexOutOfBoundsException(Exception):
    def __init__(self, s = None):
        Exception.__init__(self, s)

class ReaderException(Exception):
    def __init__(self, s = None):
        Exception.__init__(self, s)
