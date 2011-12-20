

class AbstractMethodCall(Exception):
    def __init__(self):
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

