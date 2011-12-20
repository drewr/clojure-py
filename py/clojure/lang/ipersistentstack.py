from exceptions import AbstractMethodCall
from ipersistentcollection import IPersistentCollection

class IPersistentStack(IPersistentCollection):
    def peek(self):
        raise AbstractMethodCall()
    def pop(self):
        raise AbstractMethodCall()
