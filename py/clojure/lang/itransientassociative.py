from cljexceptions import AbstractMethodCall
from itransientcollection import ITransientCollection
from ilookup import ILookup

class ITransientAssociative(ITransientCollection, ILookup):
    def assoc(self, key, val):
        raise AbstractMethodCall()
