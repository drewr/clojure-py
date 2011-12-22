from cljexceptions import AbstractMethodCall

class IEditableCollection(object):
    def asTransient(self):
        raise AbstractMethodCall()

