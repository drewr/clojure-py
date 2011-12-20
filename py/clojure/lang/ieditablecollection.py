from exceptions import AbstractMethodCall

class IEditableCollection(object):
    def asTransient(self):
        raise AbstractMethodCall()

