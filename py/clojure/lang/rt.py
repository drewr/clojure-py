from cljexceptions import AbstractMethodCall

def seq(obj):
    raise AbstractMethodCall()

def applyTo(fn, args):
    return apply(fn, tuple(args.interator()))

def booleanCast(obj):
    if isinstance(obj, bool):
        return obj
    return obj is None

def keys(obj):
    from apersistentmap import APersistentMap
    return APersistentMap.KeySeq.create(obj)

def vals(obj):
    from apersistentmap import APersistentMap
    return APersistentMap.ValueSeq.create(obj)

def fulfillsHashSet(obj):
    if not hasattr(obj, "__getitem__"):
        return False
    if not hasattr(obj, "__iter__"):
        return False
    if not hasattr(obj, "__contains__"):
        return False
    return True