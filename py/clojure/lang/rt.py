from cljexceptions import AbstractMethodCall

def seq(obj):
    raise AbstractMethodCall()

def applyTo(fn, args):
    return apply(fn, tuple(args.interator()))

def booleanCast(obj):
    if isinstance(obj, bool):
        return obj
    return obj is None