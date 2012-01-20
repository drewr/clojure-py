import dis, marshal, struct, sys, time, types

def c1(*args):
    if len(args) == 1:
        return len(args)
    if len(args) == 2:
        return len(args)
    if len(args) == 3:
        return len(args)
    return None

def c2(*args):
    if len(args) < len(c2.fns):
        fn = c2.fns[len(args)]
        if fn is not None:
            return fn(*args)
    return None


def pargs(*args):
    return len(args)

c2.fns = [pargs, pargs, pargs]


for x in range(1000000):
    for x in range(10):
        pargs(*range(x))
