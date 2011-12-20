from exceptions import AbstractMethodCall

def hashCombine(hash, seed):
    seed ^= seed + 0x9e3779b9 + (seed << 6) + (seed >> 2)
    return seed


def hasheq(o):
    raise AbstractMethodCall()