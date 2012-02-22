def fact(x):
    f = 1
    n = x
    while True:
        if n == 1:
            return f
        else:
            f = f * n
            n = n - 1
            continue


def test(times):
    rem = times
    while True:
        if rem > 0:
            fact(20000)
            rem = rem - 1
            continue
        else:
            return


test(20)
