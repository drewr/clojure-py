
gcount = 0

def count(f):
    global gcount
    gcount += 1
    c = gcount
    def wrap(*args, **kw):
        print c
        f(*args, **kw)
    return wrap

class Bar1():
    @count
    def foo(self):
        print "done"

class Bar2():
    @count
    def foo(self):
        print "done2"

bar1 = Bar1()
bar2 = Bar1()
bar3 = Bar2()

bar1.foo()
bar2.foo()
bar3.foo()