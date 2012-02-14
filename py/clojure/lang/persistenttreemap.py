from py.clojure.lang.apersistentmap import APersistentMap
from py.clojure.lang.iobj import IObj
from py.clojure.lang.cljexceptions import ArityException
from py.clojure.lang.reversible import Reversible
from py.clojure.lang.aseq import ASeq
import py.clojure.lang.rt as RT


class PersistentTreeMap(APersistentMap, IObj, Reversible):
    def __init__(self, *args):
        if len(args) == 0:
            self._meta = None
            self.comp = RT.DefaultComparator()
            self.tree = None
            self._count = 0
        elif len(args) == 1:
            self._meta = None
            self.comp = args[0]
            self.tree = None
            self._count = 0
        elif len(args) == 2:
            self._meta = args[0]
            self.comp = args[1]
            self.tree = None
            self._count = 0
        elif len(args) == 4 and isinstance(args[0], IPersistentMap):
            self._meta = args[0]
            self.comp = args[1]
            self.tree = args[2]
            self._count = args[2]
        elif len(args) == 4 and isinstance(args[0], Comparator):
            self._meta = meta
            self.comp = comp
            self.tree = tree
            self._count = count
        else:
            raise ArityException()

    def withMeta(self, meta):
        return PersistentTreeMap(meta, self.comp, self.tree, self._count)

    @staticmethod
    def create(self, *args):
        if len(args) == 1 and isinstance(args[0], Map):
            other = args[0]
            ret = EMPTY
            for o in other.entrySet():
                ret = ret.assoc(o.getKey(), o.getValue())
            return ret
        elif len(args) == 1 and isinstance(args[0], ISeq):
            items = args[0]
            ret = EMPTY
            while items is not None:
                if items.next() is None:
                    raise IllegalArgumentException("No value supplied for key: %s" % items.first())
                ret = ret.assoc(items.first(), RT.second(items))
                items = items.next().next()
            return ret
        elif len(args) == 2:
            comp = args[0]
            items = args[1]
            ret = PersistentTreeMap(comp)
            while items is not None:
                if items.next() is None:
                    raise IllegalArgumentException("No value supplied for key: %s" % items.first())
                ret = ret.assoc(items.first(), RT.second(items))
                items = items.next().next()
            return ret

    def containsKey(self, key):
        return self.entryAt(key) is not None

    def assocEx(self, key, val):
        found = Box(None)
        t = add(self.tree, key, val, found)
        if t is None:   # None == already contains key
            raise Util.runtimeException("Key already present")
        return PersistentTreeMap(comp, t.blacken(), self._count + 1, self.meta())

    def assoc(self, key, val):
        found = Box(None)
        t = add(self.tree, key, val, found)
        if t is None: # None == already contains key
            foundNode = found.val
            if foundNode.val() is val: # note only get same collection on identity of val, not equals()
                return self
            return PersistentTreeMap(self.comp, self.replace(self.tree, key, val), self._count, self.meta())
        return PersistentTreeMap(self.comp, t.blacken(), self._count + 1, self.meta())

    def without(self, key):
        found = Box(None)
        t = remove(self.tree, key, found)
        if t is None:
            if found.val is None: # None == doesn't contain key
                return self
            # empty
            return PersistentTreeMap(self.meta(), self.comp)
        return PersistentTreeMap(self.comp, t.blacken(), self._count - 1, self.meta())

    def seq(self):
        if self._count > 0:
            return self.Seq.create(self.tree, True, self._count)
        return None

    def empty(self):
        return PersistentTreeMap(self.meta(), self.comp);	

    def rseq(self):
        if self._count > 0:
            return self.Seq.create(self.tree, False, self._count)
        return None

    def comparator(self):
        return self.comp

    def entryKey(self, entry):
        return entry.key()

    def seq(self, ascending):
        if self._count > 0:
            return self.Seq.create(self.tree, ascending, self._count)
        return None

    def seqFrom(self, key, ascending):
        if self._count > 0:
            stack = None
            t = tree
            while t is not None:
                c = self.doCompare(key, t.key)
                if c == 0:
                    stack = RT.cons(t, stack)
                    return self.Seq(stack, ascending)
                elif ascending:
                    if c < 0:
                        stack = RT.cons(t, stack)
                        t = t.left()
                    else:
                        t = t.right()
                else:
                    if c > 0:
                        stack = RT.cons(t, stack)
                        t = t.right()
                    else:
                        t = t.left()
            if stack is not None:
                return self.Seq(stack, ascending)
        return None

    def iterator(self):
        return self.NodeIterator(self.tree, True)

    def reverseIterator(self):
        return self.NodeIterator(self.tree, False)

    def keys(self, *args):
        if len(args) == 0:
            return self.keys(self.iterator())
        elif len(args) == 1:
            it = args[0]
            return KeyIterator(it)

    def vals(self, *args):
        if len(args) == 0:
            return self.vals(self.iterator())
        elif len(args) == 1:
            it = args[0]
            return ValIterator(it)

    def minKey(self):
        t = self.min()
        return t.key if t is not None else None

    def min(self):
        t = tree
        if t is not None:
            while t.left() is not None:
                t = t.left()
        return t

    def maxKey(self):
        t = self.max()
        return t.key if t is not None else None

    def max(self):
        t = tree
        if t is not None:
            while t.right() is not None:
                t = t.right()
        return t

    def depth(self, *args):
        if len(args) == 0:
            return self.depth(self.tree)
        if len(args) == 1:
            t = args[0]
            if t is None:
                return 0
            return 1 + max([self.depth(t.left()), self.depth(t.right())])

    def valAt(self, *args):
        if len(args) == 1:
            key = args[0]
            return valAt(key, None)
        elif len(args) == 2:
            key = args[0]
            notFound = args[1]
            n = self.entryAt(key)
            return n.val() if n is not None else notFound

    def capacity(self):
        return self._count
    count = capacity

    def entryAt(self, key):
        t = self.tree
        while t is not None:
            c = self.doCompare(key, t.key)
            if c == 0:
                return t
            elif c < 0:
                t = t.left()
            else:
                t = t.right()
        return t

    def doCompare(self, k1, k2):
        return self.comp.compare(k1, k2)

    def add(self, t, key, val, found):
        if t is None:
            if val is None:
                return self.Red(key)
            return self.RedVal(key, val)
        c = self.doCompare(key, t.key)
        if c == 0:
            found.val = t
            return None
        ins = self.add(t.left(), key, val, found) if c < 0 else self.add(t.right(), key, val, found)
        if ins is None: # found below
            return None
        if c < 0:
            return t.addLeft(ins)
        return t.addRight(ins)

    def remove(self, t, key, found):
        if t is None:
            return None; # not found indicator
        c = self.doCompare(key, t.key)
        if c == 0:
            found.val = t
            return self.append(t.left(), t.right())
        del_ = self.remove(t.left(), key, found) if c < 0 else self.remove(t.right(), key, found)
        if del_ is None and found.val is None: # not found below
            return None
        if c < 0:
            if isinstance(t.left(), Black):
                return self.balanceLeftDel(t.key, t.val(), del_, t.right())
            else:
                return self.red(t.key, t.val(), del_, t.right())
        if isinstance(t.right(), Black):
            return self.balanceRightDel(t.key, t.val(), t.left(), del_)
        return self.red(t.key, t.val(), t.left(), del_)

    def append(self, left, right):
        if left is None:
            return right
        elif right is None:
            return left
        elif isinstance(left, self.Red):
            if isinstance(right, self.Red):
                app = self.append(left.right(), right.left())
                if isinstance(app, self.Red):
                    return self.red(app.key, app.val(),
                                    self.red(left.key, left.val(), left.left(), app.left()),
                                    self.red(right.key, right.val(), app.right(), right.right()))
                else:
                    return self.red(left.key, left.val(), left.left(), self.red(right.key, right.val(), app, right.right()))
            else:
                return self.red(left.key, left.val(), left.left(), self.append(left.right(), right))
        elif isinstance(right, self.Red):
            return red(right.key, right.val(), append(left, right.left()), right.right())
        else: # black/black
            app = self.append(left.right(), right.left())
            if isinstance(app, self.Red):
                return self.red(app.key, app.val(),
                                self.black(left.key, left.val(), left.left(), app.left()),
                                self.black(right.key, right.val(), app.right(), right.right()))
            else:
                return self.balanceLeftDel(left.key, left.val(), left.left(), self.black(right.key, right.val(), app, right.right()))

    def balanceLeftDel(self, key, val, del_, right):
        if isinstance(del_, self.Red):
            return red(key, val, del_.blacken(), right)
        elif isinstance(right, self.Black):
            return self.rightBalance(key, val, del_, right.redden())
        elif isinstance(right, self.Red) and isinstance(right.left(), self.Black):
            return self.red(right.left().key, right.left().val(),
                            self.black(key, val, del_, right.left().left()),
                            self.rightBalance(right.key, right.val(), right.left().right(), right.right().redden()))
        else:
            raise UnsupportedOperationException("Invariant violation")

    def balanceRightDel(self, key, val, left, del_):
        if isinstance(del_, self.Red):
            return self.red(key, val, left, del_.blacken())
        elif isinstance(left, self.Black):
            return self.leftBalance(key, val, left.redden(), del_)
        elif isinstance(left, self.Red) and isinstance(left.right(), self.Black):
            return self.red(left.right().key, left.right().val(),
                    self.leftBalance(left.key, left.val(), left.left().redden(), left.right().left()),
                    self.black(key, val, left.right().right(), del_))
        else:
            raise UnsupportedOperationException("Invariant violation")

    def leftBalance(self, key, val, ins, right):
        if isinstance(ins, self.Red) and isinstance(ins.left(), self.Red):
            return self.red(ins.key, ins.val(), ins.left().blacken(), self.black(key, val, ins.right(), right))
        elif isinstance(ins, self.Red) and isinstance(ins.right(), self.Red):
            return self.red(ins.right().key, ins.right().val(),
                            self.black(ins.key, ins.val(), ins.left(), ins.right().left()),
                            self.black(key, val, ins.right().right(), right))
        else:
            return self.black(key, val, ins, right)

    def rightBalance(self, key, val, left, ins):
        if isinstance(ins, self.Red) and isinstance(ins.right(), self.Red):
            return self.red(ins.key, ins.val(), black(key, val, left, ins.left()), ins.right().blacken())
        elif isinstance(ins, self.Red) and isinstance(ins.left(), self.Red):
            return self.red(ins.left().key, ins.left().val(),
                            self.black(key, val, left, ins.left().left()),
                            self.black(ins.key, ins.val(), ins.left().right(), ins.right()))
        else:
            return self.black(key, val, left, ins)

    def replace(self, t, key, val):
        c = doCompare(key, t.key)
        return t.replace(t.key,
                         val if c == 0 else t.val(),
                         replace(t.left(), key, val) if c < 0 else t.left(),
                         replace(t.right(), key, val) if c > 0 else t.right())

    def red(self, key, val, left, right):
        if left is None and right is None:
            if val is None:
                return self.Red(key)
            return self.RedVal(key, val)
        if val is None:
            return self.RedBranch(key, left, right)
        return self.RedBranchVal(key, val, left, right)

    def black(self, key, val, left, right):
        if left is None and right is None:
            if val is None:
                return self.Black(key)
            return self.BlackVal(key, val)
        if val is None:
            return self.BlackBranch(key, left, right)
        return self.BlackBranchVal(key, val, left, right)

    def meta(self):
        return self._meta

    class Node(object):
        def __init__(self, key):
            self.key = key

        def key(self):
            return self.key
        getKey = key

        def val(self):
            return None
        getValue = val

        def left(self):
            return None

        def right(self):
            return None

        def addLeft(self, ins):
            raise AbstractMethodCall

        def addRight(self, ins):
            raise AbstractMethodCall

        def removeLeft(self, del_):
            raise AbstractMethodCall

        def removeRight(self, del_):
            raise AbstractMethodCall

        def blacken(self):
            raise AbstractMethodCall

        def redden(self):
            raise AbstractMethodCall

        def balanceLeft(self, parent):
            return self.black(parent.key, parent.val(), self, parent.right())

        def balanceRight(self, parent):
            return self.black(parent.key, parent.val(), parent.left(), self)

        def replace(self, key, val, left, right):
            raise AbstractMethodCall

    class Black(Node):
        def addLeft(self, ins):
            return ins.balanceLeft(self)

        def addRight(self, ins):
            return ins.balanceRight(self)

        def removeLeft(self, del_):
            return self.balanceLeftDel(self.key, self.val(), del_, self.right())

        def removeRight(self, del_):
            return self.balanceRightDel(self.key, self.val(), self.left(), del_)

        def blacken(self):
            return self

        def redden(self):
            return Red(key)

        def replace(self, key, val, left, right):
            return self.black(key, val, left, right)

    class BlackVal(Black):
        def __init__(self, key, val):
            super(key)
            self.val = val

        def val(self):
            return val

        def redden(self):
            return RedVal(key, val)

    class BlackBranch(Black):
        def __init__(self, key, left, right):
            super(key)
            self.left = left
            self.right = right

        def left(self):
            return self.left

        def right(self):
            return self.right

        def redden(self):
            return self.RedBranch(self.key, self.left, self.right)

    class BlackBranchVal(BlackBranch):
        def __init__(self, key, val, left, right):
            super(key, left, right)
            self.val = val

        def val(self):
            return self.val

        def redden(self):
            return RedBranchVal(self.key, val, self.left, self.right)

    class Red(Node):
        def addLeft(self, ins):
            return red(self.key, val(), ins, self.right())

        def addRight(self, ins):
            return red(self.key, self.val(), self.left(), ins)

        def removeLeft(self, del_):
            return red(self.key, self.val(), del_, self.right())

        def removeRight(self, del_):
            return red(self.key, self.val(), self.left(), del_)

        def blacken(self):
            return Black(self.key)

        def redden(self):
            raise UnsupportedOperationException("Invariant violation")

        def replace(self, key, val, left, right):
            return red(key, val, left, right)

    class RedVal(Red):
        def __init__(self, key, val):
            super(key)
            self.val = val

        def val(self):
            return self.val

        def blacken(self):
            return BlackVal(self.key, self.val)

    class RedBranch(Red):
        def __init__(self, key, left, right):
            super(key)
            self.left = left
            self.right = right

        def left(self):
            return self.left

        def right(self):
            return self.right

        def balanceLeft(self, parent):
            if isinstance(self.left, Red):
                return red(self.key, self.val(), self.left.blacken(), black(parent.key, parent.val(), right, parent.right()))
            elif isinstance(self.right, Red):
                return red(self.right.key, self.right.val(), black(self.key, self.val(), self.left, self.right.left()),
                           black(parent.key, parent.val(), self.right.right(), parent.right()))
            else:
                return super(RedBranch, self).balanceLeft(parent)

        def balanceRight(self, parent):
            if isinstance(self.right, Red):
                return red(self.key, self.val(), black(parent.key, parent.val(), parent.left(), self.left), self.right.blacken())
            elif isinstance(left, Red):
                return red(self.left.key, self.left.val(), black(parent.key, parent.val(), parent.left(), self.left.left()),
                           black(self.key, self.val(), self.left.right(), self.right))
            else:
                return super(RedBranch, self).balanceRight(parent)

        def blacken(self):
            return BlackBranch(self.key, self.left, self.right)

    class RedBranchVal(RedBranch):
        def RedBranchVal(self, key, val, left, right):
            super(key, left, right)
            self.val = val

        def val(self):
            return self.val

        def blacken(self):
            return BlackBranchVal(self.key, self.val, self.left, self.right)

    class Seq(ASeq):
        def __init__(self, stack, asc):
            self.stack = stack
            self.asc = asc
            self.cnt = -1
        def __init__(self, stack, asc, cnt):
            self.stack = stack
            self.asc = asc
            self.cnt = cnt
        def __init__(self, meta, stack, asc, cnt):
            super(meta)
            self.stack = stack
            self.asc = asc
            self.cnt = cnt

        @staticmethod
        def create(t, asc, cnt):
            return Seq(self.push(t, None, asc), asc, cnt)

        def push(self, t, stack, asc):
            while t is not None:
                stack = RT.cons(t, stack)
                t = t.left() if asc else t.right()
            return stack

        def first(self):
            return stack.first()

        def next(self):
            t = self.stack.first()
            nextstack = self.push(t.right() if self.asc else t.left(), self.stack.next(), self.asc)
            if nextstack is not None:
                return Seq(nextstack, self.asc, self.cnt - 1)
            return None

        def count(self):
            if self.cnt < 0:
                return super(Seq, self).count()
            return self.cnt

        def withMeta(self, meta):
            return Seq(meta, self.stack, self.asc, self.cnt)

    class NodeIterator(object):
        def __init__(self, t, asc):
            self.asc = asc
            self.push(t)

        def push(self, t):
            while t is not None:
                self.stack.push(t)
                t = t.left() if self.asc else t.right()

        def hasNext(self):
            return not self.stack.isEmpty()

        def next(self):
            t = self.stack.pop()
            self.push(t.right() if self.asc else t.left())
            return t

        def remove(self):
            raise UnsupportedOperationException()

    class KeyIterator(object):
        def __init__(self, it):
            self._it = it

        def hasNext(self):
            return self._it.hasNext()

        def next(self):
            return self._it.next().key

        def remove(self):
            raise UnsupportedOperationException()

    class ValIterator(object):
        def __init__(self, it):
            self._it = it

        def hasNext(self):
            return self._it.hasNext()

        def next(self):
            return self._it.next().val()

        def remove(self):
            raise UnsupportedOperationException()
        
EMPTY = PersistentTreeMap()
