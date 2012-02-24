from clojure.lang.apersistentmap import APersistentMap
from clojure.lang.aseq import ASeq
from clojure.lang.box import Box
from clojure.lang.cljexceptions import (ArityException,
                                        AbstractMethodCall,
                                        IllegalArgumentException,
                                        UnsupportedOperationException)
from clojure.lang.comparator import Comparator
from clojure.lang.iobj import IObj
from clojure.lang.ipersistentmap import IPersistentMap
from clojure.lang.iseq import ISeq
from clojure.lang.reversible import Reversible
import clojure.lang.rt as RT


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
            self.comp = args[0]
            self.tree = args[1]
            self._count = args[2]
            self._meta = args[3]
        else:
            raise ArityException()

    def withMeta(self, meta):
        return PersistentTreeMap(meta, self.comp, self.tree, self._count)

    def containsKey(self, key):
        return self.entryAt(key) is not None

    def assocEx(self, key, val):
        found = Box(None)
        t = self.add(self.tree, key, val, found)
        if t is None:   # None == already contains key
            raise Util.runtimeException("Key already present")#FIXME: Util not defined anywhere?
        return PersistentTreeMap(self.comp, t.blacken(), self._count + 1, self.meta())

    def assoc(self, key, val):
        found = Box(None)
        t = self.add(self.tree, key, val, found)
        if t is None: # None == already contains key
            foundNode = found.val
            if foundNode.val() is val: # note only get same collection on identity of val, not equals()
                return self
            return PersistentTreeMap(self.comp, self.replace(self.tree, key, val), self._count, self.meta())
        return PersistentTreeMap(self.comp, t.blacken(), self._count + 1, self.meta())

    def without(self, key):
        found = Box(None)
        t = self.remove(self.tree, key, found)
        if t is None:
            if found.val is None: # None == doesn't contain key
                return self
            # empty
            return PersistentTreeMap(self.meta(), self.comp)
        return PersistentTreeMap(self.comp, t.blacken(), self._count - 1, self.meta())

    def seq(self, *args):
        if len(args) == 0:
            if self._count > 0:
                return createSeq(self.tree, True, self._count)
            return None
        elif len(args) == 1:
            ascending = args[0]
            if self._count > 0:
                return createSeq(self.tree, ascending, self._count)
            return None
        else:
            raise ArityException()

    def empty(self):
        return PersistentTreeMap(self.meta(), self.comp);	

    def rseq(self):
        if self._count > 0:
            return createSeq(self.tree, False, self._count)
        return None

    def comparator(self):
        return self.comp

    def seqFrom(self, key, ascending):
        if self._count > 0:
            stack = None
            t = self.tree
            while t is not None:
                c = self.doCompare(key, t.key())
                if c == 0:
                    stack = RT.cons(t, stack)
                    return Seq(stack, ascending)
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
                return Seq(stack, ascending)
        return None

    def iterator(self):
        return NodeIterator(self.tree, True)

    def reverseIterator(self):
        return NodeIterator(self.tree, False)

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
        return t.key() if t is not None else None

    def min(self):
        t = self.tree
        if t is not None:
            while t.left() is not None:
                t = t.left()
        return t

    def maxKey(self):
        t = self.max()
        return t.key() if t is not None else None

    def max(self):
        t = self.tree
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
            return self.valAt(key, None)
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
            c = self.doCompare(key, t.key())
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
                return Red(key)
            return RedVal(key, val)
        c = self.doCompare(key, t.key())
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
        c = self.doCompare(key, t.key())
        if c == 0:
            found.val = t
            return self.append(t.left(), t.right())
        del_ = self.remove(t.left(), key, found) if c < 0 else self.remove(t.right(), key, found)
        if del_ is None and found.val is None: # not found below
            return None
        if c < 0:
            if isinstance(t.left(), Black):
                return self.balanceLeftDel(t.key(), t.val(), del_, t.right())
            else:
                return red(t.key(), t.val(), del_, t.right())
        if isinstance(t.right(), Black):
            return self.balanceRightDel(t.key(), t.val(), t.left(), del_)
        return red(t.key(), t.val(), t.left(), del_)

    def append(self, left, right):
        if left is None:
            return right
        elif right is None:
            return left
        elif isinstance(left, Red):
            if isinstance(right, Red):
                app = self.append(left.right(), right.left())
                if isinstance(app, Red):
                    return red(app.key(), app.val(),
                               red(left.key(), left.val(), left.left(), app.left()),
                               red(right.key(), right.val(), app.right(), right.right()))
                else:
                    return red(left.key(), left.val(), left.left(), red(right.key(), right.val(), app, right.right()))
            else:
                return red(left.key(), left.val(), left.left(), self.append(left.right(), right))
        elif isinstance(right, Red):
            return red(right.key(), right.val(), self.append(left, right.left()), right.right())
        else: # black/black
            app = self.append(left.right(), right.left())
            if isinstance(app, Red):
                return red(app.key(), app.val(),
                           black(left.key(), left.val(), left.left(), app.left()),
                           black(right.key(), right.val(), app.right(), right.right()))
            else:
                return self.balanceLeftDel(left.key(), left.val(), left.left(), black(right.key(), right.val(), app, right.right()))

    def balanceLeftDel(self, key, val, del_, right):
        if isinstance(del_, Red):
            return red(key, val, del_.blacken(), right)
        elif isinstance(right, Black):
            return self.rightBalance(key, val, del_, right.redden())
        elif isinstance(right, Red) and isinstance(right.left(), Black):
            return red(right.left().key(), right.left().val(),
                       black(key, val, del_, right.left().left()),
                       self.rightBalance(right.key(), right.val(), right.left().right(), right.right().redden()))
        else:
            raise UnsupportedOperationException("Invariant violation")

    def balanceRightDel(self, key, val, left, del_):
        if isinstance(del_, Red):
            return red(key, val, left, del_.blacken())
        elif isinstance(left, Black):
            return self.leftBalance(key, val, left.redden(), del_)
        elif isinstance(left, Red) and isinstance(left.right(), Black):
            return red(left.right().key(), left.right().val(),
                       self.leftBalance(left.key(), left.val(), left.left().redden(), left.right().left()),
                       black(key, val, left.right().right(), del_))
        else:
            raise UnsupportedOperationException("Invariant violation")

    def leftBalance(self, key, val, ins, right):
        if isinstance(ins, Red) and isinstance(ins.left(), Red):
            return red(ins.key(), ins.val(), ins.left().blacken(), black(key, val, ins.right(), right))
        elif isinstance(ins, Red) and isinstance(ins.right(), Red):
            return red(ins.right().key(), ins.right().val(),
                       black(ins.key(), ins.val(), ins.left(), ins.right().left()),
                       black(key, val, ins.right().right(), right))
        else:
            return black(key, val, ins, right)

    def rightBalance(self, key, val, left, ins):
        if isinstance(ins, Red) and isinstance(ins.right(), Red):
            return red(ins.key(), ins.val(), black(key, val, left, ins.left()), ins.right().blacken())
        elif isinstance(ins, Red) and isinstance(ins.left(), Red):
            return red(ins.left().key(), ins.left().val(),
                       black(key, val, left, ins.left().left()),
                       black(ins.key(), ins.val(), ins.left().right(), ins.right()))
        else:
            return black(key, val, left, ins)

    def replace(self, t, key, val):
        c = self.doCompare(key, t.key())
        return t.replace(t.key(),
                         val if c == 0 else t.val(),
                         self.replace(t.left(), key, val) if c < 0 else t.left(),
                         self.replace(t.right(), key, val) if c > 0 else t.right())

    def meta(self):
        return self._meta

EMPTY = PersistentTreeMap()


def create(self, *args):
    if len(args) == 1 and isinstance(args[0], Map):#FIXME: Map undefined
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

def entryKey(entry):
    return entry.key()

def red(key, val, left, right):
    if left is None and right is None:
        if val is None:
            return Red(key)
        return RedVal(key, val)
    if val is None:
        return RedBranch(key, left, right)
    return RedBranchVal(key, val, left, right)

def black(key, val, left, right):
    if left is None and right is None:
        if val is None:
            return Black(key)
        return BlackVal(key, val)
    if val is None:
        return BlackBranch(key, left, right)
    return BlackBranchVal(key, val, left, right)


class Node(object):
    def __init__(self, key):
        self._key = key

    def key(self):
        return self._key
    getKey = key

    def val(self):
        return None
    getValue = val

    def left(self):
        return None

    def right(self):
        return None

    def addLeft(self, ins):
        raise AbstractMethodCall()

    def addRight(self, ins):
        raise AbstractMethodCall()

    def removeLeft(self, del_):
        raise AbstractMethodCall()

    def removeRight(self, del_):
        raise AbstractMethodCall()

    def blacken(self):
        raise AbstractMethodCall()

    def redden(self):
        raise AbstractMethodCall()

    def balanceLeft(self, parent):
        return black(parent.key(), parent.val(), self, parent.right())

    def balanceRight(self, parent):
        return black(parent.key(), parent.val(), parent.left(), self)

    def replace(self, key, val, left, right):
        raise AbstractMethodCall()


class Black(Node):
    def addLeft(self, ins):
        return ins.balanceLeft(self)

    def addRight(self, ins):
        return ins.balanceRight(self)

    def removeLeft(self, del_):
        return self.balanceLeftDel(self.key(), self.val(), del_, self.right())

    def removeRight(self, del_):
        return self.balanceRightDel(self.key(), self.val(), self.left(), del_)

    def blacken(self):
        return self

    def redden(self):
        return Red(self.key)

    def replace(self, key, val, left, right):
        return black(key, val, left, right)


class BlackVal(Black):
    def __init__(self, key, val):
        super(BlackVal, self).__init__(key)
        self._val = val

    def val(self):
        return self._val

    def redden(self):
        return RedVal(self._key, self._val)


class BlackBranch(Black):
    def __init__(self, key, left, right):
        super(BlackBranch, self).__init__(key)
        self._left = left
        self._right = right

    def left(self):
        return self._left

    def right(self):
        return self._right

    def redden(self):
        return RedBranch(self._key, self._left, self._right)


class BlackBranchVal(BlackBranch):
    def __init__(self, key, val, left, right):
        super(BlackBranchVal, self).__init__(key, left, right)
        self._val = val

    def val(self):
        return self._val

    def redden(self):
        return RedBranchVal(self._key, self._val, self._left, self._right)


class Red(Node):
    def addLeft(self, ins):
        return red(self._key, self.val(), ins, self.right())

    def addRight(self, ins):
        return red(self._key, self.val(), self.left(), ins)

    def removeLeft(self, del_):
        return red(self._key, self.val(), del_, self.right())

    def removeRight(self, del_):
        return red(self._key, self.val(), self.left(), del_)

    def blacken(self):
        return Black(self._key)

    def redden(self):
        raise UnsupportedOperationException("Invariant violation")

    def replace(self, key, val, left, right):
        return red(key, val, left, right)


class RedVal(Red):
    def __init__(self, key, val):
        super(RedVal, self).__init__(key)
        self._val = val

    def val(self):
        return self._val

    def blacken(self):
        return BlackVal(self._key, self._val)


class RedBranch(Red):
    def __init__(self, key, left, right):
        super(RedBranch, self).__init__(key)
        self._left = left
        self._right = right

    def left(self):
        return self._left

    def right(self):
        return self._right

    def balanceLeft(self, parent):
        if isinstance(self._left, Red):
            return red(self._key, self.val(), self._left.blacken(), black(parent._key, parent.val(), self.right(), parent.right()))
        elif isinstance(self._right, Red):
            return red(self._right._key, self._right.val(), black(self._key, self.val(), self._left, self._right.left()),
                       black(parent._key, parent.val(), self._right.right(), parent.right()))
        else:
            return super(RedBranch, self).balanceLeft(parent)

    def balanceRight(self, parent):
        if isinstance(self._right, Red):
            return red(self._key, self.val(), black(parent._key, parent.val(), parent.left(), self._left), self._right.blacken())
        elif isinstance(self._left, Red):
            return red(self._left._key, self._left.val(), black(parent._key, parent.val(), parent.left(), self._left.left()),
                       black(self._key, self.val(), self._left.right(), self._right))
        else:
            return super(RedBranch, self).balanceRight(parent)

    def blacken(self):
        return BlackBranch(self._key, self._left, self._right)


class RedBranchVal(RedBranch):
    def __init__(self, key, val, left, right):
        super(RedBranchVal, self).__init__(key, left, right)
        self._val = val

    def val(self):
        return self._val

    def blacken(self):
        return BlackBranchVal(self._key, self._val, self._left, self._right)


class Seq(ASeq):
    def __init__(self, *args):
        if len(args) == 2:
            self.stack = args[0]
            self.asc = args[1]
            self.cnt = -1
        elif len(args) == 3:
            self.stack = args[0]
            self.asc = args[1]
            self.cnt = args[2]
        elif len(args) == 4:
            super(Seq, self).__init__(args[0])
            self.stack = args[1]
            self.asc = args[2]
            self.cnt = args[3]

    def first(self):
        return self.stack.first()

    def next(self):
        t = self.stack.first()
        nextstack = pushSeq(t.right() if self.asc else t.left(), self.stack.next(), self.asc)
        if nextstack is not None:
            return Seq(nextstack, self.asc, self.cnt - 1)
        return None

    def count(self):
        if self.cnt < 0:
            return super(Seq, self).count()
        return self.cnt

    def withMeta(self, meta):
        return Seq(meta, self.stack, self.asc, self.cnt)

def createSeq(t, asc, cnt):
    return Seq(pushSeq(t, None, asc), asc, cnt)

def pushSeq(t, stack, asc):
    while t is not None:
        stack = RT.cons(t, stack)
        t = t.left() if asc else t.right()
    return stack


class NodeIterator(object):
    def __init__(self, t, asc):
        self.asc = asc
        self.stack = []
        self.push(t)

    def push(self, t):
        while t is not None:
            self.stack.append(t)
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
        return self._it.next().key()

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
