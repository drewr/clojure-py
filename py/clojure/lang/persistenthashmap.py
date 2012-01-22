from py.clojure.lang.apersistentmap import APersistentMap
#from py.clojure.lang.atransientmap import ATransientMap
from py.clojure.lang.cljexceptions import ArityException, AbstractMethodCall
from py.clojure.lang.ieditablecollection import IEditableCollection
from py.clojure.lang.iobj import IObj
from py.clojure.lang.aseq import ASeq
from py.clojure.lang.util import bitCount, arrayCopy
from py.clojure.lang.box import Box
from py.clojure.lang.atomicreference import AtomicReference
from py.clojure.lang.mapentry import MapEntry
from py.clojure.lang.cons import Cons

def mask(h, shift):
    return (h >> shift) & 0x01f

def cloneAndSet(array, i, a, j = None, b = None):
    clone = array[:]
    clone[i] = a
    if j:
        clone[j] = b
    return clone

def bitpos(hsh, shift):
    return 1 << mask(hsh, shift)

def createNode(*args):
    if len(args) == 7:
        edit, shift, key1, val1, key2hash, key2, val2 = args
    elif len(args) == 6:
        shift, key1, val1, key2hash, key2, val2 = args
        edit = AtomicReference()
    else:
        raise ArityException()

    if shift > 64:
        raise Exception("Shift max reached")

    key1hash = hash(key1)
    if key1hash == key2hash:
        return PersistentHashMap.HashCollisionNode(None, key1hash, 2, [key1, val1, key2, val2])
    nbox = Box(None)
    nd1 =  EMPTY_BITMAP_NODE \
            .assocEd(edit, shift, key1hash, key1, val1, nbox)
    nd2 = nd1.assocEd(edit, shift, key2hash, key2, val2, nbox)
    return nd2

def removePair(array, i):
    newArray = array[:2*i]
    newArray.extend(array[2*(i + 1):])
    return newArray
    
class PersistentHashMap(APersistentMap, IEditableCollection, IObj):



    def __init__(self, *args):
        if len(args) == 4:
            self._meta = None
            self.count = args[0]
            self.root = args[1]
            self.hasNull = args[2]
            self.noneValue = args[3]
        elif len(args) == 5:
            self._meta = args[0]
            self.count = args[1]
            self.root = args[2]
            self.hasNull = args[3]
            self.noneValue = args[4]
        else:
            raise ArityException()

    @staticmethod
    def fromDict(d):
        m = EMPTY
        for v in d:
            m = m.assoc(v, d[v])
        return m


    def assoc(self, key, val):
        if key is None:
            if self.hasNull and val == self.noneValue:
                return self
            return PersistentHashMap(self._meta, self.count if self.hasNull else self.count + 1, self.root, True, val)
        addedLeaf = Box(None)
        newRoot = EMPTY_BITMAP_NODE if self.root is None else self.root
        newRoot = newRoot.assoc(0, hash(key), key, val, addedLeaf)

        if newRoot == self.root:
            return self

        return PersistentHashMap(self._meta, self.count if addedLeaf.val is None else self.count + 1, newRoot, self.hasNull, self.noneValue)

    def without(self, key):
        if key is None:
            return PersistentHashMap(self._meta, self.count - 1, self.root, False, None) if self.hasNull else self

        if self.root is None:
            return self
        newroot = self.root.without(0, hash(key), key)
        if newroot is self.root:
            return self

        return PersistentHashMap(self._meta, self.count - 1, newroot, self.hasNull, self.noneValue)

    def valAt(self, key, notFound = None):
        if key is None:
            return self.noneValue if self.hasNull else notFound
        return self.root.find(0, hash(key), key, notFound) if self.root is not None else notFound

    def entryAt(self, key, notFound = None):
        val = self.root.find(0, hash(key), key, notFound) if self.root is not None else notFound
        return MapEntry(key, val)

    def seq(self):
        s =  self.root.nodeSeq() if self.root is not None else None
        return Cons(MapEntry(None, self.noneValue), s) if self.hasNull else s

    def __len__(self):
        return self.count

    def containsKey(self, key):
        if key is None:
            return self.hasNull
        return self.root.find(0, hash(key), key, NOT_FOUND) is not NOT_FOUND if self.root is not None else False

    def __repr__(self):
        s = []
        for x in self:
            s.append(repr(x))
            s.append(repr(self[x]))
        return "{" + " ".join(s) + "}"

    class INode(object):
        def assoc(self, shift,  hsh, key, val, addedLeaf):
            raise AbstractMethodCall(self)

        def without(self,  shift,  hsh, key):
            raise AbstractMethodCall(self)

        def find(self,  shift,  hsh, key, notFound = None):
            raise AbstractMethodCall(self)

        def nodeSeq(self):
            raise AbstractMethodCall(self)

        def assocEd(self, edit,  shift, hsh, key, val, addedLeaf):
            raise AbstractMethodCall(self)

        def withoutEd(self,  edit,  shift,  hsh,  key,  removedLeaf):
            raise AbstractMethodCall(self)

    class ArrayNode(INode):
        def __init__(self, edit, count, array):
            self.edit = edit
            self.count = count
            self.array = array

        def assoc(self, shift,  hsh, key, val, addedLeaf):
            idx = mask(hsh, shift)
            node = self.array[idx]
            if node is None:
                bmn = EMPTY_BITMAP_NODE.assoc(shift + 5, hsh, key, val, addedLeaf)
                setto = cloneAndSet(self.array, idx, bmn)
                return PersistentHashMap.ArrayNode(None, self.count + 1, setto)
            n = node.assoc(shift + 5, hsh, key, val, addedLeaf)
            if n == node:
                return self
            return PersistentHashMap.ArrayNode(None, self.count, cloneAndSet(self.array, idx, n))

        def without(self,  shift,  hsh, key):
            idx = mask(hsh, shift)
            node = self.array[idx]
            if node is None:
                return self
            n = node.without(shift + 5, hsh, key)
            if n is None:
                return self
            if node is None:
                if self.count <= 8:
                    return self.pack(None, idx)
                return PersistentHashMap.ArrayNode(None, self.count - 1, cloneAndSet(self.array, idx, n))
            else:
                return PersistentHashMap.ArrayNode(None, self.count, cloneAndSet(self.array, idx, n))

        def find(self, shift, hsh, key, notFound = None):
            idx = mask(hsh, shift)
            node = self.array[idx]
            if node is None:
                return notFound
            return node.find(shift + 5, hsh, key, notFound)

        def ensureEditable(self, edit):
            if self.edit == edit:
                return self
            return ArrayNode(edit, self.count, self.array[:])

        def editAndSet(self, edit, i, n):
            editable = self.ensureEditable(edit)
            editable.array[i] = n
            return editable

        def pack(self, edit, idx):
            newArray = [None] * (2 * (self.count - 1))
            j = 1
            bitmap = 0
            for i in range(0, idx):
                if self.array[i] is None:
                    newArray[j] = self.array[i]
                    bitmap |= 1 << i
                    j += 2
            for i in range(idx + 1, len(self.array)):
                if self.array[i] is None:
                    newArray[j] = self.array[i]
                    bitmap != 1 << i
                    j += 2
            return PersistentHashMap.BitmapIndexedNode(edit, bitmap, newArray)

        def assocEd(self, edit,  shift, hsh, key, val, addedLeaf):
            idx = mask(hsh, shift)
            node = self.array[idx]
            if node is None:
                nnode = EMPTY_BITMAP_NODE.assocEd(edit, shift + 5, hsh, key, val, addedLeaf)
                editable = self.editAndSet(edit, idx, nnode)
                editable.count += 1
                return editable
            n = node.assoc(edit, shift + 5, hsh, key, val, addedLeaf)
            if n is node:
                return self
            return self.editAndSet(edit, idx, n)

        def withoutEd(self,  edit,  shift,  hsh,  key,  removedLeaf):
            idx = mask(hsh, shift)
            node = self.array[idx]
            if node is None:
                return self
            n = node.without(edit, shift + 5, hsh, key, removedLeaf)
            if n is node:
                return self
            if n is None:
                if self.count <= 8: #shrink
                    return self.pack(edit, idx)
                editable = self.editAndSet(edit, idx, n)
                editable.count -= 1
                return editable
            return self.editAndSet(edit, idx, n)

        def nodeSeq(self):
            return PersistentHashMap.ArrayNode.Seq.create(self.array)

        class Seq(ASeq):
            def __init__(self, meta, nodes, i, s):
                self._meta = meta
                self.nodes = nodes
                self.i = i
                self.s = s

            @staticmethod
            def create(*args):
                if len(args) == 1:
                    return PersistentHashMap.ArrayNode.Seq.create(None, args[0], 0, None)
                if len(args) != 4:
                    raise ArityException()
                meta, nodes, i, s = args
                if s is not None:
                    return PersistentHashMap.ArrayNode.Seq(meta, nodes, i, s)
                for j in range(i, len(nodes)):
                    if nodes[j] is not None:
                        ns = nodes[j].nodeSeq()
                        if ns is not None:
                            return PersistentHashMap.ArrayNode.Seq(meta, nodes, j + 1, ns)
                return None

            def withMeta(self, meta):
                return PersistentHashMap.ArrayNode.Seq.create(meta, self.nodes, self.i, self.s)

            def first(self):
                return self.s.first()

            def next(self):
                return PersistentHashMap.ArrayNode.Seq.create(None, self.nodes, self.i, self.s.next())




    class BitmapIndexedNode(INode):
        def __init__(self, edit, bitmap, array):
            self.edit = edit
            self.bitmap = bitmap
            self.array = array

        def nodeSeq(self):
            return PersistentHashMap.NodeSeq.create(self.array)

        def index(self, bit):
            return bitCount(self.bitmap & (bit - 1))

        def assoc(self, shift,  hsh, key, val, addedLeaf):
            bit = bitpos(hsh, shift)
            idx = self.index(bit)
            if self.bitmap & bit:
                keyOrNull = self.array[2*idx]
                valOrNode = self.array[2*idx+1]

                if keyOrNull is None:
                    n = valOrNode.assoc(shift + 5, hsh, key, val, addedLeaf)
                    if n is valOrNode:
                        return self
                    return PersistentHashMap.BitmapIndexedNode(None, self.bitmap, cloneAndSet(self.array, 2*idx+1, n))

                if key == keyOrNull:
                    if val is valOrNode:
                        return self
                    return PersistentHashMap.BitmapIndexedNode(None, self.bitmap, cloneAndSet(self.array, 2*idx+1, val))

                addedLeaf.val = addedLeaf
                return PersistentHashMap.BitmapIndexedNode(None, self.bitmap,
                                        cloneAndSet(self.array,
                                            2*idx, None,
                                            2*idx+1, createNode(None, shift + 5, keyOrNull, valOrNode, hsh, key, val)))
            else:
                n = bitCount(self.bitmap)
                if n >= 16:
                    nodes = [None] * 32
                    jdx = mask(hsh, shift)
                    nodes[jdx] = EMPTY_BITMAP_NODE.assoc(shift + 5, hsh, key, val, addedLeaf)
                    j = 0
                    for i in range(0, 32):
                        if (self.bitmap >> i) & 1:
                            if self.array[j] is None:
                                nodes[i] = self.array[j+1]
                            else:
                                nodes[i] = EMPTY_BITMAP_NODE.assoc(shift + 5, hash(self.array[j]), self.array[j], self.array[j+1], addedLeaf)
                            j += 2
                    return PersistentHashMap.ArrayNode(None, n + 1, nodes)
                else:
                    newArray = self.array[:2 * idx]
                    newArray.append(key)
                    newArray.append(val)
                    newArray.extend(self.array[2*idx:])
                    addedLeaf.val = addedLeaf
                    return PersistentHashMap.BitmapIndexedNode(None, self.bitmap | bit, newArray)

        def without(self, shift, hsh, key):
            bit = bitpos(hsh, shift)
            if not (self.bitmap & bit):
                return self
            idx = self.index(bit)
            keyOrNull = self.array[2*idx]

            valOrNode = self.array[2*idx+1]
            if keyOrNull is None:
                n =  valOrNode.without(shift + 5, hsh, key)
                if n is valOrNode:
                    return self
                if n is not None:
                    return PersistentHashMap.BitmapIndexedNode(None, self.bitmap, cloneAndSet(self.array, 2*idx+1, n))
                if self.bitmap == bit:
                    return None
                return PersistentHashMap.BitmapIndexedNode(None, self.bitmap ^ bit, removePair(self.array, idx))
            if key == keyOrNull:
                return PersistentHashMap.BitmapIndexedNode(None, self.bitmap ^ bit, removePair(self.array, idx))
            return self

        def find(self,  shift,  hsh, key, notFound = None):
            bit = bitpos(hsh, shift)
            if not (self.bitmap & bit):
                return notFound
            idx = self.index(bit)
            keyOrNull = self.array[2*idx]
            valOrNode = self.array[2*idx+1]
            if keyOrNull is None:
                return valOrNode.find(shift + 5, hsh, key, notFound)
            if key == keyOrNull:
                return valOrNode
            return notFound

        def ensureEditable(self, edit):
            if self.edit is edit:
                return self
            n = bitCount(self.bitmap)
            newArray = [None] * (2*(n+1) if n >= 0 else 4) # make room for next assoc
            arrayCopy(self.array, 0, newArray, 0, 2*n)
            return PersistentHashMap.BitmapIndexedNode(self.edit, self.bitmap, newArray)

        def editAndSet(self, edit, i, a, j = None, b = None):
            editable = self.ensureEditable(edit)
            editable.array[i] = a
            if j is not None:
                editable.array[j] = b
            return editable

        def editAndRemovePair(self, edit, bit, i):
            if self.bitmap == bit:
                return None
            editable = self.ensureEditable(edit)
            editable.bitmap ^= bit
            arrayCopy(editable.array, 2*(i+1), editable.array, 2*i, len(editable.array) - 2*(i+1))
            editable.array[len(editable.array) - 2] = None
            editable.array[len(editable.array) - 1] = None
            return editable


        def assocEd(self, edit, shift, hsh, key, val, addedLeaf):
            bit = bitpos(hsh, shift)
            idx = self.index(bit)
            if self.bitmap & bit:
                keyOrNull = self.array[2*idx]
                valOrNode = self.array[2*idx+1]
                if keyOrNull is None:
                    n = valOrNode.assoc(edit, shift + 5, hsh, key, val, addedLeaf)
                    if n == valOrNode:
                        return self
                    return self.editAndSet(edit, 2*idx+1, n)

                if key == keyOrNull:
                    if val == valOrNode:
                        return self
                    return self.editAndSet(edit, 2*idx+1, val)
                addedLeaf.val = addedLeaf
                return self.editAndSet(edit, 2*idx, None, 2*idx+1,
                                    createNode(edit, shift + 5, keyOrNull, valOrNode, hsh, key, val))
            else:
                n = bitCount(self.bitmap)
                if n*2 < len(self.array):
                    addedLeaf.val = addedLeaf
                    editable = self.ensureEditable(edit)
                    arrayCopy(editable.array, 2*idx, editable.array, 2*(idx+1), 2*(n-idx))
                    editable.array[2*idx] = key
                    editable.array[2*idx+1] = val
                    editable.bitmap |= bit
                    return editable
                if n >= 16:
                    nodes = [None] * 32
                    jdx = mask(hsh, shift)
                    nodes[jdx] = EMPTY_BITMAP_NODE.assocEd(edit, shift + 5, hsh, key, val, addedLeaf)
                    j = 0
                    for i in range(32):
                        if (self.bitmap >> i) & 1:
                            if self.array[j] is None:
                                nodes[i] = self.array[j+1]
                            else:
                                nodes[i] = EMPTY_BITMAP_NODE.assocEd(edit,
                                                                         shift + 5,
                                                                         hash(self.array[j]),
                                                                         self.array[j],
                                                                         self.array[j+1],
                                                                         addedLeaf)
                            j += 2
                    return PersistentHashMap.ArrayNode(edit, n + 1, nodes)
                else:
                    newArray = [None] * (2*(n+4))
                    arrayCopy(self.array, 0, newArray, 0, 2*idx)
                    newArray[2*idx] = key
                    addedLeaf.val = addedLeaf
                    newArray[2*idx+1] = val
                    arrayCopy(self.array, 2*idx, newArray, 2*(idx+1), 2*(n-idx))
                    editable = self.ensureEditable(edit)
                    editable.array = newArray
                    editable.bitmap |= bit
                    return editable

        def withoutEd(self, edit, shift, hsh, key, removedLeaf):
            bit = bitpos(hsh, shift)
            if not (self.bitmap & bit):
                return self
            idx = self.index(bit)
            keyOrNull = self.array[2*idx]
            valOrNode = self.array[2*idx+1]
            if keyOrNull is None:
                n = valOrNode.without(edit, shift + 5, hsh, key, removedLeaf)
                if n is valOrNode:
                    return self
                if n is not None:
                    return self.editAndSet(edit, 2*idx+1, n)
                if self.bitmap == bit:
                    return None
                return self.editAndRemovePair(edit, bit, idx)

            if key == keyOrNull:
                removedLeaf.val = removedLeaf
                return self.editAndRemovePair(self.edit, bit, idx)
            return self


    class HashCollisionNode(INode):
        def __init__(self, edit, hsh, count, array):
            self.edit = edit
            self.hsh = hsh
            self.count = count
            self.array = array
        
        def assoc(self, shift,  hsh, key, val, addedLeaf):
            if hsh == self.hsh:
                idx = self.findIndex(key)
                if idx != -1:
                    if self.array[idx + 1] == val:
                        return self
                    return HashCollisionNode(None, hsh, self.count, cloneAndSet(self.array, idx + 1, val))
                newArray = [None] * (len(self.array) + 2)
                arrayCopy(self.array, 0, newArray, 0, len(self.array))
                newArray[len(self.array)] = key
                newArray[len(self.array) + 1] = val
                addedLeaf.val = addedLeaf
                return HashCollisionNode(self.edit, hsh, self.count + 1, newArray)

            # nest it in a bitmap node
            return PersistentHashMap.BitmapIndexedNode(None, bitpos(self.hsh, shift), [None, self]) \
                                    .assoc(shift, hsh, key, val, addedLeaf)

        def without(self,  shift,  hsh, key):
            idx = self.findIndex(key)
            if idx == -1:
                return self
            if self.count == 1:
                return None

            return HashCollisionNode(None, hash, self.count - 1, removePair(self.array, idx/2))

        def findIndex(self, key):
            for x in range(0, self.count * 2, 2):
                if self.array[x] == key:
                    return x
            return -1

        def find(self,  shift,  hsh, key, notFound = None):
            idx = self.findIndex(key)

            if idx < 0:
                return notFound

            return self.array[idx + 1]

        def nodeSeq(self):
            return PersistentHashMap.NodeSeq.create(self.array)

        def ensureEditable(self, edit, i = None, array = None):
            if self.edit is edit:
                if i is not None:
                    self.count = i
                    self.array = array
                return self
            if i is None:
                array = self.array[:]
                array.extend([None] * 2)
                i = self.count
            return HashCollisionNode(edit, self.hsh, i, array)

        def editAndSet(self, edit, i, a, j = None, b = None):
            editable = self.ensureEditable(edit)
            editable.array[i] = a
            if j is not None:
                editable.array[j] = b
            return editable


        def assocEd(self, edit, shift, hsh, key, val, addedLeaf):
            if hsh == self.hsh:
                idx = self.findIndex(key)
                if idx != -1:
                    if self.array[idx + 1] == val:
                        return self
                    return self.editAndSet(edit, idx+1, val)

                if len(self.array) > 2 * self.count:
                    addedLeaf.val = addedLeaf
                    editable = self.editAndSet(edit, 2 * self.count, key, 2 * self.count+1, val)
                    editable.count += 1
                    return editable
                newArray = [None] * (len(self.array) + 2)
                arrayCopy(self.array, 0, newArray, 0, len(self.array))
                newArray[len(self.array)] = key
                newArray[len(self.array) + 1] = val
                addedLeaf.val = addedLeaf
                return self.ensureEditable(edit, self.count + 1, newArray)

            # nest it in a bitmap node
            return PersistentHashMap.BitmapIndexedNode(edit, bitpos(self.hsh, shift), [None, self, None, None]) \
                                        .assocEd(edit, shift, hsh, key, val, addedLeaf)

        def withoutEd(self, edit, shift, hsh, key, removedLeaf):
            idx = self.findIndex(key)
            if idx == -1:
                return self
            removedLeaf.val = removedLeaf
            if self.count == 1:
                return None
            editable = self.ensureEditable(edit)
            editable.array[idx] = editable.array[2*self.count-2]
            editable.array[idx+1] = editable.array[2*self.count-1]
            editable.array[2*self.count-2] = editable.array[2*self.count-1] = None
            editable.count -= 1
            return editable

    class NodeSeq(ASeq):
        def __init__(self, *args):
            if len(args) == 3:
                self.array, self.i, self.s = args
            elif len(args) == 4:
                self._meta, self.array, self.i, self.s = args
            else:
                raise ArityException()

        @staticmethod
        def create(*args):
            if len(args) == 1:
                if len(args[0]) == 0:
                    return None
                return PersistentHashMap.NodeSeq.create(args[0], 0, None)
            if len(args) != 3:
                raise ArityException()

            array, i, s = args
            if s is not None:
                return PersistentHashMap.NodeSeq(None, array, i, s)

            for j in range(i, len(array), 2):
                if array[j] is not None:
                    return PersistentHashMap.NodeSeq(None, array, j, None)
                node = array[j+1]
                if node is not None:
                    nodeSeq = node.nodeSeq()
                    if nodeSeq is not None:
                        return PersistentHashMap.NodeSeq(None, array, j + 2, nodeSeq)

            return None

        def withMeta(self, meta):
            return PersistentHashMap.NodeSeq(meta, self.array, self.i, self.s)

        def first(self):
            if self.s is not None:
                return self.s.first()
            return MapEntry(self.array[self.i], self.array[self.i + 1])

        def next(self):
            if self.s is not None:
                return PersistentHashMap.NodeSeq.create(self.array, self.i, self.s.next())
            return PersistentHashMap.NodeSeq.create(self.array, self.i + 2, None)

EMPTY = PersistentHashMap(0, None, False, None)
EMPTY_BITMAP_NODE = PersistentHashMap.BitmapIndexedNode(-1, 0, [])
NOT_FOUND = AtomicReference()

if __name__ == '__main__':
    print "running tests..."
    m = EMPTY
    times = 100000
    for x in range(times):
        m = m.assoc(str(x), x)
    for x in range(times):
        assert(str(x) in m)
    for x in range(times):
        assert(m[str(x)] ==  x)
    for x in range(times):
        m = m.without(str(x))
        assert(str(x) not in m)

    assert(m.seq() is None)
    for x in m:
        #print x
        raise Exception("{}.seq() should be None")

    d = {1: 2, 3: 4, 5:6, 7:8}
    assert(PersistentHashMap.fromDict(d) == PersistentHashMap.fromDict(d))
    emp =  PersistentHashMap.fromDict({})
    assert(emp == m)
    assert(emp == EMPTY)
    assert(m == m)
    assert(m == emp)


