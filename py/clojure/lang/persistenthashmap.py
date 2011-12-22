from apersistentmap import APersistentMap
from atransientmap import ATransientMap
from exceptions import ArityException, AbstractMethodCall
from ieditablecollection import IEditableCollection
from iobj import IObj
from aseq import ASeq

def mask(hash, shift):
    return (hash >> shift) & 0x01f;

def cloneAndSet(array, i, a, j = None, b = None):
    clone = array[:]
    clone[i] = a
    if j:
        clone[j] = b
    return clone

class PersistentHashMap(APersistentMap, IEditableCollection, IObj):
    def __init__(self, *args):
        if len(args) == 4:
            self._meta = None
            self.count = args[0]
            self.root = args[1]
            self.hasNull = args[2]
            self.nullValue = args[3]
        elif len(args) == 5:
            self._meta = args[0]
            self.count = args[1]
            self.root = args[2]
            self.hasNull = args[3]
            self.nullValue = args[4]
        else:
            raise ArityException()


    class INode(object):
        def assoc(self, shift,  hash, key, val, addedLeaf):
            raise AbstractMethodCall()

        def without(self,  shift,  hash, key):
            raise AbstractMethodCall()

        def find(self,  shift,  hash, key, notFound = None):
            raise AbstractMethodCall()

        def nodeSeq(self):
            raise AbstractMethodCall()

        def assocLeaf(self, edit,  shift, hash, key, val, addedLeaf):
            raise AbstractMethodCall()

        def withoutLeaf(self,  edit,  shift,  hash,  key,  removedLeaf):
            raise AbstractMethodCall()

    class ArrayNode(INode):
        def __init__(self, edit, count, array):
            self.edit = edit
            self.count = count
            self.array = array

        def assoc(self, shift,  hash, key, val, addedLeaf):
            idx = mask(hash, shift)
            node = self.array[idx]
            if node is None:
                bmn = BitmapIndexedNode.EMPTY.assoc(shift + 5, hash, key, val, addedLeaf)
                setto = cloneAndSet(self.array, idx, bmn)
                return PersistentMap.ArrayNode(None, self.count + 1, setto)
            n = node.assoc(shift + 5, hash, key, val, addedLeaf);
            if(n == node):
                return self
            return PersistentMap.ArrayNode(null, count, cloneAndSet(array, idx, n))

        def without(self,  shift,  hash, key):
            idx = mask(hash, shift)
            node = self.array[idx]
            if node is None:
                return self
            n = node.without(shift + 5, hash, key)
            if n is None:
                return self
            if node is None:
                if self.count <= 8:
                    return pack(null, idx)
                return PersistentMap.ArrayNode(None, self.count - 1, cloneAndSet(self.array, self.idx, n))
            else:
                return PersistentMap.ArrayNode(None, self.count, cloneAndSet(self.array, idx, n))

        def find(self, shift, hash, key, notFound = None):
            idx = mask(hash, shift);
            node = self.array[idx];
            if node is None:
                return notFound;
            return node.find(shift + 5, hash, key, notFound);

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
            return BitmapIndexedNode(edit, bitmap, newArray)

        def assocLeaf(self, edit,  shift, hash, key, val, addedLeaf):
            idx = mask(hash, shift)
            node = self.array[idx]
            if node is None:
                nnode = BitmapIndexedNode.EMPTY.assoc(edit, shift + 5, hash, key, val, addedLeaf)
                editable = self.editAndSet(edit, idx, nnode)
                editable.count += 1
                return editable
            n = node.assoc(edit, shift + 5, hash, key, val, addedLeaf)
            if n is node:
                return self
            return self.editAndSet(edit, idx, n)

        def withoutLeaf(self,  edit,  shift,  hash,  key,  removedLeaf):
            idx = mask(hash, shift)
            node = self.array[idx]
            if node is None:
                return self
            n = node.without(edit, shift + 5, hash, key, removedLeaf)
            if n is node:
                return self
            if n is null:
                if self.count <= 8: #shrink
                    return self.pack(edit, idx)
                editable = self.editAndSet(edit, idx, n)
                editable.count -= 1
                return editable
            return self.editAndSet(edit, idx, n)

        class Seq(ASeq):
            def __init__(self, meta, nodes, i, s):
                self._meta = meta
                self.node = nodes
                self.i = i
                self.s = s
            @staticmethod
            def create(self, *args):
                if len(args) == 1:
                    return PersistentHashMap.ArrayNode.Seq(None, args[0], 0, None)
                if len(args) != 4:
                    raise ArityException()
                meta = args[0]
                nodes = args[1]
                i = args[2]
                s = args[3]
                if s is None:
                    return PersistentHashMap.ArrayNode.Seq(None, nodes, i, s)
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
                return PersistentHashMap.ArrayNode.Seq.create(None, self.nodes, i, self.s.next())


