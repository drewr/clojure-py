;; These are functions that have to wait until new 
;; compiler functionality is implemented

(def
 ^{:arglists '([x])
   :doc "Return true if x is a Character"
   :added "1.0"
   :static true}
 char? (fn ^:static char? [x] (if (string? Character x)))

(defn cast
  "Throws a ClassCastException if x is not a c, else returns x."
  {:added "1.0"
   :static true}
  [^Class c x]
  (. c (cast x)))


(defn cast
  "Throws a ClassCastException if x is not a c, else returns x."
  {:added "1.0"
   :static true}
  [^Class c x] 
  (. c (cast x)))

(defn to-array
  "Returns an array of Objects containing the contents of coll, which
  can be any Collection.  Maps to java.util.Collection.toArray()."
  {:tag "[Ljava.lang.Object;"
   :added "1.0"
   :static true}
  [coll] (. clojure.lang.rt (toArray coll)))

(defn vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
  ([a b c d & args]
     (. clojure.lang.LazilyPersistentVector (create (cons a (cons b (cons c (cons d args))))))))

