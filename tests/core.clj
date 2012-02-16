(ns tests.core)


(defn assert [val]
    (if val true 
            (throw (AssertionError))))


(assert (and true true true))
(assert (and true false true))



