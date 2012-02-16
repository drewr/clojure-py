(ns tests.core)


(defn assert-true [val]
    (if val true 
            (throw (AssertionError (str val " is not true")))))

(defn assert-false [val]
    (if val (throw (AssertionError (str val " is not false")))
            false))

(defmacro deftest [name & body]
    `((~'fn ~name [] ~@body)))


(deftest if-not-tests
    (assert-true (if-not false true))
    (assert-false (if-not true true false))
    (assert-true (if-not true false true)))

(deftest and-tests
    (assert-true (and true true true))
    (assert-false (and true false true))
    (assert-false (and nil true)))



