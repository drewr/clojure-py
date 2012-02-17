(ns tests.core)


(defn assert-true [val]
    (if val true 
            (throw (AssertionError (str val " is not true")))))

(defn assert-false [val]
    (if val (throw (AssertionError (str val " is not false")))
            false))

(defn assert-greater [x y]
    (if (py.bytecode/COMPARE_OP ">" x y)
        true
        (throw (AssertionError (str x " is not greater than " y)))))

(defn assert-lesser [x y]
    (if (py.bytecode/COMPARE_OP "<" x y)
        true
        (throw (AssertionError (str x " is not greater than " y)))))

(defn assert-equal [x y]
    (if (py.bytecode/COMPARE_OP "==" x y)
        true
        (throw (AssertionError (str x " does not equal " y)))))


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

(deftest identical?-tests
    (assert-true (identical? 1 1))
    (assert-false (identical? 1 2))
    (assert-false (identical? '() '()))
    (let [tmp '()]
         (assert-true (identical? tmp tmp))))

(deftest compare-tests
    (assert-lesser (compare 0 1) 0)
    (assert-lesser (compare -1 0) 0))

(deftest or-tests
    (assert-true (or true false false))
    (assert-false (or false false false))
    (assert-true (or false true false)))

(deftest zero?-tests
    (assert-true (zero? 0))
    (assert-false (zero? 1)))

(deftest count-tests
    (assert-equal (count '()) 0)
    (assert-equal (count '(1)) 1))

(deftest int
    (assert-equal (int "1") 1)
    (assert-equal (int "-1") -1))

(deftest <-tests
    (assert-true (< 1))
    (assert-true (< 1 2))
    (assert-true (< 1 2 3))
    (assert-true (< 1 2 3 4))
    (assert-false (< 1 3 2 4)))

(deftest reduce1-tests
    (assert-equal (reduce1 (fn [x y] (py.bytecode/BINARY_ADD x y))
                            '(1 2 3 4)) 10)
    (assert-equal (reduce1 (fn [x y] (py.bytecode/BINARY_ADD x y))
                            5 '(1 2 3 4)) 15))
(deftest reverse-tests
    (assert-equal (reverse [1 2 3 4 5]) [5 4 3 2 1]))

(deftest >1?-tests
    (assert-true (>1? 3))
    (assert-false (>1? 0)))

(deftest >0?-tests
    (assert-true (>0? 2))
    (assert-false (>0? -1)))


(deftest +-tests
    (assert-equal (+) 0)
    (assert-equal (+ 1) 1)
    (assert-equal (+ 1 2) 3)
    (assert-equal (+ 1 2 3 4) 10))

(deftest *-tests
    (assert-equal (*) 1)
    (assert-equal (* 1) 1)
    (assert-equal (* 1 2) 2)
    (assert-equal (* 1 2 3 4) 24))

(deftest /-tests
    (assert-equal (/ 1) 1)
    (assert-equal (/ 2 2) 1)
    (assert-equal (/ 20 5 2) 2))

(deftest /-tests
    (assert-equal (- 1) -1)
    (assert-equal (- 2 2) 0)
    (assert-equal (- 20 5 2) 13))

(deftest <=-tests
    (assert-true (<= 1))
    (assert-true (<= 1 2))
    (assert-true (<= 1 1 3 4))
    (assert-false (<= 2 1 1 1)))

(deftest >-tests
    (assert-true (> 4))
    (assert-true (> 4 3))
    (assert-true (> 4 3 2 1))
    (assert-false (> 4 3 2 2)))

(deftest >=-tests
    (assert-true (>= 4))
    (assert-true (>= 4 3))
    (assert-true (>= 4 3 3))
    (assert-false (>= 3 4 2 1)))

(deftest ==-tests
    (assert-true (== 1))
    (assert-true (== 1 1))
    (assert-true (== 1 1 1 1 1))
    (assert-false (== 1 2 1 1 1)))

(deftest max-tests
    (assert-equal (max 1) 1)
    (assert-equal (max 1 2) 2)
    (assert-equal (max 3 2 1) 3))

(deftest min-tests
    (assert-equal (min 1) 1)
    (assert-equal (min 1 2) 1)
    (assert-equal (min 3 2 1) 1))

(deftest pos?-tests
    (assert-true (pos? 1))
    (assert-false (pos? -1))
    (assert-false (pos? 0)))

(deftest neg?-tests
    (assert-true (neg? -1))
    (assert-false (neg? 1))
    (assert-false (neg? 0)))

(deftest quot
    (assert-equal (quot 23 7) 3))

(deftest rem
    (assert-equal (rem 23 7) 2))


