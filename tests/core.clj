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

(deftest quot-tests
    (assert-equal (quot 23 7) 3))

(deftest rem-tests
    (assert-equal (rem 23 7) 2))

(deftest bit-not-tests
    (assert-equal (bit-not 5) -6))

(deftest bit-and-tests
    (assert-equal (bit-and 5 4) 4)
    (assert-equal (bit-and 5 4 1) 0))

(deftest bit-or-tests
    (assert-equal (bit-or 6 5 4 2) 7))

(deftest bit-xor-tests
    (assert-equal (bit-xor 2 3 4) 5))

(deftest bit-and-not-tests
    (assert-equal (bit-and-not 3 1 2) 0))

(deftest bit-shift-left-tests
    (assert-equal (bit-shift-left 1 3) 8))

(deftest bit-shift-right-tests
    (assert-equal (bit-shift-right 8 3) 1))

(deftest bit-clear-tests
    (assert-equal (bit-clear 3 1) 1))

(deftest bit-set-tests
    (assert-equal (bit-set 0 1) 2))

(deftest bit-flip-tests
    (assert-equal (bit-flip 0 1) 2)
    (assert-equal (bit-flip 2 1) 0))

(deftest bit-flip-tests
    (assert-true (bit-test 3 1))
    (assert-false (bit-test 1 1)))

(deftest integer?-tests
    (assert-true (integer? 1))
    (assert-false (integer? "1")))

(deftest even?-tests
    (assert-true (even? 2))
    (assert-false (even? 1)))

(deftest odd?-tests
    (assert-true (odd? 1))
    (assert-false (odd? 2)))

(deftest complement-tests
    (assert-true ((complement (fn [] false))))
    (assert-true ((complement (fn [x] false)) 1))
    (assert-true ((complement (fn [x y] false)) 1 2))
    (assert-true ((complement (fn [x y z] false)) 1 2 3)))

(deftest constantly-tests
    (assert-equal ((constantly 1) 1 2 3 4 5) 1))

(deftest identityi-tests
    (assert-equal (identity 3) 3)
    (assert-equal (identity 4) 4))

(deftest peek-tests
    (assert-equal (peek '(1 2)) 1)
    (assert-equal (peek nil) nil))

(deftest pop-tests
    (assert-equal (pop '(1 2)) '(2))
    (assert-equal (pop nil) nil))

;;map stuff

(deftest contains?-tests
    (assert-true (contains? [4 4 4 4] 3))
    (assert-true (contains? {:a 1 :b 2} :a))
    (assert-false (contains? [1 1 1] 4))
    (assert-false (contains? {:a 4} :b)))

(deftest get-tests
    (assert-equal (get {:a 1} :a) 1)
    (assert-equal (get "abc" 1) "b"))

(deftest dissoc-tests
    (assert-equal (dissoc {:a 1 :b 2} :b) {:a 1})
    (assert-equal (dissoc {:a 1 :b 2} :a :b) {}))

(deftest find-tests
    (assert-equal (.getKey (find {:a 1} :a)) :a)
    (assert-equal (.getValue (find {:a 1} :a)) 1))

(deftest select-keys-tests
    (assert-equal (select-keys {:a 1 :b 2 :c 3} [:a]) {:a 1}))

(deftest keys-tests
    (assert-equal (keys {:a 1 :b 2}) [:b :a]))

(deftest vals-tests
    (assert-equal (vals {:a 1 :b 2}) [2 1]))

(deftest key-tests
    (assert-equal (key (find {:a 1 :b 2} :b)) :b))

(deftest val-tests
    (assert-equal (val (find {:a 1 :b 2} :b)) 2))

