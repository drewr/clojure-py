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
    (assert-equal (keys {:a 1}) [:a]))

(deftest vals-tests
    (assert-equal (vals {:a 1}) [1]))

(deftest key-tests
    (assert-equal (key (find {:a 1 :b 2} :b)) :b))

(deftest val-tests
    (assert-equal (val (find {:a 1 :b 2} :b)) 2))

(deftest name-tests
    (assert-equal (name 'Foo) "Foo")
    (assert-equal (name "Foo") "Foo"))

(deftest namespace-tests
    (assert-equal (namespace 'baz/Foo) "baz")
    (assert-equal (namespace 'Foo) nil))

; broken need to fix
;(deftest dot-dot-tests
;    (assert-equal (.. :foo (.-sym) (.-name)) ":foo"))

(deftest ->-tests
    (assert-equal (-> " baz " (.rstrip) (.lstrip)) "baz"))

(deftest ->>-tests ; haven't a clue how to test this
    )


;;; var stuff


;;; if-let and when-let tests are from
;;; http://blog.jayfields.com/2011/03/clojure-if-let-and-when-let.html

(deftest if-let-tests
	(assert-equal (if-let [a 4] (+ a 4) (+ 10 10)) 8)
	(assert-equal (if-let [a nil] (+ a 4) (+ 10 10)) 20))


(deftest when-let
    (assert-equal (when-let [a 9] (+ a 4))  13)
    (assert-equal (when-let [a nil] (+ a 4)) nil))

;;; functional stuff

(deftest comp-tests
    (assert-equal ((comp str +) 8 8 8) "24"))

(deftest juxt-tests
    (assert-equal ((juxt :a :b :c :d) {:a 1 :b 2 :c 3 :d 4}) [1 2 3 4]))

(deftest partial-tests
    (assert-equal ((partial + 1) 1) 2))

;;; sequence stuff

(deftest sequence-tests
    (assert-equal (sequence [1 2 3]) '(1 2 3)))

(deftest every?-tests
    (assert-true (every? even? '(2 4 6)))
    (assert-false (every? even? '(1 4 6))))

(deftest every?-tests
    (assert-false (not-every? even? '(2 4 6)))
    (assert-true (not-every? even? '(1 4 6))))

(deftest some-tests
    (assert-true (some even? '(1 2 3 4)))
    (assert-equal (some even? '(1 3 5 7)) nil))

(deftest not-any?-tests
    (assert-true (not-any? odd? '(2 4 6)))
    (assert-false (not-any? odd? '(1 2 3))))

;(deftest dotimes-tests
;    (dotimes [n 5] (assert-true (and (>= n 0) (< n 5)))))

(deftest map-tests
    (assert-equal (map inc [1 2 3 4 5]) (seq [2 3 4 5 6])))

(deftest mapcat-tests
    (assert-equal (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]]) [0 1 2 3 4 5 6 7 8 9]))
 
(deftest filter-tests
    (assert-equal (filter even? [1 2 3 4 5]) [2 4]))

(deftest remove-tests
    (assert-equal (remove even? [1 2 3 4 5]) [1 3 5]))

(deftest take-tests
    (assert-equal (take 2 [1 2 3 4]) [1 2]))

(deftest take-while
    (assert-equal (take-while even? [2 2 1 1]) [2 2]))

(deftest drop-tests
    (assert-equal (drop 1 [1 2 3]) [2 3]))

(deftest drop-last-tests
    (assert-equal (drop-last 2 [1 2 3 4]) [1 2]))

(deftest take-last-tests
    (assert-equal (take-last 3 [1 2 3 4]) [2 3 4]))

(deftest drop-while-tests
    (assert-equal (drop-while even? [2 4 6 1 2 3]) [1 2 3]))

(deftest cycle-tests
    (assert-equal (take 6 (cycle [1 2 3])) [1 2 3 1 2 3]))

(deftest split-at-tests
    (assert-equal (split-at 3 [1 2 3 4 5]) [[1 2 3] [4 5]]))

(deftest split-with-tests
    (assert-equal (split-with odd? [1 1 1 1 2 2 2 2]) [[1 1 1 1] [2 2 2 2]]))

(deftest repeat-tests
    (assert-equal (repeat 3 1) [1 1 1]))

(deftest interate-tests
    (assert-equal (take 3 (iterate inc 0)) [0 1 2]))

(deftest range-tests
    (assert-equal (range 0 8 2) [0 2 4 6]))

(deftest merge-tests
    (assert-equal (merge {:a 1 :b 2} {:a 3 :c 4}) {:a 3 :b 2 :c 4}))

(deftest merge-with-tests
    (assert-equal (merge-with + 
                   {:a 1  :b 2}
                   {:a 9  :b 98 :c 0})
                  {:c 0, :a 10, :b 100}))


(deftest zipmap-tests
    (assert-equal (zipmap [:a :b :c :d :e] [1 2 3 4 5])
                    {:e 5, :d 4, :c 3, :b 2, :a 1}))


(deftest sort-tests
    (assert-equal (sort [3 1 2 4]) [1 2 3 4])
    (assert-equal (sort > (vals {:foo 5, :bar 2, :baz 10})) [10 5 2]))

(deftest sort-by-tests
    (assert-equal (sort-by first > [[1 2] [2 2] [2 3]]) [[2 2] [2 3] [1 2]]))

(deftype Accum [i]
    (inc [self] (py/setattr self "i" (inc i))))


(deftest dorun-tests
    (let [accum (Accum 0)]
         (dorun (map (fn [x] (.inc accum))
                     (range 10)))
         (assert-equal (.-i accum) 10)))

(deftest nthnext-tests
    (assert-equal (nthnext (range 10) 3) '(3 4 5 6 7 8 9)))


(deftest partition-tests
    (assert-equal (partition 4 (range 20)) 
                  '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (16 17 18 19)))
    (assert-equal (partition 4 6 ["a" "b" "c" "d"] (range 20))
                  '((0 1 2 3) (6 7 8 9) (12 13 14 15) (18 19 "a" "b"))))

(deftest eval-tests
    (assert-equal (eval '(+ 1 2)) 3))

(deftest doseq-tests
    (doseq [x [1 2 3]
                          y [1 2 3]]
                         (print (* x y))))
;; prints      
;;[1 2 3 2 4 6 3 6 9]

(deftest do-times
    (let [accum (Accum 0)]
         (dotimes [i 5]
             (assert-equal (.-i accum) i)
             (.inc accum))))

