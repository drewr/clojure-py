(defn + [x y] (py.bytecode/BINARY_ADD x y))
(defn - [x y] (py.bytecode/BINARY_SUBTRACT x y))
(defn fib 
    ([x] (fib 0 1 x))
    ([current next remaining]
        (if (= 0 remaining)
            current
            (recur next (+ current next) (- remaining 1)))))

