(ns derefperf)

(deftype V [i]
    (deref [self] i))


(deftype V2 [i]
    (deref [self] 1))

(def v (V 1))
(def v2 (V2 1))


(time (dotimes [x 10000]
        (dotimes [y 1000000]
            (let [z (+ x y)]
                (+ z (.deref v))))))

(time (dotimes [x 10000]
        (dotimes [y 1000000]
            (let [z (+ x y)]
                (+ z (.deref v2))))))

(time (dotimes [x 10000]
        (dotimes [y 1000000]
            (let [z (+ x y)]
                (+ z 1)))))
