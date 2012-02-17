(ns clojure.examples.factorial)

(defn fact [x]
    (loop [n x f 1]
        (if (= n 1)
            f
            (recur (dec n) (* f n)))))

(defn test [times]
    (loop [rem times]
        (if (> rem 0)
            (do (fact 20000)
                (recur (dec rem))))))

(test 20)
