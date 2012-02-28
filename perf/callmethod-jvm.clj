(ns perf.callmethod)
; Let's test some ways of calling functions. To do this we'll simply count
; how long it takes to count to some insanely high number over and over again

(def ^Integer highend 400000)
(def ^Integer interations 10000)

(defn ^Integer our-inc [^Integer x]
    (+ x 1))


; This first method is how we currently call functions. We call LOAD_GLOBAL
; every single time, and then execute it

(defn load-global-test [^Integer highend]
    (loop [^Integer x 0]
        (if (< x highend)
            (recur (our-inc x)))))


(print "load-global-test")
(time (dotimes [x interations] (load-global-test highend)))

