(ns perf.callmethod)
; Let's test some ways of calling functions. To do this we'll simply count
; how long it takes to count to some insanely high number over and over again

(def highend 40000000)
(def interations 10)

(defn our-inc [x]
    (py.bytecode/BINARY_ADD x 1))


; This first method is how we currently call functions. We call LOAD_GLOBAL
; every single time, and then execute it

(defn load-global-test [highend]
    (loop [x 0]
        (if (< x highend)
            (recur (our-inc x)))))


(print "load-global-test")
(time (dotimes [x interations] (load-global-test highend)))


; Now we'll try something different. In this case we'll pass it in, not as a global
; but as a local

(defn with-local-test [f highend]
    (loop [x 0]
        (if (< x highend)
            (recur (f x)))))

(print "with-local-test")
(time (dotimes [x interations] (with-local-test our-inc highend)))

; Now let's try it as a real Var. 

(deftype V [itm]
    (deref [self] itm))

(def VInc (V our-inc))

(defn with-var-test [v highend]
    (loop [x 0]
        (if (< x highend)
            (recur ((.deref v) x)))))

(print "with-var-test")
(time (dotimes [x interations] (with-var-test VInc highend)))



; As if a binding were involved, but implemented as a python
; object for more performance (as a dict initial tests were about 500x slower



(deftype V2 [itm bindings]
    (deref [self] (.deref bindings)))

(def VInc2 (V2 nil (V our-inc)))

(defn with-binding-test [v highend]
    (loop [x 0]
        (if (< x highend)
            (recur ((.deref v) x)))))

(print "with-binding-test")

(time (dotimes [x interations] (with-binding-test VInc2 highend)))


; Functions loaded as constants

(defn with-const-test [highend]
    (loop [x 0]
        (if (< x highend)
            (recur ((py.bytecode/LOAD_CONST (fn [i] (py.bytecode/BINARY_ADD i 1))) x)))))

(print "with-const-test")

(time (dotimes [x interations] (with-const-test highend)))


; Inlined function call, this will be the fastest

(defn with-inline-test [highend]
    (loop [x 0]
        (if (< x highend)
            (recur (py.bytecode/BINARY_ADD x 1)))))

(print "with-inline-test")

(time (dotimes [x interations] (with-inline-test highend)))
