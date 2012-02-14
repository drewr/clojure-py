(ns tests.core)
(import unittest)

(defmacro deftest
    [name & body]
    (let [f `(~'deftype ~name []
            unittest/TestCase
            (~'__init__ [~'self]
                 (unittest.TestCase/__init__ ~'self (.-name 'runTest)))
            (~'setUp [~'self] nil)
            (~'test_run [~'self]
                (let-macro [~'assert-equal (.-assertEqual ~'self)
                            ~'assert-not-equal (.-assertNotEqual ~'self)
                            ~'assert-true (.-assertTrue ~'self)
                            ~'assert-false (.-assertFalse ~'self)]
                            ~@body)))]
              
        (print f)
        f))



(deftest AndTests
    (assert-true (and true true true false))
    (assert-false (and true false false))
    (assert-false (and false false false)))


(unittest.main)

