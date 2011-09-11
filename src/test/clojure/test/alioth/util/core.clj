(ns test.alioth.util.core
    (:use [alioth.util.core]
          [clojure.test])
    (:import [java.io ByteArrayInputStream]))

(deftest test-streams=
  (let [bais (fn [s] (ByteArrayInputStream. (byte-array (map byte s))))]
    (are [description inputs expected] (= expected (apply streams= inputs))
         "one stream" [(bais [0 1 2])] true
         "two equal streams" (map bais [[0 1 2] [0 1 2]]) true 
         "many equal streams" (repeat 10 (bais (repeat 10 0))) true
         "two unequal streams" (map bais [[0 1 2] [0 1 1]]) false
         "many unequal streams" (conj (repeat 10 (bais (repeat 10 0))) 
                                      (bais (repeat 9 0))) false 
         )))

(deftest test-ns-resource
  (let [parent-ns (create-ns 'test.alioth.util)]
    (is (ns-resource parent-ns "core.clj"))
    (is (not (ns-resource parent-ns "CAFEBABE.clj")))))
