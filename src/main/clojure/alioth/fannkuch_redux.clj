;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=fannkuchredux&lang=all
;   Inspired by http://shootout.alioth.debian.org/u64q/program.php?test=fannkuchredux&lang=java&id=1

(ns alioth.fannkuch-redux
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const chunk-size 150)

(defmacro for-loop [[binding pred adv] & body]
  `(loop [~@binding]
     (when ~pred
       ~@body
       (recur ~adv))))

(defn print-result [n res chk]
  (println (format "%d\nPfannkuchen(%d) = %d" chk n res)))

(defn -main [& args]
  (let [n (if-let [n (first args)] (Integer/parseInt n) 12)]
    (let [fact (long-array (inc n))]
      (aset fact 0 1)
      (for-loop [(i 1) (< i (alength fact)) (inc i)]
        (aset fact i (* (aget fact (dec i)) i)))
      )))