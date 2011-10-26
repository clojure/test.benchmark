;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=threadring
(ns alioth.thread-ring  
  (:import [java.util.concurrent Semaphore])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const AGENT_COUNT 503)
(def ^Semaphore finished-lock (Semaphore. 0))

(defn relay [state msg]
  (if (> msg 0)
    (send (:next state) relay (dec msg))
    (do (println (:id state))
        (.release finished-lock)))
  state)

(defn make-ring [n]
  (let [head (agent {:id 1}) 
        tail (reduce (fn [next idx] (agent {:id idx :next next}))
                     head (range n 1 -1))]
    (await (send head assoc :next tail))
    head))

(defn run [agent-count iterations]
  (let [head (make-ring agent-count)]
    (send head relay iterations)
    (.acquire finished-lock)))

(defn -main [& args]
  (let [iterations (if (first args) (Integer/parseInt (first args)) 50000)]
    (run AGENT_COUNT iterations))
    (shutdown-agents))
