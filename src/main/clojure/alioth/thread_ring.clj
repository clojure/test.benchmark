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
  (:import [java.util.concurrent Exchanger])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const AGENT_COUNT 503)
(def ^Exchanger output-pipe (Exchanger.))

(defn send* [^clojure.lang.Agent a f & args]
  (.dispatch a f args false))

(defn relay [state msg]
  (if (> msg 0)
    (send* (:next state) relay (dec msg))
    (.exchange output-pipe (:id state)))
  state)

(defn make-ring [n]
  (let [tail (agent {:id n}) 
        head (reduce (fn [next id] (agent {:id id :next next}))
                      tail 
                      (range (dec n) 0 -1))]
    (await (send tail assoc :next head))
    head))

(defn run [agents iterations]
  (let [head (make-ring agents)]
    (send head relay iterations)
    (.exchange output-pipe nil)))

(defn -main [& args]
  (let [iterations (if (first args) (Integer/parseInt (first args)) 50000)]
    (println (run AGENT_COUNT iterations)))
    (shutdown-agents))
