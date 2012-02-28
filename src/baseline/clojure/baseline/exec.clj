;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns baseline.exec
  (:require [clojure.pprint :as cpp]
            [baseline.io :as bio]
            [baseline.spec :as spec])
  (:gen-class))

(defn time-executor [executor args]
  (bio/with-null-out 
    (let [start (System/nanoTime)]
      (apply executor args)
      (/ (double (- (System/nanoTime) start)) 1000000.0))))

(defn sample-executor [sample-size executor args]
  (let [samples (repeatedly (inc sample-size) #(time-executor executor args))
        max (reduce max samples)]
    (/ (reduce + (- max) samples) sample-size)))

(defn exec-baseline-spec [sample-size {:keys [id args tollerances] :as spec}]
  (letfn [(time-fn [key] (sample-executor sample-size (spec key) args))
          (collect-execution [m key] (assoc m key (time-fn key)))]
    {:id id 
     :metrics {:runtime (reduce collect-execution {} [:target :baseline])}
     :tollerances tollerances
     :sample-size sample-size}))

(defn deviation-percentage [baseline target]
  (double (- 1 (/ baseline target))))

(defn failure-report [metrics tollerances]
  (for [[category {:keys [baseline target]}] metrics]
    (when-let [expected (get tollerances category)]
      (let [actual (deviation-percentage baseline target)]
        (when (> actual expected)
          [category {:expected expected :actual actual}])))))

(defn emit-failure-report [metrics tollerances]
  (let [report (->> (failure-report metrics tollerances)
                    (filter identity)
                    (reduce hash-map))]
    (if (seq report) report :none)))

(defn report-on [{:keys [tollerances metrics] :as execution}]
  (assoc execution :failures (emit-failure-report metrics tollerances)))

(defn run-baseline-tests [sample-size specs]
  (map (comp report-on
             (partial exec-baseline-spec sample-size)) specs))

(defn parse-sample-size [sample-size-str]
  (if sample-size-str
    (Integer/parseInt sample-size-str)
    spec/DEFAULT-SAMPLE-SIZE))

(defn parse-specs [key-list]
  (if (seq key-list)
    (let [key-set (apply hash-set key-list)]
      (filter #(key-set (:id %)) spec/baseline-specs))
    spec/baseline-specs))

(defn -main [& args]
  (let [sample-size (parse-sample-size (first args))
        specs (parse-specs (rest args))]
    (cpp/pprint (run-baseline-tests sample-size specs)))
  (shutdown-agents))
