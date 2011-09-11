(ns alioth.util.core 
    (:use [clojure.java.io :only [resource]])
    (:import [java.net URL]
             [java.io InputStream]
             [clojure.lang Namespace])
    (:require [clojure.string :as str]))

(defn ^URL ns-resource
  "Returns the url for a resource relative to the given namespace."
  [^Namespace ns resource-name]
  (-> (str ns)
      (str/replace \. \/)
      (str \/ resource-name)
      (resource)))

(defn streams=
  "Predicate that is true iff the contents of the streams are identical."
  [& streams]
  (letfn [(advance [] (map (fn [^InputStream s] (.read s)) streams))]
    (loop [nth-chars (advance)]
      (if (apply not= nth-chars)
        false
        (if (= -1 (first nth-chars)) 
          true
          (recur (advance))))))) 
