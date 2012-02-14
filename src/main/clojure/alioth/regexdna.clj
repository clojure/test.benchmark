;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   Alioth benchmarks: http://shootout.alioth.debian.org/u64q/benchmark.php?test=regexdna
(ns alioth.regexdna
  (:import [java.io FileDescriptor FileInputStream])
  (:import [java.nio ByteBuffer])
  (:import [java.nio.channels FileChannel])
  (:import [java.util.regex Matcher Pattern])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const object-array-t "[Ljava.lang.Object;")

(defmacro for-loop [[binding pred adv] & body]
  `(loop [~@binding]
     (when ~pred
       ~@body
       (recur ~adv))))

(definterface LengthSetable
  (^void setLength [^long l]))

(deftype ByteWrapper [^bytes backing ^long ^:unsynchronized-mutable length]
  CharSequence
    (charAt [_ index] (char (aget backing index)))
    (length [_] length)
  LengthSetable
    (^void setLength [_ ^long l] (set! length l)))

(def ^:const comments (Pattern/compile ">.*\n|\n"))
(def ^:const codes-pat (Pattern/compile "[BDHKMNRSVWY]"))

(def codes [["B" "(c|g|t)"]
            ["D" "(a|g|t)"]
            ["H" "(a|c|t)"]
            ["K" "(g|t)"]
            ["M" "(a|c)"]
            ["N" "(a|c|g|t)"]
            ["R" "(a|g)"]
            ["S", "(c|g)"]
            ["V", "(a|c|g)"]
            ["W", "(a|t)"]
            ["Y", "(c|t)"]])
(def acodes (into-array (map into-array codes)))
(def longest (long (reduce #(max %1 (count (second %2))) 0 codes)))

(def _strs ["agggtaaa|tttaccct"
            "[cgt]gggtaaa|tttaccc[acg]"
            "a[act]ggtaaa|tttacc[agt]t"
            "ag[act]gtaaa|tttac[agt]ct"
            "agg[act]taaa|ttta[agt]cct"
            "aggg[acg]aaa|ttt[cgt]ccct"
            "agggt[cgt]aa|tt[acg]accct"
            "agggta[cgt]a|t[acg]taccct"
            "agggtaa[cgt]|[acg]ttaccct"
            ])
(def ^object-array-t strs (into-array _strs))
(def ^object-array-t pats (into-array (map #(Pattern/compile %) _strs)))

(def repl
  (let [buf (byte-array (* 26 (inc longest)))]
    (doseq [[offset ^String code] (map (fn [[^String l code]]
                                         [(* longest (- (int  (.charAt l 0)) (int \A))) code])
                                       codes)]
      (doseq [j (range (count code))]
        (aset buf (+ offset j) (byte (int (.charAt code j))))))
    buf))

(defn rm-comments [^ByteWrapper t]
  (let [^bytes backing (.backing t)
        backing-length (count backing) ; todo check if needed
        m (.matcher comments t)]
    (if (.find m)
      (.setLength t (loop [tail (.start m), restart (.end m)]
                      (if (.find m)
                        (recur (long (loop [tail tail, restart restart]
                                       (if (not= restart (.start m))
                                         (do (aset backing tail (aget backing restart))
                                             (recur (inc tail) (inc restart)))
                                         tail)))
                               (.end m))
                        (loop [tail tail, restart restart]
                          (if (< restart backing-length)
                            (do (aset backing tail (aget backing restart))
                                (recur (inc tail) (inc restart)))
                            tail))))))))

(defn count-patterns [^ByteWrapper t]
  (for-loop [(i 0) (< i (count pats)) (inc i)]
            (let [m (.matcher ^Pattern (aget pats i) t)
                  c (loop [c 0] (if (.find m) (recur (inc c)) c))]
              (println (aget strs i) c))))

(defn replace-in ^ByteWrapper [^ByteWrapper t]
  (let [^bytes backing (.backing t)
        ^bytes repl repl
        buf (byte-array (* (.length t) longest))
        m (.matcher codes-pat t)
        pos (loop [pos 0, last 0]
              (if (.find m)
                (let [[p l] (loop [pos pos, last last]
                              (if (< last (.start m))
                                (do (aset buf pos (aget backing last))
                                    (recur (inc pos) (inc last)))
                                (loop [pos pos, i (* longest (- (int (aget backing last)) (int \A)))]
                                  (if (not= 0 (int (aget repl i)))
                                    (do (aset buf pos (aget repl i))
                                        (recur (inc pos) (inc i)))
                                    [pos (inc last)]))))]
                  (recur (long p) (long l)))
                (loop [pos pos, last last]
                  (if (< last (.length t))
                    (do (aset buf pos (aget backing last))
                        (recur (inc pos) (inc last)))
                    pos))))]
    (ByteWrapper. buf pos)))

(defn regexdna [^bytes ba]
  (let [t (ByteWrapper. ba (count ba))]
    (rm-comments t)
    (let [w (future (replace-in t))] 
      (count-patterns t)
      (println)
      (println (count (.backing t)))
      (println (.length t))
      (println (.length ^ByteWrapper @w)))))

(defn -main [& args]
  (let [cin (-> FileDescriptor/in FileInputStream. .getChannel)
        bb (-> cin .size int ByteBuffer/allocate)]
    (.read cin bb)
    (regexdna (.array bb)))
  (shutdown-agents))