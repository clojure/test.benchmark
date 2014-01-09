;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   Alioth benchmarks:
;   http://shootout.alioth.debian.org/u64q/benchmark.php?test=regexdna

(ns alioth.regexdna
  (:import [java.io FileInputStream FileDescriptor]
           [java.nio ByteBuffer]
           [java.util.regex Pattern])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn -main [& args]
  (let [cin (-> FileDescriptor/in FileInputStream. .getChannel)
        bb (-> cin .size int ByteBuffer/allocate)]
    (.read cin bb)
    (let [input (String. (.array bb) "US-ASCII")
          sequence (.replaceAll input ">.*\n|\n" "")
          replacements (array-map "B" "(c|g|t)"
                                  "D" "(a|g|t)"
                                  "H" "(a|c|t)"
                                  "K" "(g|t)"
                                  "M" "(a|c)"
                                  "N" "(a|c|g|t)"
                                  "R" "(a|g)"
                                  "S", "(c|g)"
                                  "V", "(a|c|g)"
                                  "W", "(a|t)"
                                  "Y", "(c|t)")
          buflen (future-call #(let [buf (StringBuffer.)
                                     m (.matcher (Pattern/compile "[WYKMSRBDVHN]") sequence)]
                                 (loop []
                                   (when (.find m)
                                     (.appendReplacement m buf "")
                                     (.append buf (get replacements (.group m)))
                                     (recur)))
                                 (.appendTail m buf)
                                 (.length buf)))
          variants ["agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
                    "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
                    "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
                    "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
                    "agggtaa[cgt]|[acg]ttaccct"]
          match-fn (fn [^String v]
                     (let [m (.matcher (Pattern/compile v) sequence)]
                       (loop [c 0]
                         (if (.find m)
                           (recur (inc c))
                           [v c]))))
          results (pmap match-fn variants)]
      (doall (for [[variant c] results] (println variant c)))
      (println)
      (println (.length input))
      (println (.length sequence))
      (println @buflen)
      (shutdown-agents))))
