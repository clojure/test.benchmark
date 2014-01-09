;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns test.alioth.regexdna
  (:use [alioth.regexdna]
        [alioth.util.core]
        [clojure.java.io :only [input-stream]]
        [clojure.test])
  (:import [java.io InputStream
                    ByteArrayOutputStream
                    ByteArrayInputStream
                    OutputStreamWriter]))

(def ^:private this-ns *ns*) ; work around bug in com.theoryinpractice.clojure.testrunner 
(defn- resource-stream [file-name]
  (input-stream (ns-resource this-ns file-name)))

(defn- to-byte-array ^bytes [^InputStream input]
  (let [out (ByteArrayOutputStream.)
        buffer-size 16384
        buffer (byte-array buffer-size)
        read #(.read input buffer 0 buffer-size)]
    (loop [readc (read)]
      (when (not= readc -1)
        (.write out buffer 0 readc)
        (recur (read))))
    (.flush out)
    (.toByteArray out)))

#_(defn- regexdna-on-input-matches-expected [in-file-name expected-file-name]
  (with-open [out (ByteArrayOutputStream.)
              input (resource-stream in-file-name)]
    (binding [*out* (OutputStreamWriter. out)]
      (regexdna (to-byte-array input)))
    (with-open [output (ByteArrayInputStream. (.toByteArray out))
                expected (resource-stream expected-file-name)]
      (streams= expected output))))

#_(deftest test-regexdna-correctness
  (are [in            expected] (regexdna-on-input-matches-expected in expected)
       "fasta100.txt"   "expected100.txt"
       "fasta10000.txt" "expected10000.txt"
       ))
