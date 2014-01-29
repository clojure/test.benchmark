;   Copyright (c) Rich Hickey and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns test.alioth.thread-ring
  (:use [alioth.thread-ring]
        [clojure.test]))

#_(deftest test-run-correctness
  (are [agents iterations expected] (= expected (run agents iterations))
        5      100        1
        5      101        2  
        503    5000       474 
       ))
