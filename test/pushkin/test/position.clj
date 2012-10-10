;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.position
  (:use
    [pushkin.test.core]
    [clojure.test])
  (:require
    [pushkin.position :as p])
  (:import
    [pushkin.position Position]))

(deftest ^:benchmark benchmark-copy-positions
  (let [ps (p/initial-positions 9)]
    (long-bench "copy positions"
      (p/clone-positions ps))))

(deftest ^:benchmark benchmark-position-lookup
  (let [ps (p/initial-positions 9)]
    (long-bench "lookup position"
      (p/position ps 42))))

(deftest ^:benchmark benchmark-field-lookup
  (let [ps (p/initial-positions 9)
        p (p/position ps 42)]
    (long-bench "lookup field"
      (.liberties p))))

(deftest ^:benchmark benchmark-field-increment
  (let [ps (p/initial-positions 9)
        p (p/position ps 42)]
    (long-bench "increment field"
      (.add_black_neighbors p 1))))

(deftest ^:benchmark benchmark-neighbor-lookup
  (bench "lookup neighbors"
    (p/neighbors 42 9)))

(deftest ^:benchmark benchmark-increment-neighbors
  (let [ps (p/clone-positions (p/initial-positions 9))]
    (long-bench "increment neighbors w/ 2 neighbors"
      (p/foreach-neighbor 9 ps [0 n]
        (.add_black_neighbors n 1)))
    (long-bench "increment neighbors w/ 3 neighbors"
      (p/foreach-neighbor 9 ps [1 n]
        (.add_black_neighbors n 1)))
    (long-bench "increment neighbors w/ 4 neighbors"
      (p/foreach-neighbor 9 ps [42 n]
        (.add_black_neighbors n 1)))))
