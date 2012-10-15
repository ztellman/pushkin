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
      (p/liberties p))))

(deftest ^:benchmark benchmark-add-remove-neighbors
  (let [ps (p/initial-positions 9)
        a (p/position ps 42)
        b (p/position ps 43)]
    (p/set-color a :black)
    (p/set-color b :black)
    (long-bench "add neighbor, then remove"
      (p/add-neighbor a b)
      (p/remove-neighbor a b))))

(deftest ^:benchmark benchmark-neighbor-lookup
  (long-bench "lookup neighbors"
    (p/neighbors 42 9)))
