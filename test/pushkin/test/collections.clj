;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.collections
  (:use
    [clojure.test]
    [pushkin.test.core])
  (:require
    [pushkin.collections :as c]))

(deftest ^:benchmark benchmark-position-stack
  (let [s (c/empty-stack 10)]
    (long-bench "add! and pop!"
      (c/add! s 1)
      (c/pop! s)))
  (let [s (c/populated-stack 81)]
    (long-bench "remove-at! and add!"
      (c/add! s (c/remove-at! s 0))))
  (let [s (c/populated-stack 81)]
    (long-bench "remove! and add!"
      (c/add! s (c/remove! s 80)))))

(deftest ^:benchmark benchmark-position-set
  (let [s (c/position-set 10)]
    (long-bench "add! and remove!"
      (c/add! s 0)
      (c/remove! s 0))))
