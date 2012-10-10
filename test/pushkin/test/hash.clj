;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.hash
  (:use
    [pushkin.test.core]
    [clojure.test])
  (:require
    [pushkin.hash :as h]))

(deftest ^:benchmark benchmark-hash
  (let [hash (h/zobrist-hash)]
    (bench "hash toggle"
      (h/toggle hash :white 42))
    (bench "hash rotate"
      (h/rotate hash))
    (bench "hash clone"
      (h/clone hash))
    (bench "hash ko check"
      (h/ko? hash 42 43))))
