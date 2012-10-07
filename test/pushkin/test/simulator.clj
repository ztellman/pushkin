;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.simulator
  (:use
    [pushkin.test.core]
    [clojure test])
  (:require
    [duel.core :as duel]
    [pushkin.simulator :as s]
    [pushkin.test.board :as bt]
    [pushkin.position :as p]
    [pushkin.board :as b]))

(defn valid-move? [board color pos]
  (and
    (not (bt/eye? board pos))
    (not (bt/suicide? board color pos))
    (not (b/ko? board color pos))))

(defn validate-score [board moves]
  (let [{black-score :black, white-score :white} (b/final-score board)
        [gnugo-winner gnugo-score] (duel/final-score moves {:boardsize (:dim board)})
        score-by-color {:white (- white-score black-score)
                        :black (- black-score white-score)}]
    (if-not gnugo-winner
      (is (= 0.0 (double (:black score-by-color))))
      (is (= gnugo-score (double (score-by-color gnugo-winner)))))))

(defn run-playout-validation [dim]
  (loop [player :black, pass? false, board (b/empty-board dim), moves []]
    (bt/validate-board board)
    (if-let [move (s/random-move board (:empty-positions board) player)]
      (let [coord (p/position->gtp move dim)]
        (is (valid-move? board player move))
        (recur (p/opponent player) false (b/add-stone board move player) (conj moves coord)))
      (if pass?
        (validate-score board moves)
        (recur (p/opponent player) true board (conj moves "pass"))))))

(deftest ^:stress validate-playouts
  (dotimes-p [_ 1e2]
    (time (run-playout-validation))))
