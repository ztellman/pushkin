;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.test.simulator
  (:use
    [pushkin core]
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
    (if-not (#{:black :white} gnugo-winner)
      (is (= 0.0 (double (:black score-by-color))))
      (is (= gnugo-score (double (score-by-color gnugo-winner)))
        (with-out-str (b/print-board board))))))

(defn run-playout-validation [dim]
  (let [board (b/empty-board dim)]
    (loop [player :black, pass? false, moves []]
      (bt/validate-board board)
      (if-let [move (s/random-move board (b/available-moves board) player)]
        (let [coord (p/position->gtp (.value move) dim)]
          (is (valid-move? board player move))
          (b/add-stone board move player)
          (recur (p/opponent player) false (conj moves coord)))
        (if pass?
          (do
            (validate-score board moves)
            board)
          (recur (p/opponent player) true (conj moves "pass")))))))

(deftest ^:benchmark benchmark-playout
  (let [board (b/empty-board 9)]
    (bench "clone board"
      (clone board))
    (bench "random 9x9 playout"
      (s/playout-game (clone board) :black false))))

(deftest ^:stress validate-playouts
  (print "\nvalidating playouts") (flush)
  (dotimes-p [_ 1e3]
    (run-playout-validation 9)
    (print ".") (flush))
  (println))
