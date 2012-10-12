;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.simulator
  (:use
    [pushkin core])
  (:require
    [pushkin.hash :as h]
    [pushkin.board :as b]
    [pushkin.position :as p])
  (:import
    [pushkin.board
     Board]
    [org.uncommons.maths.random
     XORShiftRNG]))

;;;

(def rng (XORShiftRNG.))

(defn rand-int* [max]
  (.nextInt ^XORShiftRNG rng max))

(defn random-move [board positions color]
  (when-not (empty? positions)
    (let [p (b/position board (nth (seq positions) (rand-int* (count positions))))]
      (if (and
            (not (b/eye? board p))
            (not (b/suicide? board color p))
            (not (b/ko? board color p)))
        p
        (random-move board (disj positions (.value p)) color)))))

(defn playout-game [^Board board color pass?]
  (let [board (clone board)]
    (loop [player color, pass? pass?]
      (if-let [move (random-move board (b/available-moves board) player)]
        (do
          (b/add-stone board move player)
          (recur (p/opponent player) false))
        (if pass?
          (b/final-score board)
          (recur (p/opponent player) true))))))

(defn run-playouts [n board color pass?]
  (->> (range n)
    (map (fn [_] (playout-game board color pass?)))
    (map (fn [{:keys [white black]}]
           (cond
             (< white black) :black
             (> white black) :white
             :else nil)))
    (remove nil?)
    frequencies))


