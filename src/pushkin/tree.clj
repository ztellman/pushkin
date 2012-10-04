;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.tree
  (:require
    [pushkin.simulator :as s]
    [pushkin.board :as b]))

(defprotocol INode
  (move [_])
  (score [_])
  (add-score [_ score])
  (add-visit [_])
  (visits [_])
  (playout [_])
  (leaf? [_])
  (select-move [_ move])
  (best-child [_]))

(def epsilon 1e-6)

(defn uct-value [parent child]
  (+
    (/
      (score child)
      (+ (visits child) epsilon))
    (Math/sqrt
      (/
        (Math/log (+ (visits parent) 1))
        (+ (visits child) epsilon)))
    (* (rand) epsilon)))


(defrecord Node
  [last-move
   color
   board
   children
   score
   visits]

  INode

  (score [_] @score)
  (visits [_] @visits)
  (add-score [_ val] (swap! score + val))
  (add-visit [_] (swap! visits inc))
  (playout [_] (s/run-playouts 100 board color (= :pass last-move)))
  (leaf? [_] (zero? @visits))
  (select-move [_ move]
    (swap! @children #(select-keys % [move])))
  (best-child [this]
    (let [children @@children]
      (if (= 1 (count children))
        (->> children first val)
        (->> children
          vals
          (sort-by #(uct-value this %))
          last)))))

(defn node
  [last-move board color]
  (let [moves (->> board
                :empty-positions
                (remove #(b/eye-type board %))
                (remove #(= :ko (b/capture-type board color %))))
        moves (conj moves :pass)]
    (map->Node
      {:last-move last-move
       :color color
       :board board
       :score (atom 0)
       :visits (atom 0)
       :children (-> moves
                   (zipmap
                     (map
                       (fn [move]
                         (if (= :pass move)
                           (node move board (b/opponent color))
                           (node move (s/add-stone board move color) (b/opponent color))))
                       moves))
                   atom
                   delay)})))

(defn starting-node [dim]
  (node nil (b/empty-board dim) :black))

(defn traverse [node]
  (let [score (if (leaf? node)
                (playout node)
                (->> node best-child traverse))]
    (add-score node score)
    (add-visit node)
    score))
