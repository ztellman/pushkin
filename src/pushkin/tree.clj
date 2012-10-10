;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.tree
  (:require
    [pushkin.position :as p]
    [pushkin.simulator :as s]
    [pushkin.board :as b]))

(defprotocol INode
  (score [_])
  (visits [_])
  (add-score [_ score])
  (add-visit [_])
  (playout [_])
  (leaf? [_])
  (selected-move [_])
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


(def curr-board nil)

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
  (playout [_] (s/run-playouts 1e2 (.clone board) color (= :pass last-move)))
  (leaf? [_] (zero? @visits))
  (select-move [_ move]
    (def curr-board board)
    (when-not (contains? @@children move)
      (throw (Exception. (str "selecting a supposedly invalid move " (p/position->gtp move (:dim board))))))
    (when (> (count @@children) 1)
      (b/print-board (:board (@@children move))))
    (swap! @children #(select-keys % [move])))
  (best-child [this]
    (let [children @@children]
      (cond
        (and (= :black color) (= :pass last-move))
        (children :pass)

        (= 1 (count children))
        (->> children first val)

        :else
        (let [children (->> children
                         vals
                         (sort-by #(uct-value this %)))]
          (if (= :black color)
            (last children)
            (first children)))))))

(defn node
  [last-move board color]
  (let [moves (->> board
                b/available-moves
                (remove #(b/ko? board color (b/position board %)))
                (remove #(b/suicide? board color (b/position board %))))
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
                           (node move board (p/opponent color))
                           (node move (let [board (.clone board)]
                                        (b/add-stone board (b/position board move) color)
                                        board)
                             (p/opponent color))))
                       moves))
                   atom
                   delay)})))

(defn starting-node [dim]
  (node nil (b/empty-board dim) :black))

(defn traverse
  [node]
  (if-not node
    0
    (let [score (if (leaf? node)
                  (do
                    (playout node))
                  (let [child (best-child node)]
                    (traverse child)))]
      (add-score node score)
      (add-visit node)
      score)))

(defn move-seq [node]
  (lazy-seq
    (let [child (best-child node)
          move (:last-move child)]
      (select-move node move)
      (cons move (move-seq child)))))

(defn node-seq [node]
  (iterate best-child node))

(defn gen-move [node depth]
  (nth (move-seq node) depth))

(defn set-move [node depth move]
  (select-move
    (nth (node-seq node) depth)
    move))
