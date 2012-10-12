;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.tree
  (:use
    [pushkin core])
  (:require
    [pushkin.position :as p]
    [pushkin.simulator :as s]
    [pushkin.board :as b])
  (:import
    [pushkin.board
     Board]))

(defprotocol INode
  (score [_])
  (visits [_])
  (add-score [_ score])
  (add-visit [_])
  (playout [_ n])
  (leaf? [_])
  (selected-move [_])
  (select-move [_ move])
  (best-move [_])
  (best-uct [_]))

(def epsilon 1e-6)

(def weight 0.1)

(defn uct-value [parent child]
  (+
    (score child)
    (* weight
     (Math/sqrt
       (/
         (Math/log (+ (visits parent) 1))
         (+ (visits child) epsilon))))
    (* (rand) epsilon)))


(def curr-board nil)

(defrecord Node
  [last-move
   color
   ^Board board
   children
   win-tallies
   visit-tally]

  INode

  (score [_]
    (let [{:keys [white black]} @win-tallies]
      (case color
        :white (/ black (+ white black epsilon))
        :black (/ white (+ white black epsilon)))))
  (visits [_] @visit-tally)
  (add-score [_ val] (swap! win-tallies #(merge-with + % val)))
  (add-visit [_] (swap! visit-tally inc))
  (playout [_ n] (s/run-playouts n (clone board) color (= :pass last-move)))
  (leaf? [_] (zero? @visit-tally))
  (select-move [_ move]
    (def curr-board board)
    (when-not (contains? @@children move)
      (throw (Exception. (str "selecting a supposedly invalid move " (p/position->gtp move (:dim board))))))
    (when (> (count @@children) 1)
      (b/print-board (:board (@@children move))))
    (swap! @children #(select-keys % [move])))
  (best-uct [this]
    (let [children @@children]
      (cond
        (and (= :black color) (= :pass last-move))
        (children :pass)

        (= 1 (count children))
        (->> children first val)

        :else
        (->> children
          vals
          (sort-by #(uct-value this %))
          last))))
  (best-move [_]
    (let [children @@children]
      (cond
        (and (= :black color) (= :pass last-move))
        (children :pass)

        (= 1 (count children))
        (->> children first val)

        :else
        (->> children
          vals
          (sort-by score)
          last)))))

(defn node
  [last-move ^Board board color]
  (let [moves (->> board
                b/available-moves
                (remove #(b/ko? board color (b/position board %)))
                (remove #(b/suicide? board color (b/position board %))))
        moves (conj moves :pass)]
    (map->Node
      {:last-move last-move
       :color color
       :board board
       :win-tallies (atom {:white 0
                     :black 0})
       :visit-tally (atom 0)
       :children (-> moves
                   (zipmap
                     (map
                       (fn [move]
                         (if (= :pass move)
                           (node move (clone board) (p/opponent color))
                           (node move (let [board (clone board)]
                                        (b/add-stone board (b/position board move) color)
                                        board)
                             (p/opponent color))))
                       moves))
                   atom
                   delay)})))

(defn played-out-node [last-move ^Board board color playouts]
  (let [moves (->> board
                b/available-moves
                (remove #(b/ko? board color (b/position board %)))
                (remove #(b/suicide? board color (b/position board %))))
        moves (conj moves :pass)]
    (map->Node
      {:last-move last-move
       :color color
       :board board
       :win-tallies (atom {:white 0
                     :black 0})
       :visit-tally (atom 0)
       :children (-> moves
                   (zipmap
                     (pmap
                       (fn [move]
                         (let [node (if (= :pass move)
                                      (node move (clone board) (p/opponent color))
                                      (node move (let [board (clone board)]
                                                   (b/add-stone board (b/position board move) color)
                                                   board)
                                        (p/opponent color)))]
                           (add-score node (playout node playouts))
                           node))
                       moves))
                   doall
                   atom
                   )})))

(defn starting-node [dim]
  (node nil (b/empty-board dim) :black))

(defn traverse
  [node]
  (if-not node
    0
    (let [score (if (leaf? node)
                  (do
                    (playout node 1e3))
                  (let [child (best-uct node)]
                    (traverse child)))]
      (add-score node score)
      (add-visit node)
      score)))

(defn move-seq [node]
  (lazy-seq
    (let [child (best-move node)
          move (:last-move child)]
      (select-move node move)
      (cons move (move-seq child)))))

(defn node-seq [node]
  (iterate best-move node))

(defn gen-move [node depth]
  (nth (move-seq node) depth))

(defn set-move [node depth move]
  (select-move
    (nth (node-seq node) depth)
    move))
