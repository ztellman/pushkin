;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.tree
  (:use
    [pushkin core]
    [useful datatypes])
  (:require
    [pushkin.position :as p]
    [pushkin.simulator :as s]
    [pushkin.board :as b])
  (:import
    [pushkin.simulator
     Simulator]))

(set! *unchecked-math* true)

;;;

(def epsilon 1e-6)

(defn merge-tallies [current new]
  (merge-with + current (assoc new :visits 1)))

(defn tally->score [{:keys [black white visits]}]
  (+
    (if (zero? (+ black white))
      1e10
      (/ (float black) (float (+ black white))))
    (* epsilon (rand))))

(defn tally->uct-score [tally parent-visits]
  (+
    (tally->score tally)
    (* 0.1
      (Math/sqrt
        (/
          (float (Math/log (+ parent-visits 1)))
          (float (+ (:visits tally) epsilon)))))
    (* (rand) epsilon)))

(defprotocol ITallyScope
  (add-tally [_ move tally])
  (score [_ move]))

(defprotocol IMoveExplorer
  (best-move [_] [_ available-moves player])
  (best-exploration [_] [_ available-moves player]))

(defrecord TallyScope
  [visits
   move->tally]

  ICloneable
  (clone [_]
    (TallyScope.
      (atom @visits)
      (atom @move->tally)))

  ITallyScope
  (add-tally [_ move tally]
    (swap! visits inc)
    (swap! move->tally update-in [move] merge-tallies tally))
  (score [_ move]
    (tally->score (@move->tally move)))

  IMoveExplorer
  (best-move [_ available-moves player]
    (let [maximizer (if (= :black player)
                      last
                      first)]
      (->> (select-keys @move->tally available-moves)
        (remove #(-> % val :visits zero?))
        (sort-by #(-> % val tally->score))
        maximizer
        first)))
  (best-exploration [this available-moves player]
    (let [maximizer (if (= :black player)
                      last
                      first)]
      (->> (select-keys @move->tally available-moves)
        (sort-by #(-> % val (tally->uct-score @visits)))
        maximizer
        first))))

(defn tally-scope [dim]
  (TallyScope.
    (atom 0)
    (atom
      (zipmap
        (range (* dim dim))
        (repeat {:visits 0, :black 0, :white 0})))))

;;;

(def weight 1)

#_(defn uct-value [parent child]
  (+
    (score child)
    (* weight
     (Math/sqrt
       (/
         (Math/log (+ (visits parent) 1))
         (+ (visits child) epsilon))))
    (* (rand) epsilon)))

;;;

(defprotocol INode
  (last-move [_])
  (next-move [_])
  (traverse [_ playouts])
  (set-move [_ move]))

(declare node)

(defn node-children [^Simulator simulator parent-scope]
  (let [moves (conj (s/available-moves simulator) :pass)]
    (zipmap
      moves
      (map #(node (s/conj-move simulator %) (tally-scope (-> simulator :board b/dim))) moves))))

(defrecord Node
  [^Simulator simulator
   scope
   traversed?
   next-move
   children]
  IMoveExplorer
  (best-move [_]
    (if (and (= :pass (s/last-move simulator)) (= :black (s/player simulator)))
      (@@children :pass)
      (@@children
        (or
          @next-move
          (best-move scope (s/available-moves simulator) (s/player simulator))))))
  (best-exploration [_]
    (@@children
      (or 
        @next-move
        (best-exploration scope (s/available-moves simulator) (s/player simulator)))))

  INode
  (last-move [_] (s/last-move simulator))
  (next-move [_] @next-move)
  (traverse [this playouts]
    (if @traversed?
      (when-let [child (best-exploration this)]
        (let [tally (traverse child playouts)]
          (when-not @next-move
            (add-tally scope (last-move child) tally))
          tally))
      (do
        (reset! traversed? true)
        (s/run-playouts simulator playouts))))
  (set-move [_ move]
    ;;(rescale scope)
    (assert (not @next-move))
    (reset! next-move move)
    (swap! @children select-keys [move])))

(defn node [simulator scope]
  (make-record Node
    :simulator simulator
    :scope scope
    :traversed? (atom false)
    :next-move (atom nil)
    :children (delay (atom (node-children simulator scope)))))

(defn starting-node [dim]
  (let [n (node (s/simulator (b/empty-board dim)) (tally-scope dim))]
    (update-in n [:traversed?] #(reset! % true))
    n))

;;;

(defn prime-variation [node]
  (lazy-seq
    (if-let [child (best-move node)]
      (cons node (prime-variation child))
      [node])))

(defn current-node [node]
  (->> node prime-variation (drop-while next-move) first))

(defn force-move [node move]
  (set-move (current-node node) move))

(defn generate-move [node]
  (let [current-node (current-node node)
        move (-> (best-move current-node) :simulator s/last-move)]
    (set-move current-node move)
    move))
