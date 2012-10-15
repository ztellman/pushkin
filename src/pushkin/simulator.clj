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
    [pushkin.position :as p]
    [pushkin.collections :as c])
  (:import
    [java.util.concurrent.atomic
     AtomicInteger]
    [pushkin.board
     Board]
    [pushkin.position
     Position]
    [org.uncommons.maths.random
     XORShiftRNG]
    [pushkin.collections
     PositionSet
     PositionStack]))

(set! *unchecked-math* true)

;;;

(defprotocol IMoveSelector
  (select-move [_ board player])
  (remove-move [_ board pos]))

(defrecord MoveSelector
  [^PositionStack moves
   ^PositionSet eyes
   ^XORShiftRNG rng
   ^AtomicInteger last-capture]

  IMoveSelector
  (remove-move [_ board pos]
    (c/remove! moves pos)
    (c/remove! eyes pos))

  (select-move [_ board player]
    (loop [cnt (count moves)]
      (when-not (== 0 cnt)
        (let [move (->> cnt (.nextInt rng) (c/remove-at! moves))]
          (let [pos (b/position board move)]

            (cond
              (b/eye? board pos)
              (do
                (c/add! eyes move)
                (recur (dec cnt)))

              (or
                (b/suicide? board player pos)
                (and
                  (== (long (b/move-number board)) (long (.get last-capture)))
                  (b/ko? board player pos)))
              (do
                (c/add! moves move)
                (recur (dec cnt)))

              :else
              pos))))))

  ICloneable
  (clone [_]
    (MoveSelector.
      (clone moves)
      (clone eyes)
      rng
      (AtomicInteger. (.get last-capture))))
  
  pushkin.board.IPositionTracker
  (stone-added [this board pos]
    (b/foreach-neighbor board [pos n] []
      (let [n-val (.value n)]
        (if (identical? :empty (p/color n))
          
          (when (and
                  (c/contains? eyes n-val)
                  (not (b/eye? board n)))
            (c/remove! eyes n-val)
            (c/add! moves n-val))
          
          (when-let [capture-point (p/atari (b/parent board n))]
            (when (c/contains? eyes capture-point)
              (c/remove! eyes capture-point)
              (c/add! moves capture-point)))))))
  
  (stone-removed [_ board pos]
    (.set last-capture (b/move-number board))
    (c/add! moves (.value ^Position pos)))

  (eyes [_ board]
    (->> eyes
      c/set->stack
      seq
      (map #(b/position board %)))))

(def rng (XORShiftRNG.))

(defn move-selector [^long dim]
  (MoveSelector.
    (c/populated-stack (* dim dim))
    (c/position-set (* dim dim))
    rng
    (AtomicInteger. -1)))

(defn legal-moves [board player]
  (->> (concat
         (->> board :tracker :moves seq)
         (->> board :tracker :eyes c/set->stack seq))
    (map #(b/position board %))
    (remove #(b/suicide? board player %))
    (remove #(b/ko? board player %))
    (map #(.value ^Position %))))

;;;

(defprotocol ISimulator
  (available-moves [_])
  (last-move [_])
  (player [_])
  (conj-move [_ move])
  (playout [_]))

(defrecord Simulator
  [^Board board
   player
   pass?
   available-moves
   moves]

  ICloneable
  (clone [_]
    (Simulator.
      (clone board)
      player
      pass?
      available-moves
      moves))

  ISimulator

  (available-moves [_] available-moves)
  (last-move [_] (last moves))
  (player [_] player)
  
  (conj-move [this move]
    (let [pass? (= :pass move)
          ^Board board (clone board)]

      (when-not pass?
        (remove-move (.tracker board) board move)
        (b/add-stone board (b/position board move) player))

      (Simulator.
        board
        (p/opponent player)
        pass?
        (legal-moves board (p/opponent player))
        (conj moves move))))
  
  (playout [_]
    (let [selector (.tracker board)
          max-moves (* (:dim board) (:dim board) 4)]
      (loop [player player, pass? pass?, moves 0]
        (if (> moves max-moves)
          (b/final-score board)
          (if-let [move (select-move selector board player)]
            (do
              (b/add-stone board move player)
              (recur (p/opponent player) false (inc moves)))
            (if pass?
              (do
                ;;(b/print-board board)
                (b/final-score board))
              (recur (p/opponent player) true (inc moves)))))))))

(defn simulator
  [^Board board]
  (let [board (assoc board :tracker (move-selector (b/dim board)))]
    (Simulator. board :black false (legal-moves board :black) [])))

(defn run-playouts [simulator n]
  (->> (range n)
    (map (fn [_] (playout (clone simulator))))
    (map (fn [{:keys [white black]}]
           (cond
             (< white black) :black
             (> white black) :white
             :else nil)))
    (remove nil?)
    frequencies))


