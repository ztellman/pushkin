;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.board
  (:use
    [pushkin core]
    [useful.datatypes])
  (:require
    [useful.state :as state]
    [pushkin.hash :as h]
    [pushkin.position :as p])
  (:import
    [java.util
     BitSet
     LinkedList]
    [java.util.concurrent.atomic
     AtomicReference
     AtomicInteger]
    [pushkin.position
     Position]))

(set! *unchecked-math* true)

;;;

(defprotocol IPositionTracker
  (eyes [_ board])
  (stone-added [_ board p])
  (stone-removed [_ board p]))

(defprotocol IBoard
  (add-white-score [_ n])
  (white-score [_])
  (add-black-score [_ n])
  (black-score [_])
  (move-number [_]))

(defrecord Board
  [^long dim
   ^objects positions
   ^AtomicInteger move-counter
   ^AtomicInteger white-score
   ^AtomicInteger black-score
   hash
   tracker]

  ICloneable
  
  (clone [_]
    (Board.
      dim
      (p/clone-positions positions)
      (AtomicInteger. (.get move-counter))
      (AtomicInteger. (.get white-score))
      (AtomicInteger. (.get black-score))
      (clone hash)
      (clone tracker)))
  
  IBoard

  (move-number [_] (.get move-counter))
  (white-score [_] (.get white-score))
  (add-white-score [_ n] (.addAndGet white-score n))
  (black-score [_] (.get black-score))
  (add-black-score [_ n] (.addAndGet black-score n)))

(defn dim [^Board board]
  (.dim board))

(defmacro position [board n]
  `(let [^Board board# ~board]
     (p/position (.positions board#) (long ~n))))

;;;

(defn ^Board empty-board
  [dim]
  (make-record Board
    :dim dim
    :positions (p/initial-positions dim)
    :move-counter (AtomicInteger. 0)
    :white-score (AtomicInteger. 0)
    :black-score (AtomicInteger. 0)
    :hash (h/zobrist-hash)
    :tracker (reify
               IPositionTracker
               ICloneable
               (stone-added [_ _ _])
               (stone-removed [_ _ _])
               (clone [this] this))))

(defn print-board [^Board board]
  (let [dim (dim board)
        cols (->> (range (inc dim))
               (map #(char (+ (int \A) %)))
               (remove #{\I})
               (interpose " ")
               (apply str))]
    (println "Black:" (black-score board))
    (println "White:" (white-score board))
    (println (str "   " cols "\n"))
    (doseq [y (reverse (range dim))]
      (print (str (inc y) "  "))
      (doseq [x (range dim)]
        (let [c (p/color (position board (p/coord->position dim x y)))]
          (print
            (case c
              :white "O "
              :black "# "
              :empty ". "))))
      (println (str " " (inc y))))
    (println (str "\n   " cols "\n"))))

(defmethod print-method Board [o w]
  (.write ^java.io.Writer w (str (with-out-str (print-board o)))))

;;;

(defmacro foreach-neighbor [board [p n] params & body]
  `(let [^Board board# ~board
         ^Position pos# ~p
         pos# (.value pos#)]
     (p/foreach-neighbor (.dim board#) (.positions board#)
       [pos# ~(with-meta n {:tag "pushkin.position.Position"})] ~params
       ~@body)))

;;;

(defn parent [^Board board ^Position pos]
  (let [parent (.parent pos)]
    (if (== (long (.value pos)) (long parent))
      pos
      (let [origin pos]
        (loop [pos (long parent)]
          (let [^Position pos (position board pos)
                parent (long (p/parent pos))]
            (if (= parent (.value pos))
              (do (p/set-parent origin pos) pos)
              (recur parent))))))))

;;;

(defn remove-stone [^Board board ^Position pos]
  (let [stone-color (p/color pos)
        opponent-color (p/opponent stone-color)]

    (if (identical? :white stone-color)
      (add-black-score board 1)
      (add-white-score board 1))

    (h/toggle (.hash board) pos stone-color)

    (foreach-neighbor board [pos n] []
      (when-not (identical? stone-color (p/color n))
        (let [pn (parent board n)]
          (p/remove-neighbor pn pos))))

    (p/reset pos)

    (foreach-neighbor board [pos n] []
      (when-not (identical? :empty (p/color n))
        (p/add-neighbor pos n)))

    (stone-removed (.tracker board) board pos)))

(defn clear-group [^Board board ^Position pos]
  (let [stone-color (p/color pos)]
    (remove-stone board pos)
    (foreach-neighbor board [pos n] []
      (when (identical? stone-color (p/color n))
        (clear-group board n)))))

(defn add-stone [^Board board ^Position pos stone-color]

  (h/rotate (.hash board))
  (h/toggle (.hash board) pos stone-color)
  (p/set-color pos stone-color)
  (.incrementAndGet ^AtomicInteger (.move-counter board))

  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] []
      (let [pn (parent board n)
            pn-color (p/color pn)]

        (p/add-neighbor pn pos)

        (condp identical? pn-color

          stone-color
          (p/unify pos pn)

          opponent-color
          (when (== 0 (long (p/liberties pn)))
            (clear-group board pn))

          nil))))

  (stone-added (.tracker board) board pos))

;;;

(defn capture [board stone-color pos]
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] [captured nil]
      (if captured
        captured
        (when (and 
                (identical? opponent-color (p/color n))
                (p/atari (parent board n)))
          n)))))

;; todo: this is a hack, do something more elegant
(defn capture-count [board stone-color pos]
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] [captured 0]
      (if (and 
              (identical? opponent-color (p/color n))
              (p/atari (parent board n)))
        (inc captured)
        captured))))

(defn eye? [board pos]
  (condp identical? (p/surrounded? pos)
    :white (when-not (capture board :black pos)
             :white)
    :black (when-not (capture board :white pos)
             :black)
    nil))

(defn ko? [^Board board stone-color pos]
  (when-let [n (capture board stone-color pos)]
    (and
      (identical? n (parent board n))
      (= 1 (capture-count board stone-color pos))
      (condp identical? stone-color
        :white (h/ko? (.hash board) pos n)
        :black (h/ko? (.hash board) n pos)))))

(defn capture-all? [board stone-color pos]
  (let [opponent-color (p/opponent stone-color)]
    (foreach-neighbor board [pos n] [capture? true]
      (and capture?
        (if (identical? opponent-color (p/color n))
          (boolean (p/atari (parent board n)))
          true)))))

(defn suicide? [board stone-color p]
  (let [opponent-color (p/opponent stone-color)]
    (boolean
      (condp identical? stone-color
        :black (case (p/surrounded? p)
                 :black (capture-all? board :white p)
                 :white (not (capture board :black p))
                 :mixed (and
                          (not (capture board :black p))
                          (capture-all? board :white p))
                 false)
        :white (case (p/surrounded? p)
                 :white (capture-all? board :black p)
                 :black (not (capture board :white p))
                 :mixed (and
                          (not (capture board :white p))
                          (capture-all? board :black p))
                 false)))))

(defn final-score [^Board board]
  (merge-with +
    {:white (white-score board)
     :black (black-score board)}
    (->> (eyes (.tracker board) board)
      (map (partial eye? board))
      frequencies)))
