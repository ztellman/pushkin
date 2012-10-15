;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.collections
  (:use
    [pushkin.core])
  (:refer-clojure
    :exclude [pop! contains?])
  (:import
    [java.lang.reflect
     Array]
    [java.util
     BitSet]))

(set! *unchecked-math* true)

(defprotocol IPositionColl
  (add! [_ p] "mutable add to set or stack")
  (remove! [_ p] "mutably remove an element from the set or stack"))

(defprotocol IPositionStack
  (remove-at! [_ idx] "mutably removes and returns the element at the index")
  (pop! [_] "mutably removes the element at the top of the stack"))

(defprotocol IPositionSet
  (contains? [_ p])
  (set->stack [_]))

;;;

(deftype PositionStack
  [^longs positions
   ^:unsynchronized-mutable ^long -count]

  clojure.lang.Counted
  (count [_] -count)

  clojure.lang.Seqable
  (seq [_] (take -count (seq positions)))

  ICloneable
  (clone [_]
    (PositionStack.
      (let [ary (make-array Long/TYPE (Array/getLength positions))]
        (System/arraycopy positions 0 ary 0 -count)
        ary)
      -count))

  IPositionColl
  IPositionStack
  (add! [_ p]
    (aset positions -count (long p))
    (set! -count (inc -count))
    nil)
  (pop! [_ ]
    (if (== 0 -count)
      nil
      (do
        (set! -count (dec -count))
        (aget positions -count))))
  (remove! [this p]
    (loop [idx 0]
      (if (<= -count idx)
        nil
        (if (== p (aget positions idx))
          (do
            (remove-at! this idx)
            p)
          (recur (inc idx))))))
  (remove-at! [_ idx]
    (let [idx (long idx)
          p (aget positions idx)]
      (System/arraycopy
        positions (inc idx)
        positions idx
        (dec (- -count idx)))
      (set! -count (dec -count))
      p)))

(defn empty-stack
  ([]
     (empty-stack 361))
  ([^long dim]
     (PositionStack.
       (make-array Long/TYPE dim)
       0)))

(defn populated-stack [^long dim]
  (let [s (empty-stack dim)]
    (dotimes [i dim]
      (add! s i))
    s))

;;;

(deftype PositionSet
  [^BitSet bit-set]

  clojure.lang.Counted
  (count [_] (.cardinality bit-set))

  ICloneable
  (clone [_] (PositionSet. (.clone bit-set)))
  
  IPositionColl
  IPositionSet
  (add! [_ p] (.set bit-set (long p) true))
  (remove! [_ p] (.set bit-set (long p) false))
  (contains? [_ p] (.get bit-set (long p)))
  (set->stack [_]
    (let [s (empty-stack (.cardinality bit-set))]
      (loop [p 0]
        (let [p (.nextSetBit bit-set p)]
          (if (== -1 p)
            s
            (do
              (add! s p)
              (recur (inc p)))))))))

(defn position-set
  ([]
     (position-set 361))
  ([^long dim]
     (PositionSet. (BitSet. dim))))
