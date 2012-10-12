;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns pushkin.hash
  (:require
    [pushkin.position])
  (:import
    [java.math
     BigInteger]
    [java.util
     BitSet
     Random]
    [pushkin.position
     Position]))

(def hash-bits 128)

(defn random-bitset [^Random rng ^long size]
  (let [bigint (BigInteger. size rng)
        bitset (BitSet. size)]
    (dotimes [i size]
      (.set bitset i (.testBit bigint i)))
    bitset))

(defn empty-bitset [size]
  (BitSet. size))

(let [rng (Random.)]
  (def black-hashes (into-array BitSet (repeatedly 361 #(random-bitset rng hash-bits))))
  (def white-hashes (into-array BitSet (repeatedly 361 #(random-bitset rng hash-bits)))))

(defn ^BitSet move-hash [color ^long pos]
  (case color
    :black (aget ^objects black-hashes pos)
    :white (aget ^objects white-hashes pos)))

(defprotocol ZobristHash
  (clone [_])
  (rotate [_])
  (toggle [_ color pos])
  (ko? [_ white-stone black-stone]))

(defn zobrist-hash
  ([]
     (zobrist-hash
       (into-array BitSet
         [(empty-bitset hash-bits) (empty-bitset hash-bits)])))
  ([^objects hashes]
     (reify ZobristHash
       (clone [_]
         (zobrist-hash
           (amap hashes idx ret (.clone ^BitSet (aget hashes idx)))))
       (rotate [_]
         (aset hashes 1 (.clone ^BitSet (aget hashes 0)))
         nil)
       (toggle [_ pos color]
         (let [pos (.value ^Position pos)]
           (.xor ^BitSet (aget hashes 0) (move-hash color pos)))
         nil)
       (ko? [this white-stone black-stone]
         (try
           (toggle this white-stone :white)
           (toggle this black-stone :black)
           (= (aget hashes 0) (aget hashes 1))
           (finally
             (toggle this white-stone :white)
             (toggle this black-stone :black)))))))

