(ns blackjack.shoe
  (:require [blackjack.card :as card]))

(def cards-per-deck 52)

(def shuffle-specs
  [80 81 82 84 86 89 92 95])

(defn total-cards [num-decks]
  (* cards-per-deck num-decks))

(def ^:private card-values-by-deck
  {:regular    (vec (range 0 13))
   :aces       [0]
   :jacks      [10]
   :aces-jacks [0 10]
   :sevens     [6]
   :eights     [7]})

(defn card-values
  [deck-type]
  (get card-values-by-deck deck-type))

(defn need-to-shuffle?
  [{:keys [shoe num-decks]}]
  (let [cards-count (count shoe)]
    (if (zero? cards-count)
      true
      (let [used (- (total-cards num-decks) cards-count)
            spec (nth shuffle-specs (dec num-decks))]
        (> (* 100 (/ (double used) cards-count)) spec)))))

(defn- shuffle-with
  [shoe ^java.util.Random rng]
  (let [arr (object-array shoe)
        n (alength arr)
        passes (* 7 n)]
    (dotimes [_ passes]
      (let [i (.nextInt rng n)
            card (aget arr i)]
        (System/arraycopy arr 0 arr 1 i)
        (aset arr 0 card)))
    (vec arr)))

(defn- one-deck-chunk
  [values next-id!]
  (let [reversed (vec (reverse values))]
    (vec (mapcat (fn [suit]
                   (mapv (fn [v] (card/make-card (next-id!) v suit))
                         reversed))
                 (range 4)))))

(defn- fill-pile [target values starting-id]
  (let [id-counter (atom starting-id)
        next-id! (fn [] (swap! id-counter inc))
        pile (loop [acc []]
               (if (>= (count acc) target)
                 acc
                 (recur (into acc (one-deck-chunk values next-id!)))))]
    [(vec (take target pile)) @id-counter]))

(defn build-shoe
  ([num-decks deck-type starting-id]
   (build-shoe num-decks deck-type starting-id (java.util.Random.)))
  ([num-decks deck-type starting-id ^java.util.Random rng]
   (let [[pile next-id] (fill-pile (total-cards num-decks)
                                   (card-values deck-type)
                                   starting-id)]
     [(shuffle-with pile rng) next-id])))

(defn build-shoe-no-shuffle
  [num-decks deck-type starting-id]
  (fill-pile (total-cards num-decks)
             (card-values deck-type)
             starting-id))
