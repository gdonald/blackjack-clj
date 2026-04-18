(ns blackjack.hand
  (:require [blackjack.card :as card]))

(defn player-hand-value
  [cards count-method]
  (let [total (reduce (fn [acc c] (+ acc (card/card-val c count-method acc)))
                      0
                      cards)]
    (if (and (= count-method :soft) (> total 21))
      (player-hand-value cards :hard)
      total)))

(defn dealer-hand-value
  [{:keys [cards hide-down-card]} count-method]
  (let [visible (if hide-down-card (rest cards) cards)
        total (reduce (fn [acc c] (+ acc (card/card-val c count-method acc)))
                      0
                      visible)]
    (if (and (= count-method :soft) (> total 21))
      (dealer-hand-value {:cards cards :hide-down-card hide-down-card} :hard)
      total)))

(defn busted? [cards]
  (> (player-hand-value cards :soft) 21))

(defn dealer-busted? [dealer-hand]
  (> (dealer-hand-value dealer-hand :soft) 21))

(defn blackjack? [cards]
  (and (= 2 (count cards))
       (some card/ace? cards)
       (some card/ten? cards)))

(defn dealer-upcard-is-ace?
  [{:keys [cards]}]
  (card/ace? (nth cards 1)))
