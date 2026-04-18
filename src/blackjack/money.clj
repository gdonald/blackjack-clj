(ns blackjack.money)

(def currency "$")

(def min-bet 500)
(def max-bet 100000000)

(defn format-money
  [cents]
  (format "%.2f" (/ cents 100.0)))

(defn normalize-bet
  [current-bet money]
  (-> current-bet
      (max min-bet)
      (min max-bet)
      (min money)))

(defn normalize-num-decks
  [num-decks deck-type]
  (let [n (-> num-decks (max 1) (min 8))]
    (if (and (= deck-type :aces) (< n 2)) 2 n)))
