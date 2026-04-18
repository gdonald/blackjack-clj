(ns blackjack.ui
  (:require [clojure.string :as str]
            [blackjack.card :as card]
            [blackjack.game :as game]
            [blackjack.hand :as hand]
            [blackjack.money :as money]))

(def key-bindings
  {:stand "s"
   :deal-double "d"
   :bet-back "b"
   :quit "q"
   :options "o"
   :num-decks-no-insurance "n"
   :deck-type "t"
   :face-type "f"
   :hit "h"
   :split "p"
   :insurance "y"
   :face-regular "r"
   :face-alternate "a"
   :deck-regular "1"
   :deck-aces "2"
   :deck-jacks "3"
   :deck-aces-jacks "4"
   :deck-sevens "5"
   :deck-eights "6"})

(defn dealer-hand-str [{:keys [face-type] :as game}]
  (let [{:keys [cards hide-down-card] :as dh} (:dealer-hand game)
        rendered (->> cards
                      (map-indexed
                       (fn [i c]
                         (if (and (zero? i) hide-down-card)
                           (apply card/face face-type card/down-card)
                           (card/face face-type (:value c) (:suit c)))))
                      (str/join " "))]
    (str "  " rendered "  ⇒  " (hand/dealer-hand-value dh :soft))))

(defn- player-hand-status-str [{:keys [cards status]}]
  (cond
    (= :lost status) (if (hand/busted? cards) "Busted!" "Lost!")
    (= :won status)  (if (hand/blackjack? cards) "Blackjack!" "Won!")
    (= :push status) "Push"
    :else ""))

(defn- player-hand-money-str
  [{:keys [current-player-hand]} {:keys [played status bet]} index]
  (str (when (= status :lost) "-")
       (when (= status :won) "+")
       money/currency
       (money/format-money bet)
       (when (and (not played) (= index current-player-hand))
         " ⇐")
       "  "))

(defn player-hand-str [{:keys [face-type] :as game} {:keys [cards] :as ph} index]
  (let [rendered (->> cards
                      (map (fn [c] (card/face face-type (:value c) (:suit c))))
                      (str/join " "))
        value (hand/player-hand-value cards :soft)]
    (str "  " rendered "  ⇒  " value "  "
         (player-hand-money-str game ph index)
         (player-hand-status-str ph))))

(defn- hand-menu-str [game]
  (let [k key-bindings]
    (str/join "  "
              (cond-> []
                (game/can-hit? game)    (conj (str "(" (k :hit) ") hit"))
                (game/can-stand? game)  (conj (str "(" (k :stand) ") stand"))
                (game/can-split? game)  (conj (str "(" (k :split) ") split"))
                (game/can-double? game) (conj (str "(" (k :deal-double) ") double"))))))

(defn menu-str [{:keys [current-menu] :as game}]
  (let [k key-bindings]
    (cond
      (= :game current-menu)
      (format "(%s) deal new hand  (%s) change bet  (%s) options  (%s) quit"
              (k :deal-double) (k :bet-back)
              (k :options) (k :quit))

      (= :hand current-menu)
      (hand-menu-str game)

      (= :options current-menu)
      (format "(%s) number of decks  (%s) deck type  (%s) face type  (%s) go back"
              (k :num-decks-no-insurance) (k :deck-type)
              (k :face-type) (k :bet-back))

      (= :deck-type current-menu)
      (format "(%s) regular  (%s) aces  (%s) jacks  (%s) aces & jacks  (%s) sevens  (%s) eights"
              (k :deck-regular) (k :deck-aces)
              (k :deck-jacks) (k :deck-aces-jacks)
              (k :deck-sevens) (k :deck-eights))

      (= :face-type current-menu)
      (format "(%s) A♠ regular  (%s) 🂡 alternate"
              (k :face-regular) (k :face-alternate))

      (= :insurance current-menu)
      (format "(%s) insure hand  (%s) refuse insurance"
              (k :insurance) (k :num-decks-no-insurance)))))

(defn header-str [game]
  (str "  " (menu-str game)))

(defn- prompt-menu? [game]
  (contains? #{:bet :num-decks} (:current-menu game)))

(defn render
  [{:keys [money player-hands] :as game}]
  (str "\n  Dealer:\n"
       (dealer-hand-str game) "\n"
       "\n  Player " money/currency (money/format-money money) ":\n"
       (str/join "\n\n"
                 (map-indexed (fn [i ph] (player-hand-str game ph i)) player-hands))
       (if (prompt-menu? game)
         "\n\n"
         (str "\n\n" (header-str game) "\n"))))

(defn clear-screen []
  (print "\033[H\033[2J")
  (flush))

(defn draw [game]
  (clear-screen)
  (if (prompt-menu? game)
    (do (print (render game)) (flush))
    (println (render game))))

(defn read-key
  []
  (let [c (.read *in*)]
    (when (>= c 0)
      (str (Character/toLowerCase (char c))))))

(defn read-line-int
  [prompt]
  (print prompt)
  (flush)
  (let [s (read-line)]
    (try (Integer/parseInt (str/trim (or s "")))
         (catch Exception _ nil))))
