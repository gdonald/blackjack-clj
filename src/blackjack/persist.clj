(ns blackjack.persist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [blackjack.money :as money]))

(def default-file "blackjack.txt")

(defn- parse-deck-type [s]
  (case s
    "regular" :regular "aces" :aces "jacks" :jacks
    "aces-jacks" :aces-jacks "sevens" :sevens "eights" :eights
    :regular))

(defn- parse-face-type [s]
  (case s "alternate" :alternate :regular))

(defn- format-deck-type [k]
  (name k))

(defn- format-face-type [k]
  (name k))

(defn parse
  [s]
  (when (and s (string? s))
    (let [parts (str/split s #"\|")]
      (when (= 5 (count parts))
        (let [[d dt ft m b] parts]
          (try
            {:num-decks (Integer/parseInt d)
             :deck-type (parse-deck-type dt)
             :face-type (parse-face-type ft)
             :money (Integer/parseInt m)
             :current-bet (Integer/parseInt b)}
            (catch NumberFormatException _ nil)))))))

(defn serialize
  [{:keys [num-decks deck-type face-type money current-bet]}]
  (format "%d|%s|%s|%d|%d"
          num-decks
          (format-deck-type deck-type)
          (format-face-type face-type)
          money
          current-bet))

(defn load-state
  [path]
  (try
    (when (.exists (io/file path))
      (parse (slurp path)))
    (catch Exception _ nil)))

(defn save-state
  [path game]
  (try
    (let [f (io/file path)]
      (when-let [parent (.getParentFile f)]
        (.mkdirs parent))
      (spit f (serialize game)))
    (catch Exception _ nil)))

(defn merge-loaded
  [game loaded]
  (if (nil? loaded)
    game
    (let [g (merge game loaded)]
      (-> g
          (update :num-decks money/normalize-num-decks (:deck-type g))
          (update :current-bet money/normalize-bet (:money g))))))
