(ns blackjack.core
  (:require [blackjack.game :as game]
            [blackjack.persist :as persist]
            [blackjack.term :as term]
            [blackjack.ui :as ui])
  (:gen-class))

(defn dispatch-game-menu
  [game k]
  (let [b ui/key-bindings]
    (cond
      (= k (b :deal-double)) (game/deal-new-hand game)
      (= k (b :bet-back))    (assoc game :current-menu :bet)
      (= k (b :options))     (assoc game :current-menu :options)
      (= k (b :quit))        (game/quit game)
      :else game)))

(defn dispatch-hand-menu [game k]
  (let [b ui/key-bindings]
    (cond
      (and (= k (b :hit))         (game/can-hit? game))    (game/hit game)
      (and (= k (b :stand))       (game/can-stand? game))  (game/stand game)
      (and (= k (b :split))       (game/can-split? game))  (game/split game)
      (and (= k (b :deal-double)) (game/can-double? game)) (game/double-down game)
      :else game)))

(defn dispatch-insurance-menu [game k]
  (let [b ui/key-bindings]
    (cond
      (= k (b :insurance))              (game/insure-hand game)
      (= k (b :num-decks-no-insurance)) (game/no-insurance game)
      :else game)))

(defn dispatch-options-menu [game k]
  (let [b ui/key-bindings]
    (cond
      (= k (b :num-decks-no-insurance)) (assoc game :current-menu :num-decks)
      (= k (b :deck-type))              (assoc game :current-menu :deck-type)
      (= k (b :face-type))              (assoc game :current-menu :face-type)
      (= k (b :bet-back))               (assoc game :current-menu :game)
      :else game)))

(defn dispatch-deck-type-menu [game k]
  (let [b ui/key-bindings
        deck-type (cond
                    (= k (b :deck-regular))    :regular
                    (= k (b :deck-aces))       :aces
                    (= k (b :deck-jacks))      :jacks
                    (= k (b :deck-aces-jacks)) :aces-jacks
                    (= k (b :deck-sevens))     :sevens
                    (= k (b :deck-eights))     :eights)]
    (if deck-type
      (game/set-deck-type game deck-type)
      game)))

(defn dispatch-face-type-menu [game k]
  (let [b ui/key-bindings]
    (cond
      (= k (b :face-regular))   (game/set-face-type game :regular)
      (= k (b :face-alternate)) (game/set-face-type game :alternate)
      :else game)))

(defn- read-bet [game]
  (let [dollars (term/with-buffered-mode
                  #(ui/read-line-int "  New bet: "))]
    (if dollars
      (game/set-bet game (* 100 dollars))
      (assoc game :current-menu :game))))

(defn- read-num-decks [game]
  (let [n (term/with-buffered-mode
            #(ui/read-line-int "  Number of decks (1-8): "))]
    (if n
      (game/set-num-decks game n)
      (assoc game :current-menu :game))))

(defn step
  [game persist-path]
  (ui/draw game)
  (case (:current-menu game)
    :bet       (let [g (read-bet game)]
                 (persist/save-state persist-path g)
                 (game/deal-new-hand g))
    :num-decks (let [g (read-num-decks game)]
                 (persist/save-state persist-path g)
                 (game/deal-new-hand g))
    (let [k (ui/read-key)]
      (if (nil? k)
        (game/quit game)
        (let [g (case (:current-menu game)
                  :game       (dispatch-game-menu game k)
                  :hand       (dispatch-hand-menu game k)
                  :insurance  (dispatch-insurance-menu game k)
                  :options    (dispatch-options-menu game k)
                  :deck-type  (dispatch-deck-type-menu game k)
                  :face-type  (dispatch-face-type-menu game k)
                  game)]
          (persist/save-state persist-path g)
          g)))))

(defn- game-loop [game persist-path]
  (loop [g game]
    (if (:quitting g)
      g
      (recur (step g persist-path)))))

(defn run
  [persist-path]
  (let [initial (-> (game/new-game)
                    (persist/merge-loaded (persist/load-state persist-path))
                    game/deal-new-hand)]
    (if (term/tty?)
      (let [saved (term/enable-unbuffered-mode!)]
        (binding [term/*saved* saved]
          (term/install-shutdown-hook! saved)
          (try
            (game-loop initial persist-path)
            (finally
              (term/restore! saved)))))
      (game-loop initial persist-path))))

(defn -main [& _args]
  (run persist/default-file))
