(ns blackjack.game
  (:require [blackjack.hand :as hand]
            [blackjack.money :as money]
            [blackjack.shoe :as shoe]))

(def max-player-hands 7)

(def initial-money 10000)
(def initial-bet 500)

(defn new-game
  []
  {:id 0
   :shoe []
   :dealer-hand nil
   :player-hands []
   :num-decks 1
   :deck-type :regular
   :face-type :regular
   :money initial-money
   :current-bet initial-bet
   :current-player-hand 0
   :current-menu :game
   :quitting false})

(defn current-player-hand [{:keys [player-hands current-player-hand]}]
  (nth player-hands current-player-hand))

(defn all-bets [{:keys [player-hands]}]
  (reduce + (map :bet player-hands)))

(defn- next-id [game]
  [(inc (:id game)) (update game :id inc)])

(defn ensure-shoe
  [{:keys [num-decks deck-type id] :as game} rng]
  (if (shoe/need-to-shuffle? game)
    (let [[new-shoe new-id] (shoe/build-shoe num-decks deck-type id rng)]
      (assoc game :shoe new-shoe :id new-id))
    game))

(defn- draw-card
  [{:keys [shoe] :as game}]
  [(first shoe) (assoc game :shoe (vec (rest shoe)))])

(defn- deal-card-to-player
  [game]
  (let [[card game] (draw-card game)
        idx (:current-player-hand game)]
    (update-in game [:player-hands idx :cards] (fnil conj []) card)))

(defn- deal-card-to-dealer [game]
  (let [[card game] (draw-card game)]
    (update-in game [:dealer-hand :cards] (fnil conj []) card)))

(defn- no-more-actions? [{:keys [cards played stood]}]
  (or played
      stood
      (hand/blackjack? cards)
      (hand/busted? cards)
      (= 21 (hand/player-hand-value cards :soft))))

(defn dealer-has-blackjack? [game]
  (hand/blackjack? (get-in game [:dealer-hand :cards])))

(defn- pay-won-hand
  [game idx]
  (let [{:keys [bet cards]} (get-in game [:player-hands idx])
        bet (if (hand/blackjack? cards)
              (long (* 1.5 bet))
              bet)]
    (-> game
        (update :money + bet)
        (assoc-in [:player-hands idx :bet] bet)
        (assoc-in [:player-hands idx :status] :won))))

(defn- collect-lost-hand [game idx]
  (let [bet (get-in game [:player-hands idx :bet])]
    (-> game
        (update :money - bet)
        (assoc-in [:player-hands idx :status] :lost))))

(defn- collect-busted-hand [game idx]
  (let [bet (get-in game [:player-hands idx :bet])]
    (-> game
        (update :money - bet)
        (assoc-in [:player-hands idx :status] :lost)
        (assoc-in [:player-hands idx :paid] true))))

(defn- player-hand-won?
  [player-value dealer-value dealer-busted?]
  (or dealer-busted?
      (> player-value dealer-value)))

(defn- player-hand-lost?
  [player-value dealer-value]
  (< player-value dealer-value))

(defn- pay-player-hand [game idx]
  (let [{:keys [paid cards]} (get-in game [:player-hands idx])]
    (if paid
      game
      (let [player-val (hand/player-hand-value cards :soft)
            dealer-val (hand/dealer-hand-value (:dealer-hand game) :soft)
            dealer-busted? (hand/dealer-busted? (:dealer-hand game))
            game (assoc-in game [:player-hands idx :paid] true)]
        (cond
          (player-hand-won? player-val dealer-val dealer-busted?)
          (pay-won-hand game idx)

          (player-hand-lost? player-val dealer-val)
          (collect-lost-hand game idx)

          :else
          (assoc-in game [:player-hands idx :status] :push))))))

(defn- pay-hands [game]
  (let [game (reduce pay-player-hand
                     game
                     (range (count (:player-hands game))))]
    (update game :current-bet money/normalize-bet (:money game))))

(defn- player-hand-done?
  [game idx]
  (let [hand (get-in game [:player-hands idx])]
    (if (no-more-actions? hand)
      (let [game (assoc-in game [:player-hands idx :played] true)
            game (if (and (not (:paid hand))
                          (hand/busted? (:cards hand)))
                   (collect-busted-hand game idx)
                   game)]
        [true game])
      [false game])))

(defn- need-to-play-dealer? [{:keys [player-hands]}]
  (some (fn [{:keys [cards]}]
          (not (or (hand/busted? cards)
                   (hand/blackjack? cards))))
        player-hands))

(defn- deal-required-dealer-cards [game]
  (loop [game game]
    (let [dh (:dealer-hand game)
          soft (hand/dealer-hand-value (assoc dh :hide-down-card false) :soft)
          hard (hand/dealer-hand-value (assoc dh :hide-down-card false) :hard)]
      (if (and (< soft 18) (< hard 17))
        (recur (deal-card-to-dealer game))
        game))))

(defn play-dealer-hand
  [game]
  (let [playing? (need-to-play-dealer? game)
        dealer-bj? (hand/blackjack? (get-in game [:dealer-hand :cards]))
        game (if (or playing? dealer-bj?)
               (assoc-in game [:dealer-hand :hide-down-card] false)
               game)
        game (if playing? (deal-required-dealer-cards game) game)
        game (assoc-in game [:dealer-hand :played] true)
        game (pay-hands game)]
    (assoc game :current-menu :game)))

(defn- on-hand-menu? [game]
  (= :hand (:current-menu game)))

(defn can-hit? [game]
  (and (on-hand-menu? game)
       (let [{:keys [cards played stood]} (current-player-hand game)]
         (not (or played
                  stood
                  (hand/blackjack? cards)
                  (hand/busted? cards)
                  (= 21 (hand/player-hand-value cards :soft)))))))

(defn can-stand? [game]
  (and (on-hand-menu? game)
       (let [{:keys [cards stood]} (current-player-hand game)]
         (not (or stood
                  (hand/busted? cards)
                  (hand/blackjack? cards))))))

(defn can-split? [game]
  (and (on-hand-menu? game)
       (let [{:keys [cards stood bet]} (current-player-hand game)
             [c0 c1] cards]
         (and (not stood)
              (< (count (:player-hands game)) max-player-hands)
              (>= (:money game) (+ (all-bets game) bet))
              (= 2 (count cards))
              (= (:value c0) (:value c1))))))

(defn can-double? [game]
  (and (on-hand-menu? game)
       (let [{:keys [cards stood bet]} (current-player-hand game)]
         (and (>= (:money game) (+ (all-bets game) bet))
              (not (or stood
                       (not= 2 (count cards))
                       (hand/blackjack? cards)))))))

(defn- more-hands-to-play? [{:keys [current-player-hand player-hands]}]
  (< current-player-hand (dec (count player-hands))))

(declare ask-hand-or-process)

(defn- play-more-hands
  [game]
  (let [game (update game :current-player-hand inc)
        game (deal-card-to-player game)
        idx (:current-player-hand game)
        [done? game] (player-hand-done? game idx)]
    (if done?
      (ask-hand-or-process game)
      (assoc game :current-menu :hand))))

(defn- ask-hand-or-process
  [game]
  (if (more-hands-to-play? game)
    (play-more-hands game)
    (play-dealer-hand game)))

(defn hit [game]
  (let [game (deal-card-to-player game)
        idx (:current-player-hand game)
        [done? game] (player-hand-done? game idx)]
    (if done?
      (ask-hand-or-process game)
      (assoc game :current-menu :hand))))

(defn stand [game]
  (let [idx (:current-player-hand game)
        game (-> game
                 (assoc-in [:player-hands idx :stood] true)
                 (assoc-in [:player-hands idx :played] true))]
    (ask-hand-or-process game)))

(defn double-down [game]
  (let [game (deal-card-to-player game)
        idx (:current-player-hand game)
        game (-> game
                 (assoc-in [:player-hands idx :played] true)
                 (update-in [:player-hands idx :bet] * 2))
        [_ game] (player-hand-done? game idx)]
    (ask-hand-or-process game)))

(defn split [game]
  (let [idx (:current-player-hand game)
        {:keys [bet cards]} (get-in game [:player-hands idx])
        [new-id game] (next-id game)
        new-hand {:id new-id :cards [(nth cards 1)] :bet bet
                  :status :unknown :played false :stood false :paid false}
        game (update game :player-hands
                     (fn [hs]
                       (vec (concat (subvec hs 0 (inc idx))
                                    [new-hand]
                                    (subvec hs (inc idx))))))
        game (assoc-in game [:player-hands idx :cards] [(nth cards 0)])
        game (deal-card-to-player game)
        [done? game] (player-hand-done? game idx)]
    (if done?
      (ask-hand-or-process game)
      (assoc game :current-menu :hand))))

(defn insure-hand
  [game]
  (let [idx (:current-player-hand game)
        bet (get-in game [:player-hands idx :bet])
        insurance-bet (quot bet 2)
        dealer-bj? (dealer-has-blackjack? game)
        game (if dealer-bj?
               (update game :money + (* 2 insurance-bet))
               (update game :money - insurance-bet))]
    (if dealer-bj?
      (-> game
          (assoc-in [:dealer-hand :hide-down-card] false)
          pay-hands
          (assoc :current-menu :game))
      (assoc game :current-menu :hand))))

(defn no-insurance
  [game]
  (if (dealer-has-blackjack? game)
    (-> game
        (assoc-in [:dealer-hand :hide-down-card] false)
        pay-hands
        (assoc :current-menu :game))
    (assoc game :current-menu :hand)))

(defn deal-new-hand
  ([game] (deal-new-hand game (java.util.Random.)))
  ([game rng]
   (let [game (ensure-shoe game rng)
         [pid game] (next-id game)
         player-hand {:id pid :cards [] :bet (:current-bet game)
                      :status :unknown :played false :stood false :paid false}
         game (assoc game
                     :player-hands [player-hand]
                     :dealer-hand {:cards [] :hide-down-card true :played false}
                     :current-player-hand 0)
         game (-> game
                  deal-card-to-player
                  deal-card-to-dealer
                  deal-card-to-player
                  deal-card-to-dealer)
         player-cards (get-in game [:player-hands 0 :cards])]
     (cond
       (and (hand/dealer-upcard-is-ace? (:dealer-hand game))
            (not (hand/blackjack? player-cards)))
       (assoc game :current-menu :insurance)

       :else
       (let [[done? game] (player-hand-done? game 0)]
         (if done?
           (-> game
               (assoc-in [:dealer-hand :hide-down-card] false)
               pay-hands
               (assoc :current-menu :game))
           (assoc game :current-menu :hand)))))))

(defn set-num-decks [game n]
  (let [n (money/normalize-num-decks n (:deck-type game))]
    (-> game
        (assoc :num-decks n :shoe [])
        (assoc :current-menu :game))))

(defn set-deck-type [game deck-type]
  (let [game (assoc game :deck-type deck-type)
        game (update game :num-decks money/normalize-num-decks deck-type)]
    (assoc game :shoe [] :current-menu :game)))

(defn set-face-type [game face-type]
  (assoc game :face-type face-type :current-menu :game))

(defn set-bet [game new-bet]
  (-> game
      (assoc :current-bet new-bet)
      (update :current-bet money/normalize-bet (:money game))
      (assoc :current-menu :game)))

(defn quit [game]
  (assoc game :quitting true))
