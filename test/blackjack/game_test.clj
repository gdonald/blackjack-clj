(ns blackjack.game-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.game :as game]))

(defn card [value suit] {:id 0 :value value :suit suit})

(def ace   (card 0 0))
(def two   (card 1 0))
(def three (card 2 0))
(def four  (card 3 0))
(def five  (card 4 0))
(def six   (card 5 0))
(def seven (card 6 0))
(def eight (card 7 0))
(def nine  (card 8 0))
(def ten   (card 9 0))
(def king  (card 12 0))

(defn ph
  [cards & {:keys [bet status played stood paid]
            :or {bet 500 status :unknown played false stood false paid false}}]
  {:id 1 :cards cards :bet bet :status status
   :played played :stood stood :paid paid})

(defn dh
  [cards & {:keys [hide-down-card played]
            :or {hide-down-card true played false}}]
  {:cards cards :hide-down-card hide-down-card :played played})

(defn make-game
  [& {:keys [shoe player-hands dealer-hand money current-bet
             current-menu current-player-hand num-decks deck-type
             face-type id]
      :or {shoe []
           player-hands []
           dealer-hand nil
           money 10000
           current-bet 500
           current-menu :game
           current-player-hand 0
           num-decks 1
           deck-type :regular
           face-type :regular
           id 0}}]
  {:id id :shoe shoe :player-hands player-hands :dealer-hand dealer-hand
   :money money :current-bet current-bet :current-menu current-menu
   :current-player-hand current-player-hand :num-decks num-decks
   :deck-type deck-type :face-type face-type :quitting false})

(deftest current-player-hand-test
  (let [g (make-game :player-hands [(ph [ace king]) (ph [two three])]
                     :current-player-hand 1)]
    (is (= [two three] (:cards (game/current-player-hand g))))))

(deftest all-bets-test
  (is (= 1500 (game/all-bets (make-game :player-hands [(ph [] :bet 500)
                                                       (ph [] :bet 1000)])))))

(deftest new-game-test
  (let [g (game/new-game)]
    (is (= 10000 (:money g)))
    (is (= 500 (:current-bet g)))
    (is (= 1 (:num-decks g)))
    (is (= :regular (:deck-type g)))
    (is (= :regular (:face-type g)))
    (is (= :game (:current-menu g)))
    (is (false? (:quitting g)))
    (is (empty? (:shoe g)))))

(deftest ensure-shoe-test
  (testing "empty shoe gets built"
    (let [g (game/ensure-shoe (game/new-game) (java.util.Random. 42))]
      (is (= 52 (count (:shoe g))))))
  (testing "full shoe is left alone"
    (let [g (assoc (game/new-game) :shoe (vec (range 52)))]
      (is (= (:shoe g) (:shoe (game/ensure-shoe g (java.util.Random.))))))))

(deftest can-hit?-test
  (testing "can hit with normal hand on :hand menu"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [five three])])]
      (is (game/can-hit? g))))
  (testing "cannot hit on a non-:hand menu"
    (let [g (make-game :current-menu :game
                       :player-hands [(ph [five three])])]
      (is (not (game/can-hit? g)))))
  (testing "cannot hit a stood hand"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [five three] :stood true)])]
      (is (not (game/can-hit? g)))))
  (testing "cannot hit on 21"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [seven seven seven])])]
      (is (not (game/can-hit? g)))))
  (testing "cannot hit a busted hand"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [king king five])])]
      (is (not (game/can-hit? g)))))
  (testing "cannot hit a played hand"
    (is (not (game/can-hit? (make-game :current-menu :hand
                                       :player-hands [(ph [five three] :played true)])))))
  (testing "cannot hit blackjack"
    (is (not (game/can-hit? (make-game :current-menu :hand
                                       :player-hands [(ph [ace king])]))))))

(deftest can-stand?-test
  (let [g (make-game :current-menu :hand
                     :player-hands [(ph [five three])])]
    (is (game/can-stand? g)))
  (let [g (make-game :current-menu :hand
                     :player-hands [(ph [ace king])])]
    (is (not (game/can-stand? g))))
  (let [g (make-game :current-menu :hand
                     :player-hands [(ph [king king five])])]
    (is (not (game/can-stand? g))))
  (testing "cannot stand when not on :hand menu"
    (is (not (game/can-stand? (make-game :current-menu :game
                                         :player-hands [(ph [five three])])))))
  (testing "cannot stand when already stood"
    (is (not (game/can-stand? (make-game :current-menu :hand
                                         :player-hands [(ph [five three] :stood true)]))))))

(deftest can-split?-test
  (testing "matching pair, enough money"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :player-hands [(ph [eight eight])])]
      (is (game/can-split? g))))
  (testing "non-matching pair"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [eight nine])])]
      (is (not (game/can-split? g)))))
  (testing "more than 2 cards"
    (let [g (make-game :current-menu :hand
                       :player-hands [(ph [eight eight three])])]
      (is (not (game/can-split? g)))))
  (testing "not enough money"
    (let [g (make-game :current-menu :hand
                       :money 600
                       :player-hands [(ph [eight eight] :bet 500)])]
      (is (not (game/can-split? g)))))
  (testing "max hands reached"
    (let [hands (repeat 7 (ph [eight eight]))
          g (make-game :current-menu :hand
                       :money 100000
                       :player-hands hands)]
      (is (not (game/can-split? g)))))
  (testing "cannot split when not on :hand menu"
    (is (not (game/can-split? (make-game :current-menu :game
                                         :player-hands [(ph [eight eight])])))))
  (testing "cannot split a stood hand"
    (is (not (game/can-split? (make-game :current-menu :hand
                                         :money 10000
                                         :player-hands [(ph [eight eight] :stood true)]))))))

(deftest can-double?-test
  (testing "two cards, enough money"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :player-hands [(ph [five three])])]
      (is (game/can-double? g))))
  (testing "more than 2 cards"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :player-hands [(ph [five three two])])]
      (is (not (game/can-double? g)))))
  (testing "blackjack cannot double"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :player-hands [(ph [ace king])])]
      (is (not (game/can-double? g)))))
  (testing "not enough money"
    (let [g (make-game :current-menu :hand
                       :money 600
                       :player-hands [(ph [five three] :bet 500)])]
      (is (not (game/can-double? g)))))
  (testing "cannot double when not on :hand menu"
    (is (not (game/can-double? (make-game :current-menu :game
                                          :money 10000
                                          :player-hands [(ph [five three])])))))
  (testing "cannot double a stood hand"
    (is (not (game/can-double? (make-game :current-menu :hand
                                          :money 10000
                                          :player-hands [(ph [five three] :stood true)]))))))

(deftest play-dealer-hand-player-wins-test
  (testing "player 20 vs dealer 17 → win"
    (let [g (make-game :money 10000
                       :player-hands [(ph [king ten] :bet 500)]
                       :dealer-hand (dh [seven ten] :hide-down-card true))
          g' (game/play-dealer-hand g)]
      (is (= 10500 (:money g')))
      (is (= :won (get-in g' [:player-hands 0 :status])))
      (is (false? (get-in g' [:dealer-hand :hide-down-card]))))))

(deftest play-dealer-hand-blackjack-payout-test
  (testing "blackjack pays 1.5x"
    (let [g (make-game :money 10000
                       :player-hands [(ph [ace king] :bet 500)]
                       :dealer-hand (dh [seven ten]))
          g' (game/play-dealer-hand g)]
      (is (= 10750 (:money g')))
      (is (= 750 (get-in g' [:player-hands 0 :bet])))
      (is (= :won (get-in g' [:player-hands 0 :status]))))))

(deftest play-dealer-hand-push-test
  (let [g (make-game :money 10000
                     :player-hands [(ph [king nine] :bet 500)]
                     :dealer-hand (dh [nine ten]))
        g' (game/play-dealer-hand g)]
    (is (= 10000 (:money g')))
    (is (= :push (get-in g' [:player-hands 0 :status])))))

(deftest play-dealer-hand-loss-test
  (let [g (make-game :money 10000
                     :player-hands [(ph [king six] :bet 500)]
                     :dealer-hand (dh [ten nine]))
        g' (game/play-dealer-hand g)]
    (is (= 9500 (:money g')))
    (is (= :lost (get-in g' [:player-hands 0 :status])))))

(deftest play-dealer-hand-dealer-busts-test
  (testing "dealer hits soft 17 (with our rule), busts"
    (let [g (make-game :money 10000
                       :player-hands [(ph [king five] :bet 500)]
                       :dealer-hand (dh [six seven]))
          g (assoc g :shoe [king king])
          g' (game/play-dealer-hand g)]
      (is (= 10500 (:money g')))
      (is (= :won (get-in g' [:player-hands 0 :status]))))))

(deftest play-dealer-hand-skipped-when-all-busted-test
  (testing "if all player hands are busted, dealer doesn't draw"
    (let [g (make-game :money 10000
                       :player-hands [(ph [king king five] :bet 500
                                          :paid true :status :lost)]
                       :dealer-hand (dh [king six]))
          g' (game/play-dealer-hand g)]
      (is (= 2 (count (get-in g' [:dealer-hand :cards]))))
      (is (= 10000 (:money g'))))))

(deftest hit-to-21-ends-hand-test
  (testing "hitting to soft 21 marks hand done and plays dealer"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe [seven]
                       :player-hands [(ph [seven seven] :bet 500)]
                       :dealer-hand (dh [seven king]))
          g' (game/hit g)]
      (is (= :game (:current-menu g'))))))

(deftest hit-test
  (testing "hit deals a card and stays on :hand if not done"
    (let [g (make-game :current-menu :hand
                       :shoe [three]
                       :player-hands [(ph [five three])]
                       :dealer-hand (dh [seven ten]))
          g' (game/hit g)]
      (is (= 3 (count (get-in g' [:player-hands 0 :cards]))))
      (is (= :hand (:current-menu g')))))
  (testing "hit that busts collects bet, plays dealer, lands on :game"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe [king]
                       :player-hands [(ph [king five] :bet 500)]
                       :dealer-hand (dh [seven ten]))
          g' (game/hit g)]
      (is (= :lost (get-in g' [:player-hands 0 :status])))
      (is (= 9500 (:money g')))
      (is (= :game (:current-menu g'))))))

(deftest hit-on-already-played-hand-exercises-no-more-actions-test
  (let [g (make-game :current-menu :hand
                     :money 10000
                     :shoe [two]
                     :player-hands [(ph [five three] :played true :paid true
                                        :status :won :bet 500)]
                     :dealer-hand (dh [seven king]))
        g' (game/hit g)]
    (is (= :game (:current-menu g')))))

(deftest hit-on-stood-hand-exercises-no-more-actions-stood-branch-test
  (let [g (make-game :current-menu :hand
                     :money 10000
                     :shoe [two]
                     :player-hands [(ph [five three] :stood true :played false)]
                     :dealer-hand (dh [seven king]))
        g' (game/hit g)]
    (is (= :game (:current-menu g')))))

(deftest stand-test
  (testing "stand ends the hand and plays dealer"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :player-hands [(ph [king nine] :bet 500)]
                       :dealer-hand (dh [seven ten]))
          g' (game/stand g)]
      (is (true? (get-in g' [:player-hands 0 :stood])))
      (is (= 10500 (:money g')))
      (is (= :game (:current-menu g'))))))

(deftest double-down-test
  (testing "double doubles bet, deals one card, ends hand"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe [three]
                       :player-hands [(ph [five four] :bet 500)]
                       :dealer-hand (dh [seven ten]))
          g' (game/double-down g)]
      (is (= 1000 (get-in g' [:player-hands 0 :bet])))
      (is (= 3 (count (get-in g' [:player-hands 0 :cards]))))
      (is (true? (get-in g' [:player-hands 0 :played])))
      (is (= :game (:current-menu g'))))))

(deftest split-test
  (testing "split creates new hand with second card, deals to original"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe [seven six]
                       :player-hands [(ph [eight eight] :bet 500)]
                       :dealer-hand (dh [seven ten]))
          g' (game/split g)]
      (is (= 2 (count (:player-hands g'))))
      (is (= [eight seven] (get-in g' [:player-hands 0 :cards])))
      (is (= [eight] (get-in g' [:player-hands 1 :cards])))
      (is (= 500 (get-in g' [:player-hands 1 :bet])))
      (is (= :hand (:current-menu g')))))

  (testing "split inserted at correct position when current is not last"
    (let [g (make-game :current-menu :hand
                       :money 100000
                       :shoe [seven]
                       :player-hands [(ph [two three] :bet 500 :stood true :played true)
                                      (ph [eight eight] :bet 500)]
                       :current-player-hand 1
                       :dealer-hand (dh [seven ten]))
          g' (game/split g)]
      (is (= 3 (count (:player-hands g'))))
      (is (= [two three] (get-in g' [:player-hands 0 :cards])))
      (is (= [eight seven] (get-in g' [:player-hands 1 :cards])))
      (is (= [eight] (get-in g' [:player-hands 2 :cards]))))))

(deftest split-then-play-more-hands-test
  (testing "after splitting, finishing the first hand advances to the second"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe (vec (concat [ace five] (repeat 30 two)))
                       :player-hands [(ph [ten ten] :bet 500)]
                       :dealer-hand (dh [seven king]))
          g' (game/split g)]
      (is (= :hand (:current-menu g')))
      (is (= 1 (:current-player-hand g')))
      (is (= [ten five] (get-in g' [:player-hands 1 :cards]))))))

(deftest split-aces-both-21-test
  (testing "split aces, deal 10 to each: both done, dealer plays"
    (let [g (make-game :current-menu :hand
                       :money 10000
                       :shoe (vec (concat [ten king] (repeat 30 two)))
                       :player-hands [(ph [ace ace] :bet 500)]
                       :dealer-hand (dh [seven king]))
          g' (game/split g)]
      (is (= :game (:current-menu g'))))))

(deftest insure-hand-dealer-has-blackjack-test
  (testing "insurance pays 2:1; main hand loses (or pushes if also BJ)"
    (let [g (make-game :money 10000
                       :current-menu :insurance
                       :player-hands [(ph [king nine] :bet 500)]
                       :dealer-hand (dh [ace king]))
          g' (game/insure-hand g)]
      (is (= 10000 (:money g')))
      (is (= :game (:current-menu g'))))))

(deftest insure-hand-dealer-no-blackjack-test
  (testing "insurance is lost; main hand continues"
    (let [g (make-game :money 10000
                       :current-menu :insurance
                       :player-hands [(ph [king nine] :bet 500)]
                       :dealer-hand (dh [ace nine]))
          g' (game/insure-hand g)]
      (is (= 9750 (:money g')))
      (is (= :hand (:current-menu g'))))))

(deftest no-insurance-dealer-has-blackjack-test
  (testing "decline insurance, dealer has BJ → reveal and settle"
    (let [g (make-game :money 10000
                       :current-menu :insurance
                       :player-hands [(ph [king nine] :bet 500)]
                       :dealer-hand (dh [ace king]))
          g' (game/no-insurance g)]
      (is (= 9500 (:money g')))
      (is (= :game (:current-menu g')))
      (is (false? (get-in g' [:dealer-hand :hide-down-card]))))))

(deftest no-insurance-dealer-no-blackjack-test
  (testing "decline insurance, dealer has no BJ → continue"
    (let [g (make-game :money 10000
                       :current-menu :insurance
                       :player-hands [(ph [king nine] :bet 500)]
                       :dealer-hand (dh [ace nine]))
          g' (game/no-insurance g)]
      (is (= 10000 (:money g')))
      (is (= :hand (:current-menu g'))))))

(deftest deal-new-hand-normal-deal-test
  (testing "two cards each, on :hand menu"
    (let [g (game/deal-new-hand (game/new-game) (java.util.Random. 42))]
      (is (= 2 (count (get-in g [:player-hands 0 :cards]))))
      (is (= 2 (count (get-in g [:dealer-hand :cards]))))
      (is (true? (get-in g [:dealer-hand :hide-down-card])))
      (is (contains? #{:hand :insurance :game} (:current-menu g))))))

(defn- shoe-with-head [head] (vec (concat head (repeat 30 two))))

(deftest deal-new-hand-player-blackjack-test
  (testing "player blackjack with no dealer ace → settles immediately"
    (let [g (make-game :shoe (shoe-with-head [ace seven king ten]))
          g' (game/deal-new-hand g (java.util.Random.))]
      (is (= :game (:current-menu g')))
      (is (= :won (get-in g' [:player-hands 0 :status])))
      (is (= 10750 (:money g'))))))

(deftest deal-new-hand-dealer-ace-prompts-insurance-test
  (testing "dealer upcard ace + player not BJ → :insurance menu"
    (let [g (make-game :shoe (shoe-with-head [king seven nine ace]))
          g' (game/deal-new-hand g (java.util.Random.))]
      (is (= :insurance (:current-menu g')))
      (is (true? (get-in g' [:dealer-hand :hide-down-card]))))))

(deftest set-num-decks-test
  (let [g (game/set-num-decks (game/new-game) 4)]
    (is (= 4 (:num-decks g)))
    (is (empty? (:shoe g)))
    (is (= :game (:current-menu g))))
  (testing "clamps to 1..8"
    (is (= 8 (:num-decks (game/set-num-decks (game/new-game) 99))))
    (is (= 1 (:num-decks (game/set-num-decks (game/new-game) 0))))))

(deftest set-deck-type-test
  (let [g (game/set-deck-type (game/new-game) :aces)]
    (is (= :aces (:deck-type g)))
    (is (= 2 (:num-decks g)))
    (is (empty? (:shoe g)))))

(deftest set-face-type-test
  (let [g (game/set-face-type (game/new-game) :alternate)]
    (is (= :alternate (:face-type g)))))

(deftest set-bet-test
  (let [g (game/set-bet (game/new-game) 1000)]
    (is (= 1000 (:current-bet g))))
  (testing "clamps to money"
    (let [g (game/set-bet (assoc (game/new-game) :money 800) 5000)]
      (is (= 800 (:current-bet g))))))

(deftest quit-test
  (is (true? (:quitting (game/quit (game/new-game))))))
