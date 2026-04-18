(ns blackjack.hand-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.hand :as hand]))

(def ace   {:value 0 :suit 0})
(def two   {:value 1 :suit 0})
(def five  {:value 4 :suit 0})
(def six   {:value 5 :suit 0})
(def seven {:value 6 :suit 0})
(def nine  {:value 8 :suit 0})
(def ten   {:value 9 :suit 0})
(def king  {:value 12 :suit 0})

(deftest player-hand-value-test
  (testing "simple totals"
    (is (= 7 (hand/player-hand-value [two five] :soft)))
    (is (= 7 (hand/player-hand-value [two five] :hard))))
  (testing "ace as 11 in soft mode"
    (is (= 21 (hand/player-hand-value [ace king] :soft)))
    (is (= 11 (hand/player-hand-value [ace king] :hard))))
  (testing "soft falls back to hard when soft would bust"
    (is (= 13 (hand/player-hand-value [ace two ten] :soft))))
  (testing "two aces — one counts as 11, the other as 1"
    (is (= 12 (hand/player-hand-value [ace ace] :soft))))
  (testing "bust"
    (is (= 25 (hand/player-hand-value [king nine six] :soft)))))

(deftest dealer-hand-value-test
  (testing "down-card hidden — only upcard counts"
    (is (= 10 (hand/dealer-hand-value {:cards [ace king] :hide-down-card true} :soft))))
  (testing "down-card revealed — both count"
    (is (= 21 (hand/dealer-hand-value {:cards [ace king] :hide-down-card false} :soft))))
  (testing "soft falls back to hard"
    (is (= 13 (hand/dealer-hand-value {:cards [ace two ten] :hide-down-card false} :soft)))))

(deftest busted?-test
  (is (hand/busted? [king nine six]))
  (is (not (hand/busted? [king king])))
  (is (not (hand/busted? [ace king]))))

(deftest dealer-busted?-test
  (is (hand/dealer-busted? {:cards [king nine six] :hide-down-card false}))
  (is (not (hand/dealer-busted? {:cards [king king] :hide-down-card false}))))

(deftest blackjack?-test
  (testing "ace + ten value card = blackjack"
    (is (hand/blackjack? [ace king]))
    (is (hand/blackjack? [king ace]))
    (is (hand/blackjack? [ace ten])))
  (testing "21 with three cards is not blackjack"
    (is (not (hand/blackjack? [seven seven seven]))))
  (testing "20 is not blackjack"
    (is (not (hand/blackjack? [king king])))))

(deftest dealer-upcard-is-ace?-test
  (testing "upcard is index 1"
    (is (hand/dealer-upcard-is-ace? {:cards [king ace]}))
    (is (not (hand/dealer-upcard-is-ace? {:cards [ace king]})))))
