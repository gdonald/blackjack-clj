(ns blackjack.money-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.money :as money]))

(deftest format-money-test
  (is (= "5.00" (money/format-money 500)))
  (is (= "100.00" (money/format-money 10000)))
  (is (= "0.01" (money/format-money 1)))
  (is (= "12.34" (money/format-money 1234))))

(deftest normalize-bet-test
  (testing "below min snaps up to min"
    (is (= money/min-bet (money/normalize-bet 100 100000))))
  (testing "above max snaps down to max"
    (is (= money/max-bet (money/normalize-bet (* 2 money/max-bet) (* 3 money/max-bet)))))
  (testing "above money snaps down to money"
    (is (= 700 (money/normalize-bet 1000 700))))
  (testing "in-range stays the same"
    (is (= 1500 (money/normalize-bet 1500 10000)))))

(deftest normalize-num-decks-test
  (testing "clamps 1..8"
    (is (= 1 (money/normalize-num-decks 0 :regular)))
    (is (= 1 (money/normalize-num-decks -3 :regular)))
    (is (= 8 (money/normalize-num-decks 9 :regular)))
    (is (= 5 (money/normalize-num-decks 5 :regular))))
  (testing "aces deck floor is 2"
    (is (= 2 (money/normalize-num-decks 1 :aces)))
    (is (= 3 (money/normalize-num-decks 3 :aces)))))
