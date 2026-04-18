(ns blackjack.shoe-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.shoe :as shoe]))

(deftest total-cards-test
  (is (= 52 (shoe/total-cards 1)))
  (is (= 416 (shoe/total-cards 8))))

(deftest card-values-test
  (is (= [0 1 2 3 4 5 6 7 8 9 10 11 12] (vec (shoe/card-values :regular))))
  (is (= [0] (vec (shoe/card-values :aces))))
  (is (= [10] (vec (shoe/card-values :jacks))))
  (is (= [0 10] (vec (shoe/card-values :aces-jacks))))
  (is (= [6] (vec (shoe/card-values :sevens))))
  (is (= [7] (vec (shoe/card-values :eights)))))

(deftest need-to-shuffle?-test
  (testing "empty shoe always needs shuffle"
    (is (shoe/need-to-shuffle? {:shoe [] :num-decks 1})))
  (testing "fresh shoe never needs shuffle"
    (is (not (shoe/need-to-shuffle? {:shoe (vec (range 52)) :num-decks 1}))))
  (testing "1-deck threshold is used > 80% of remaining"
    (is (not (shoe/need-to-shuffle? {:shoe (vec (range 29)) :num-decks 1})))
    (is (shoe/need-to-shuffle? {:shoe (vec (range 28)) :num-decks 1}))))

(deftest build-shoe-test
  (testing "regular deck has 52 unique cards with all suits and values"
    (let [[s next-id] (shoe/build-shoe 1 :regular 0)]
      (is (= 52 (count s)))
      (is (= 52 next-id))
      (is (= 52 (count (set (map :id s)))))
      (is (= #{0 1 2 3 4 5 6 7 8 9 10 11 12} (set (map :value s))))
      (is (= #{0 1 2 3} (set (map :suit s))))))
  (testing "aces deck has only aces"
    (let [[s _] (shoe/build-shoe 1 :aces 0)]
      (is (= 52 (count s)))
      (is (every? #(= 0 (:value %)) s))))
  (testing "8 decks have 416 cards"
    (let [[s _] (shoe/build-shoe 8 :regular 0)]
      (is (= 416 (count s))))))

(deftest build-shoe-deterministic-test
  (testing "deterministic build with seeded rng is reproducible"
    (let [rng1 (java.util.Random. 42)
          rng2 (java.util.Random. 42)
          [s1 _] (shoe/build-shoe 1 :regular 0 rng1)
          [s2 _] (shoe/build-shoe 1 :regular 0 rng2)]
      (is (= s1 s2)))))

(deftest build-shoe-no-shuffle-test
  (testing "deterministic build of an unshuffled regular deck"
    (let [[s next-id] (shoe/build-shoe-no-shuffle 1 :regular 0)]
      (is (= 52 (count s)))
      (is (= 52 next-id))
      (is (= {:id 1 :value 12 :suit 0} (first s)))))
  (testing "no-shuffle build for aces deck — needs multiple iterations"
    (let [[s _] (shoe/build-shoe-no-shuffle 2 :aces 0)]
      (is (= 104 (count s)))
      (is (every? #(= 0 (:value %)) s))))
  (testing "exercises all deck types"
    (doseq [dt [:regular :aces :jacks :aces-jacks :sevens :eights]]
      (let [[s _] (shoe/build-shoe-no-shuffle 1 dt 0)]
        (is (pos? (count s)))))))
