(ns blackjack.card-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.card :as card]))

(deftest ace?-test
  (is (card/ace? {:value 0}))
  (is (not (card/ace? {:value 1})))
  (is (not (card/ace? {:value 12}))))

(deftest ten?-test
  (testing "T, J, Q, K all count as 10"
    (is (card/ten? {:value 9}))
    (is (card/ten? {:value 10}))
    (is (card/ten? {:value 11}))
    (is (card/ten? {:value 12})))
  (testing "9 and below are not 10s"
    (is (not (card/ten? {:value 8})))
    (is (not (card/ten? {:value 0})))))

(deftest card-val-test
  (testing "non-ace cards count their pip value"
    (is (= 2 (card/card-val {:value 1} :soft 0)))
    (is (= 9 (card/card-val {:value 8} :soft 0))))
  (testing "T, J, Q, K count as 10"
    (is (= 10 (card/card-val {:value 9} :soft 0)))
    (is (= 10 (card/card-val {:value 12} :soft 0))))
  (testing "ace counts as 11 in soft mode when total <= 10"
    (is (= 11 (card/card-val {:value 0} :soft 0)))
    (is (= 11 (card/card-val {:value 0} :soft 10))))
  (testing "ace counts as 1 in soft mode when total > 10"
    (is (= 1 (card/card-val {:value 0} :soft 11))))
  (testing "ace always counts as 1 in hard mode"
    (is (= 1 (card/card-val {:value 0} :hard 0)))
    (is (= 1 (card/card-val {:value 0} :hard 10)))))

(deftest face-test
  (testing "regular faces"
    (is (= "A♠" (card/face :regular 0 0)))
    (is (= "K♦" (card/face :regular 12 3)))
    (is (= "??" (card/face :regular 13 0))))
  (testing "alternate faces"
    (is (= "🂡" (card/face :alternate 0 0)))
    (is (= "🂠" (card/face :alternate 13 0)))))
