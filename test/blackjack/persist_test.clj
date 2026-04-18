(ns blackjack.persist-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [blackjack.persist :as persist]
            [blackjack.game :as game]))

(def ^:dynamic *tmp-file* nil)

(use-fixtures :each
  (fn [t]
    (let [f (java.io.File/createTempFile "bj-test" ".txt")]
      (try
        (binding [*tmp-file* (.getAbsolutePath f)]
          (.delete f)
          (t))
        (finally (.delete f))))))

(deftest parse-test
  (testing "valid string"
    (is (= {:num-decks 4 :deck-type :aces :face-type :alternate
            :money 12345 :current-bet 500}
           (persist/parse "4|aces|alternate|12345|500"))))

  (testing "all deck-types parse"
    (doseq [[s k] [["regular" :regular] ["aces" :aces] ["jacks" :jacks]
                   ["aces-jacks" :aces-jacks] ["sevens" :sevens] ["eights" :eights]]]
      (is (= k (:deck-type (persist/parse (str "1|" s "|regular|10000|500")))))))

  (testing "unknown deck-type defaults to :regular"
    (is (= :regular (:deck-type (persist/parse "1|garbage|regular|10000|500")))))

  (testing "malformed inputs"
    (is (nil? (persist/parse nil)))
    (is (nil? (persist/parse "")))
    (is (nil? (persist/parse "1|aces")))
    (is (nil? (persist/parse "x|aces|regular|10000|500")))))

(deftest serialize-test
  (is (= "1|regular|regular|10000|500"
         (persist/serialize (game/new-game))))
  (is (= "8|alternate-no|alternate|999|500"
         (persist/serialize {:num-decks 8 :deck-type :alternate-no
                             :face-type :alternate :money 999 :current-bet 500}))))

(deftest roundtrip-test
  (let [g {:num-decks 6 :deck-type :sevens :face-type :alternate
           :money 50000 :current-bet 1000}]
    (is (= g (persist/parse (persist/serialize g))))))

(deftest load-state-missing-file-returns-nil-test
  (is (nil? (persist/load-state *tmp-file*))))

(deftest save-then-load-test
  (let [g (assoc (game/new-game) :money 7500 :current-bet 250 :num-decks 4
                 :deck-type :jacks :face-type :alternate)]
    (persist/save-state *tmp-file* g)
    (is (= {:num-decks 4 :deck-type :jacks :face-type :alternate
            :money 7500 :current-bet 250}
           (persist/load-state *tmp-file*)))))

(deftest save-creates-parent-directories-test
  (let [path (str *tmp-file* "/sub/dir/blackjack.txt")
        g (game/new-game)]
    (persist/save-state path g)
    (is (.exists (io/file path)))
    (.delete (io/file path))))

(deftest load-state-bad-path-returns-nil-test
  (testing "unreadable path returns nil instead of throwing"
    (let [dir (java.io.File/createTempFile "bj-dir" "")]
      (.delete dir)
      (.mkdir dir)
      (try
        (is (nil? (persist/load-state (.getAbsolutePath dir))))
        (finally (.delete dir))))))

(deftest save-state-bad-path-swallows-error-test
  (testing "writing to an invalid path returns nil rather than throwing"
    (is (nil? (persist/save-state "\u0000bad-path" (game/new-game))))))

(deftest merge-loaded-test
  (testing "nil loaded returns base game unchanged"
    (let [g (game/new-game)]
      (is (= g (persist/merge-loaded g nil)))))

  (testing "loaded state overrides fields and normalizes"
    (let [g (game/new-game)
          loaded {:num-decks 3 :deck-type :regular :face-type :alternate
                  :money 800 :current-bet 5000}
          merged (persist/merge-loaded g loaded)]
      (is (= 3 (:num-decks merged)))
      (is (= :alternate (:face-type merged)))
      (is (= 800 (:current-bet merged)))))

  (testing "aces deck forces num-decks >= 2"
    (let [g (game/new-game)
          loaded {:num-decks 1 :deck-type :aces :face-type :regular
                  :money 10000 :current-bet 500}
          merged (persist/merge-loaded g loaded)]
      (is (= 2 (:num-decks merged))))))
