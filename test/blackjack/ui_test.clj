(ns blackjack.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [blackjack.ui :as ui]))

(defn card [v s] {:id 0 :value v :suit s})
(def ace   (card 0 0))
(def five  (card 4 0))
(def nine  (card 8 0))
(def king  (card 12 0))

(deftest dealer-hand-str-hides-down-card-test
  (let [game {:face-type :regular
              :dealer-hand {:cards [ace king] :hide-down-card true}}
        s (ui/dealer-hand-str game)]
    (is (str/includes? s "??"))
    (is (str/includes? s "K♠"))
    (is (str/includes? s "10"))))

(deftest dealer-hand-str-revealed-test
  (let [game {:face-type :regular
              :dealer-hand {:cards [ace king] :hide-down-card false}}
        s (ui/dealer-hand-str game)]
    (is (not (str/includes? s "??")))
    (is (str/includes? s "A♠"))
    (is (str/includes? s "21"))))

(deftest player-hand-str-shows-arrow-on-current-test
  (let [game {:face-type :regular :current-player-hand 0}
        ph {:cards [five nine] :bet 500 :status :unknown
            :played false :stood false :paid false}
        s (ui/player-hand-str game ph 0)]
    (is (str/includes? s "⇐"))
    (is (str/includes? s "$5.00"))))

(deftest player-hand-str-status-test
  (let [game {:face-type :regular :current-player-hand 0}]
    (is (str/includes?
         (ui/player-hand-str game
                             {:cards [ace king] :bet 500 :status :won
                              :played true :stood false :paid true}
                             0)
         "Blackjack!"))
    (is (str/includes?
         (ui/player-hand-str game
                             {:cards [king king] :bet 500 :status :won
                              :played true :stood false :paid true}
                             0)
         "Won!"))
    (is (str/includes?
         (ui/player-hand-str game
                             {:cards [king king five] :bet 500 :status :lost
                              :played true :stood false :paid true}
                             0)
         "Busted!"))
    (is (str/includes?
         (ui/player-hand-str game
                             {:cards [king five] :bet 500 :status :lost
                              :played true :stood false :paid true}
                             0)
         "Lost!"))
    (is (str/includes?
         (ui/player-hand-str game
                             {:cards [king nine] :bet 500 :status :push
                              :played true :stood false :paid true}
                             0)
         "Push"))))

(deftest header-game-menu-test
  (let [s (ui/header-str {:money 10000 :current-menu :game})]
    (is (str/includes? s "(d) deal new hand"))
    (is (str/includes? s "(b) change bet"))
    (is (str/includes? s "(o) options"))
    (is (str/includes? s "(q) quit"))))

(deftest header-hand-menu-shows-only-available-actions-test
  (let [g {:money 10000 :current-menu :hand
           :current-player-hand 0
           :player-hands [{:cards [five nine] :bet 500 :status :unknown
                           :played false :stood false :paid false}]}
        s (ui/header-str g)]
    (is (str/includes? s "(h) hit"))
    (is (str/includes? s "(s) stand"))
    (is (str/includes? s "(d) double"))
    (is (not (str/includes? s "split")))))

(deftest header-insurance-menu-test
  (let [s (ui/header-str {:money 10000 :current-menu :insurance})]
    (is (str/includes? s "(y) insure hand"))
    (is (str/includes? s "(n) refuse insurance"))))

(deftest header-options-menu-test
  (let [s (ui/header-str {:money 10000 :current-menu :options})]
    (is (str/includes? s "number of decks"))
    (is (str/includes? s "deck type"))
    (is (str/includes? s "face type"))))

(deftest header-deck-type-menu-test
  (let [s (ui/header-str {:money 10000 :current-menu :deck-type})]
    (is (str/includes? s "regular"))
    (is (str/includes? s "aces"))
    (is (str/includes? s "jacks"))
    (is (str/includes? s "sevens"))
    (is (str/includes? s "eights"))))

(deftest header-face-type-menu-test
  (let [s (ui/header-str {:money 10000 :current-menu :face-type})]
    (is (str/includes? s "(r) A♠ regular"))
    (is (str/includes? s "(a) 🂡 alternate"))))

(defn- with-input [s f]
  (binding [*in* (-> s java.io.StringReader.
                     clojure.lang.LineNumberingPushbackReader.)]
    (f)))

(deftest read-key-test
  (testing "lower-cased single char"
    (with-input "S" (fn [] (is (= "s" (ui/read-key))))))
  (testing "EOF returns nil"
    (with-input "" (fn [] (is (nil? (ui/read-key)))))))

(defn- swallow-out [f]
  (binding [*out* (java.io.StringWriter.)] (f)))

(deftest read-line-int-test
  (testing "reads and parses an integer"
    (with-input "42\n"
      (fn []
        (is (= 42 (swallow-out #(ui/read-line-int "Bet: ")))))))
  (testing "returns nil for non-numeric input"
    (with-input "abc\n"
      (fn []
        (is (nil? (swallow-out #(ui/read-line-int "Bet: ")))))))
  (testing "returns nil on EOF (nil from read-line)"
    (with-input ""
      (fn []
        (is (nil? (swallow-out #(ui/read-line-int "Bet: "))))))))

(deftest clear-screen-emits-escape-codes-test
  (let [out (with-out-str (ui/clear-screen))]
    (is (.contains out "\033[H\033[2J"))))

(deftest draw-renders-and-clears-test
  (let [game {:money 10000 :current-menu :game :face-type :regular
              :current-player-hand 0
              :dealer-hand {:cards [ace king] :hide-down-card true}
              :player-hands [{:cards [five nine] :bet 500 :status :unknown
                              :played false :stood false :paid false}]}
        out (with-out-str (ui/draw game))]
    (is (.contains out "Dealer:"))
    (is (.contains out "\033[2J"))))

(deftest player-hand-status-default-branch-test
  (testing "unknown status keyword returns empty string"
    (let [game {:face-type :regular :current-player-hand 0}
          ph {:cards [five nine] :bet 500 :status :weird-thing
              :played false :stood false :paid false}]
      (is (string? (ui/player-hand-str game ph 0))))))

(deftest header-hand-menu-with-no-actions-test
  (testing "blackjack hand: none of hit/stand/split/double available"
    (let [g {:money 10000 :current-menu :hand :current-player-hand 0
             :player-hands [{:cards [{:value 0 :suit 0} {:value 12 :suit 0}]
                             :bet 500 :status :unknown
                             :played false :stood false :paid false}]}
          s (ui/header-str g)]
      (is (not (str/includes? s "(h) hit")))
      (is (not (str/includes? s "(s) stand")))
      (is (not (str/includes? s "(p) split")))
      (is (not (str/includes? s "(d) double"))))))

(deftest menu-str-unknown-menu-returns-nil-test
  (is (nil? (ui/menu-str {:current-menu :wat}))))

(deftest header-hand-menu-with-split-test
  (testing "all four actions shown when player has a low pair"
    (let [g {:money 10000 :current-menu :hand :current-player-hand 0
             :player-hands [{:cards [{:value 7 :suit 0} {:value 7 :suit 1}]
                             :bet 500 :status :unknown
                             :played false :stood false :paid false}]}
          s (ui/header-str g)]
      (is (str/includes? s "(h) hit"))
      (is (str/includes? s "(s) stand"))
      (is (str/includes? s "(p) split"))
      (is (str/includes? s "(d) double")))))

(deftest render-test
  (let [game {:money 10000 :current-menu :game :face-type :regular
              :current-player-hand 0
              :dealer-hand {:cards [ace king] :hide-down-card true}
              :player-hands [{:cards [five nine] :bet 500 :status :unknown
                              :played false :stood false :paid false}]}
        s (ui/render game)]
    (is (str/includes? s "Dealer:"))
    (is (str/includes? s "Player $100.00:"))))

(deftest render-bet-menu-omits-header-test
  (let [game {:money 10000 :current-menu :bet :face-type :regular
              :current-player-hand 0
              :dealer-hand {:cards [ace king] :hide-down-card true}
              :player-hands [{:cards [five nine] :bet 500 :status :unknown
                              :played false :stood false :paid false}]}
        s (ui/render game)]
    (is (not (str/includes? s "New bet")))
    (is (not (str/includes? s "Bet amount")))
    (is (str/ends-with? s "\n\n"))))

(deftest render-num-decks-menu-omits-header-test
  (let [game {:money 10000 :current-menu :num-decks :face-type :regular
              :current-player-hand 0
              :dealer-hand {:cards [ace king] :hide-down-card true}
              :player-hands [{:cards [five nine] :bet 500 :status :unknown
                              :played false :stood false :paid false}]}
        s (ui/render game)]
    (is (not (str/includes? s "Number of decks")))
    (is (str/ends-with? s "\n\n"))))

(deftest draw-prompt-menu-does-not-add-trailing-newline-test
  (let [game {:money 10000 :current-menu :bet :face-type :regular
              :current-player-hand 0
              :dealer-hand {:cards [ace king] :hide-down-card true}
              :player-hands [{:cards [five nine] :bet 500 :status :unknown
                              :played false :stood false :paid false}]}
        out (with-out-str (ui/draw game))]
    (is (.contains out "\033[2J"))
    (is (str/ends-with? out "\n\n"))))
