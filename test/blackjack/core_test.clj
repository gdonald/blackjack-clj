(ns blackjack.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [blackjack.core :as core]
            [blackjack.term :as term]
            [blackjack.ui :as ui]))

(def ^:dynamic *tmp-file* nil)

(use-fixtures :each
  (fn [t]
    (let [f (java.io.File/createTempFile "bj-core-test" ".txt")]
      (try
        (binding [*tmp-file* (.getAbsolutePath f)]
          (.delete f)
          (t))
        (finally (.delete f))))))

(defn card [v s] {:id 0 :value v :suit s})
(def two   (card 1 0))
(def five  (card 4 0))
(def six   (card 5 0))
(def seven (card 6 0))
(def nine  (card 8 0))
(def king  (card 12 0))

(defn ph [cards & {:keys [bet status played stood paid]
                   :or {bet 500 status :unknown played false stood false paid false}}]
  {:id 1 :cards cards :bet bet :status status
   :played played :stood stood :paid paid})

(defn dh [cards & {:keys [hide-down-card played]
                   :or {hide-down-card true played false}}]
  {:cards cards :hide-down-card hide-down-card :played played})

(defn make-game [overrides]
  (merge {:id 0 :shoe [] :player-hands [] :dealer-hand nil
          :money 10000 :current-bet 500 :current-menu :game
          :current-player-hand 0 :num-decks 1 :deck-type :regular
          :face-type :regular :quitting false}
         overrides))

(deftest dispatch-game-menu-test
  (testing "deal-double key triggers a new deal"
    (let [g (make-game {:shoe (vec (concat [five seven nine king]
                                           (repeat 30 two)))})
          g' (core/dispatch-game-menu g "d")]
      (is (= 1 (count (:player-hands g'))))
      (is (= 2 (count (get-in g' [:player-hands 0 :cards]))))))
  (testing "bet-back enters :bet menu"
    (is (= :bet (:current-menu (core/dispatch-game-menu (make-game {}) "b")))))
  (testing "options enters :options menu"
    (is (= :options (:current-menu (core/dispatch-game-menu (make-game {}) "o")))))
  (testing "quit sets :quitting"
    (is (true? (:quitting (core/dispatch-game-menu (make-game {}) "q")))))
  (testing "unknown key is no-op"
    (let [g (make-game {})]
      (is (= g (core/dispatch-game-menu g "x"))))))

(deftest dispatch-hand-menu-test
  (let [base (make-game {:current-menu :hand
                         :money 10000
                         :shoe (vec (repeat 5 two))
                         :player-hands [(ph [five six])]
                         :dealer-hand (dh [seven king])})]
    (testing "h hits when can-hit?"
      (let [g' (core/dispatch-hand-menu base "h")]
        (is (= 3 (count (get-in g' [:player-hands 0 :cards]))))))
    (testing "s stands when can-stand?"
      (let [g' (core/dispatch-hand-menu base "s")]
        (is (true? (get-in g' [:player-hands 0 :stood])))))
    (testing "d doubles when can-double?"
      (let [g' (core/dispatch-hand-menu base "d")]
        (is (= 1000 (get-in g' [:player-hands 0 :bet])))))
    (testing "p splits when can-split?"
      (let [g (make-game {:current-menu :hand
                          :money 10000
                          :shoe (vec (repeat 5 two))
                          :player-hands [(ph [seven seven])]
                          :dealer-hand (dh [seven king])})
            g' (core/dispatch-hand-menu g "p")]
        (is (= 2 (count (:player-hands g'))))))
    (testing "unknown key is no-op"
      (is (= base (core/dispatch-hand-menu base "z"))))
    (testing "action key is ignored if action is not allowed"
      (is (= base (core/dispatch-hand-menu base "p"))))))

(deftest dispatch-insurance-menu-test
  (let [base (make-game {:current-menu :insurance
                         :money 10000
                         :player-hands [(ph [king nine] :bet 500)]
                         :dealer-hand (dh [(card 0 0) king])})]
    (testing "y takes insurance"
      (let [g' (core/dispatch-insurance-menu base "y")]
        (is (= 10000 (:money g')))
        (is (= :game (:current-menu g')))))
    (testing "n declines insurance"
      (let [g' (core/dispatch-insurance-menu base "n")]
        (is (= 9500 (:money g')))
        (is (= :game (:current-menu g')))))
    (testing "unknown key is no-op"
      (is (= base (core/dispatch-insurance-menu base "x"))))))

(deftest dispatch-options-menu-test
  (let [base (make-game {:current-menu :options})]
    (is (= :num-decks (:current-menu (core/dispatch-options-menu base "n"))))
    (is (= :deck-type (:current-menu (core/dispatch-options-menu base "t"))))
    (is (= :face-type (:current-menu (core/dispatch-options-menu base "f"))))
    (is (= :game (:current-menu (core/dispatch-options-menu base "b"))))
    (is (= base (core/dispatch-options-menu base "x")))))

(deftest dispatch-deck-type-menu-test
  (let [base (make-game {:current-menu :deck-type})]
    (is (= :regular    (:deck-type (core/dispatch-deck-type-menu base "1"))))
    (is (= :aces       (:deck-type (core/dispatch-deck-type-menu base "2"))))
    (is (= :jacks      (:deck-type (core/dispatch-deck-type-menu base "3"))))
    (is (= :aces-jacks (:deck-type (core/dispatch-deck-type-menu base "4"))))
    (is (= :sevens     (:deck-type (core/dispatch-deck-type-menu base "5"))))
    (is (= :eights     (:deck-type (core/dispatch-deck-type-menu base "6"))))
    (is (= base (core/dispatch-deck-type-menu base "x")))))

(deftest dispatch-face-type-menu-test
  (let [base (make-game {:current-menu :face-type})]
    (is (= :regular   (:face-type (core/dispatch-face-type-menu base "r"))))
    (is (= :alternate (:face-type (core/dispatch-face-type-menu base "a"))))
    (is (= base (core/dispatch-face-type-menu base "x")))))

(defn- with-input [s f]
  (binding [*in* (-> s java.io.StringReader. clojure.lang.LineNumberingPushbackReader.)]
    (f)))

(deftest step-reads-key-and-applies-dispatch-test
  (testing ":game menu — pressing 'q' quits"
    (with-redefs [ui/draw (fn [_] nil)]
      (with-input "q"
        (fn []
          (let [g (make-game {})
                g' (core/step g *tmp-file*)]
            (is (true? (:quitting g')))))))))

(deftest step-bet-menu-reads-line-and-deals-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "10\n"
      (fn []
        (let [g (make-game {:current-menu :bet
                            :shoe (vec (concat [five seven nine king]
                                               (repeat 30 two)))})
              g' (core/step g *tmp-file*)]
          (is (= 1000 (:current-bet g')))
          (is (seq (:player-hands g'))))))))

(deftest step-num-decks-menu-reads-line-and-deals-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "4\n"
      (fn []
        (let [g (make-game {:current-menu :num-decks})
              g' (core/step g *tmp-file*)]
          (is (= 4 (:num-decks g')))
          (is (seq (:shoe g'))))))))

(deftest step-bet-menu-bad-input-returns-to-game-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "abc\n"
      (fn []
        (let [g (make-game {:current-menu :bet
                            :shoe (vec (concat [five seven nine king]
                                               (repeat 30 two)))})
              g' (core/step g *tmp-file*)]
          (is (= 500 (:current-bet g'))))))))

(deftest step-num-decks-menu-bad-input-returns-to-game-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "xyz\n"
      (fn []
        (let [g (make-game {:current-menu :num-decks})
              g' (core/step g *tmp-file*)]
          (is (= 1 (:num-decks g'))))))))

(deftest step-hand-menu-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "s"
      (fn []
        (let [g (make-game {:current-menu :hand
                            :money 10000
                            :player-hands [(ph [king nine] :bet 500)]
                            :dealer-hand (dh [seven king])})
              g' (core/step g *tmp-file*)]
          (is (true? (get-in g' [:player-hands 0 :stood]))))))))

(deftest step-insurance-menu-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "n"
      (fn []
        (let [g (make-game {:current-menu :insurance
                            :money 10000
                            :player-hands [(ph [king nine] :bet 500)]
                            :dealer-hand (dh [(card 0 0) nine])})
              g' (core/step g *tmp-file*)]
          (is (= :hand (:current-menu g'))))))))

(deftest step-options-menu-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "t"
      (fn []
        (let [g (make-game {:current-menu :options})
              g' (core/step g *tmp-file*)]
          (is (= :deck-type (:current-menu g'))))))))

(deftest step-deck-type-menu-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "2"
      (fn []
        (let [g (make-game {:current-menu :deck-type})
              g' (core/step g *tmp-file*)]
          (is (= :aces (:deck-type g'))))))))

(deftest step-face-type-menu-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "a"
      (fn []
        (let [g (make-game {:current-menu :face-type})
              g' (core/step g *tmp-file*)]
          (is (= :alternate (:face-type g'))))))))

(deftest step-unknown-menu-falls-through-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input "x"
      (fn []
        (let [g (make-game {:current-menu :wat})
              g' (core/step g *tmp-file*)]
          (is (= g g')))))))

(deftest step-eof-quits-test
  (with-redefs [ui/draw (fn [_] nil)]
    (with-input ""
      (fn []
        (let [g (make-game {})
              g' (core/step g *tmp-file*)]
          (is (true? (:quitting g'))))))))

(deftest -main-invokes-run-test
  (let [called (atom false)]
    (with-redefs [core/run (fn [_] (reset! called true) {})]
      (core/-main)
      (is (true? @called)))))

(deftest run-quits-immediately-on-q-test
  (testing "TTY branch: stty is invoked, then loop runs"
    (let [restored (atom nil)]
      (with-redefs [ui/draw (fn [_] nil)
                    term/tty? (constantly true)
                    term/enable-unbuffered-mode! (constantly "saved-settings")
                    term/install-shutdown-hook! (fn [_] nil)
                    term/restore! (fn [s] (reset! restored s))]
        (with-input "q"
          (fn []
            (let [g (core/run *tmp-file*)]
              (is (true? (:quitting g)))
              (is (= "saved-settings" @restored))))))))
  (testing "non-TTY branch"
    (with-redefs [ui/draw (fn [_] nil)
                  term/tty? (constantly false)]
      (with-input "q"
        (fn []
          (let [g (core/run *tmp-file*)]
            (is (true? (:quitting g)))))))))
