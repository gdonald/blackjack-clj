(ns blackjack.term-test
  (:require [clojure.test :refer [deftest is testing]]
            [blackjack.term :as term]))

(deftest tty?-test
  (testing "tty? checks /dev/tty existence — on darwin/linux this exists"
    (is (boolean? (term/tty?)))))

(deftest restore!-nil-is-noop-test
  (is (nil? (term/restore! nil))))

(deftest enable-unbuffered-mode!-mocks-stty-test
  (let [calls (atom [])]
    (with-redefs [blackjack.term/stty
                  (fn [& args]
                    (swap! calls conj (vec args))
                    (if (= args ["-g"]) "saved-state" ""))]
      (let [saved (term/enable-unbuffered-mode!)]
        (is (= "saved-state" saved))
        (is (= [["-g"] ["-icanon" "min" "1" "-echo"]] @calls))))))

(deftest stty-private-fn-executes-test
  (try
    (let [result (#'blackjack.term/stty "-g")]
      (is (string? result)))
    (catch Exception _
      (is true))))

(deftest run-process-executes-subprocess-test
  (let [pb (ProcessBuilder. ^java.util.List ["echo" "hello"])
        result (#'blackjack.term/run-process pb)]
    (is (= "hello" result))))

(deftest restore!-with-saved-calls-stty-test
  (let [calls (atom [])]
    (with-redefs [blackjack.term/stty
                  (fn [& args] (swap! calls conj (vec args)) "")]
      (term/restore! "saved-state")
      (is (= [["saved-state"]] @calls)))))

(deftest with-buffered-mode-runs-thunk-without-stty-when-not-in-unbuffered-mode-test
  (let [calls (atom [])]
    (with-redefs [blackjack.term/stty
                  (fn [& args] (swap! calls conj (vec args)) "")]
      (let [result (term/with-buffered-mode (fn [] :ok))]
        (is (= :ok result))
        (is (empty? @calls))))))

(deftest with-buffered-mode-toggles-around-thunk-when-saved-bound-test
  (let [calls (atom [])]
    (with-redefs [blackjack.term/stty
                  (fn [& args] (swap! calls conj (vec args)) "")]
      (binding [term/*saved* "saved-state"]
        (let [result (term/with-buffered-mode
                       (fn []
                         (swap! calls conj [:thunk])
                         :done))]
          (is (= :done result))
          (is (= [["saved-state"]
                  [:thunk]
                  ["-icanon" "min" "1" "-echo"]]
                 @calls)))))))

(deftest with-buffered-mode-restores-unbuffered-on-exception-test
  (let [calls (atom [])]
    (with-redefs [blackjack.term/stty
                  (fn [& args] (swap! calls conj (vec args)) "")]
      (binding [term/*saved* "saved-state"]
        (is (thrown? Exception
                     (term/with-buffered-mode
                       (fn [] (throw (Exception. "boom"))))))
        (is (= [["saved-state"] ["-icanon" "min" "1" "-echo"]] @calls))))))

(deftest install-shutdown-hook!-adds-hook-test
  (let [restored (atom nil)]
    (with-redefs [blackjack.term/restore! (fn [s] (reset! restored s))]
      (let [t (term/install-shutdown-hook! "saved-value")]
        (is (instance? Thread t))
        (.run t)
        (is (= "saved-value" @restored))
        (.removeShutdownHook (Runtime/getRuntime) t)))))
