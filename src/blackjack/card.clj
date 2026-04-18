(ns blackjack.card)

(def faces-regular
  [["A♠" "A♥" "A♣" "A♦"]
   ["2♠" "2♥" "2♣" "2♦"]
   ["3♠" "3♥" "3♣" "3♦"]
   ["4♠" "4♥" "4♣" "4♦"]
   ["5♠" "5♥" "5♣" "5♦"]
   ["6♠" "6♥" "6♣" "6♦"]
   ["7♠" "7♥" "7♣" "7♦"]
   ["8♠" "8♥" "8♣" "8♦"]
   ["9♠" "9♥" "9♣" "9♦"]
   ["T♠" "T♥" "T♣" "T♦"]
   ["J♠" "J♥" "J♣" "J♦"]
   ["Q♠" "Q♥" "Q♣" "Q♦"]
   ["K♠" "K♥" "K♣" "K♦"]
   ["??"]])

(def faces-alternate
  [["🂡" "🂱" "🃁" "🃑"]
   ["🂢" "🂲" "🃂" "🃒"]
   ["🂣" "🂳" "🃃" "🃓"]
   ["🂤" "🂴" "🃄" "🃔"]
   ["🂥" "🂵" "🃅" "🃕"]
   ["🂦" "🂶" "🃆" "🃖"]
   ["🂧" "🂷" "🃇" "🃗"]
   ["🂨" "🂸" "🃈" "🃘"]
   ["🂩" "🂹" "🃉" "🃙"]
   ["🂪" "🂺" "🃊" "🃚"]
   ["🂫" "🂻" "🃋" "🃛"]
   ["🂭" "🂽" "🃍" "🃝"]
   ["🂮" "🂾" "🃎" "🃞"]
   ["🂠"]])

(def down-card [13 0])

(defn make-card [id value suit]
  {:id id :value value :suit suit})

(defn ace? [{:keys [value]}]
  (= 0 value))

(defn ten? [{:keys [value]}]
  (> value 8))

(defn card-val
  [{:keys [value]} count-method total]
  (let [v (inc value)
        v (if (> v 9) 10 v)]
    (if (and (= count-method :soft) (= v 1) (< total 11))
      11
      v)))

(defn face
  [face-type value suit]
  (let [faces (if (= face-type :alternate) faces-alternate faces-regular)]
    (get-in faces [value suit])))
