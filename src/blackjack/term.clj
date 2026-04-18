(ns blackjack.term
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- run-process
  [^ProcessBuilder pb]
  (let [proc (.start pb)
        out (slurp (.getInputStream proc))]
    (.waitFor proc)
    (str/trim out)))

(defn- stty
  [& args]
  (let [cmd (vec (cons "stty" args))
        pb (doto (ProcessBuilder. ^java.util.List cmd)
             (.redirectInput (java.io.File. "/dev/tty")))]
    (run-process pb)))

(defn enable-unbuffered-mode!
  []
  (let [saved (stty "-g")]
    (stty "-icanon" "min" "1" "-echo")
    saved))

(defn restore!
  [saved]
  (when saved
    (stty saved)))

(def ^:dynamic *saved* nil)

(defn with-buffered-mode
  [f]
  (if-let [s *saved*]
    (do (stty s)
        (try (f) (finally (stty "-icanon" "min" "1" "-echo"))))
    (f)))

(defn install-shutdown-hook!
  [saved]
  (let [thread (Thread. ^Runnable #(restore! saved))]
    (.addShutdownHook (Runtime/getRuntime) thread)
    thread))

(defn tty?
  []
  (.exists (io/file "/dev/tty")))
