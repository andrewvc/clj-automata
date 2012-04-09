(ns qlearn.core
  (:require [quil.core :as qc])
  (:use clojure.pprint)
  (:import java.lang.Math))

(defn int->bdigits
  "Gets the binary digits that comprise an integer as a seq of ints"
  [number]
  (doall (for [c (Integer/toBinaryString number)]
           (Integer/valueOf (str c)))))

(defn zero-pad
  "Forwad pads a seq with 0s to match a given length"
  [xs len]
  (let [shortage (- len (count xs))]
  (if (< shortage 1) xs
      (concat (repeat shortage 0) xs))))

;; Load the pattern sequence of 
(def input-patterns
     (map #(zero-pad (int->bdigits %1) 3) (range 8)))

(defn rule-mappings
  "Creates a rule function."
  [number]
  (zipmap input-patterns
          (reverse (zero-pad (int->bdigits number) 8))))

(defn rule
  "Returns a function that will process a triad of input values
   according to rule #"
  [number]
  (let [mappings (rule-mappings number)]
    (fn [triad] (get mappings triad))))

(defn simulate
  "Runs a single iteration of a given rule-fn on a given-state"
  [rule-fn state]
  (let [rule (rule-mappings 110)]
    (for [triad (partition 3 1 (concat [0] state [0]))]
      (rule-fn triad))))

(defn simulation
  "Returns a lazy-seq of future states for a given rule-fn and state"
  [rule-fn state]
  (let [new-state (simulate rule-fn state)]
    (cons new-state (lazy-seq (simulation rule-fn new-state)))))

(def live-color [242 233 99])
(def dead-color [64 37 27])

(defn draw-buffer
  [buffer scale]
  (letfn [(draw-row[y row]
           (dorun (map-indexed (fn [x col] (draw-cell x y col)) row)))
          (draw-cell[x y col]
           (apply qc/fill (if (= 1 col) live-color dead-color))
           (qc/rect (* scale x) (* scale y) scale scale))]
    (doall (map-indexed draw-row buffer))))

(defn setup-for
  [buffer scale]
  (fn setup-fn []
    (qc/smooth)
    (qc/frame-rate 24)
    (qc/background 255)
    (draw-buffer buffer scale)))

(defn run-rule [rule-num {:keys [width height scale]}]
  (let [width (or width 100)
        height (or height 100)
        scale (or scale 5)
        initial (repeatedly height #(rand-int 2))
        sim (simulation (rule rule-num) initial)
        time-slices (atom (partition height 1 sim))]
    (qc/defsketch automata
      :title (str "Rule " rule-num)
      :setup (setup-for (first @time-slices) scale)
      :draw (fn drawfn []
              (let [ts (rest @time-slices)]
                (swap! time-slices (fn [_] ts))
                (draw-buffer (first ts) scale)))
      :size [(* scale width) (* scale height)])))

(defn -main [rule-num & args]
  (run-rule (Integer/valueOf rule-num) {:width 100 :height 100 :scale 4}))