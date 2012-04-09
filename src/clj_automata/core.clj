(ns clj-automata.core
  "This module visualizes elementary cellular automata. It's primarily intended
   to show off the fun aspects of functional programming in clojure to those who are coming from an OO background.

   If you're interested in how elementary cellular automata work, see:
   http://en.wikipedia.org/wiki/Elementary_cellular_automaton

   Also, I highly recommend reading this file bottom to top

   In particular, the code here makes heavy use of lazy-sequences, both directly through
   `lazy-seq` and also through functions like map, for, partition, and other functions that
   in clojure return lazy sequences. In some cases, we force lazy-evaluation for side-effects with
   (dorun).

   Since the visualization is of a stream of state for the automata, these features are particularly
   relevant here. At a high level this library revolves around a single lazy seq that represents
   all future states.

   The rendering model here is quite simple, we use pure clojure to fill a buffer of data that contains
   nested 2d lazy sequences representing all currently visible cells on the screen. As we iterate through
   these lazy sequences we re-render those cells on the canvas. Since it's all lazy, we actually don't have
   all the pixels referenced in memory at any given time, only those that are being actively worked with.

   The core functions behind the laziness are (simulation), which returns a lazy sequence that perpetually
   calls (simulate) which looks at the previous row's state and determines what the next row will be.

   There are other functional aspects here at play. The (rule) function for instance, is a higher-order function,
   it's only goal is to return an anonymous function that can determine a given cell's state given its three
   determining cells from the previous state. The elementary automata algorithm can be easily implemented based on
   pattern matching."
  (:gen-class)
  (:require [quil.core :as qc])
  (:use clojure.pprint)
  (:import java.lang.Math))

;; Colors for each cell
(def live-color [242 233 99])
(def dead-color [64 37 27])

(defn int->bdigits
  "Gets the binary digits that comprise an integer as a seq of ints"
  [number]
  (for [c (Integer/toBinaryString number)] (Integer/valueOf (str c))))

(defn zero-pad
  "Forward pads a seq with 0s to match a given length. Used for making sure int->bdigits hits byte boundaries"
  [x len]
  (let [shortage (- len (count x))]
    (if (< shortage 1) x
        (concat (repeat shortage 0) x))))

(def input-patterns
     "The list of possible input sequences for elementary cellular automata, which are easily done by counting down from 8 in binary, and making sure we have at least three digits. This should produce a list like: ((111 110 ...))"
     (map #(zero-pad (int->bdigits %1) 3) (range 8)))

(defn rule-mappings
  "Returns a mapping of patterns to new states. Returns a structure like:
   {(0 1 1) 1
    ...}"
  [number]
  ;; Zipmap combines two sequences into a map, much like a zipper!
  ;; The key here is that the magic rule numbers are not numbers at all
  ;; but a sequence rather (their individual binary digits) that get mapped
  ;; onto the list of possible inputs described in input-patterns.
  ;; The heart of the generic solution here is really just checking
  ;; equality of a sequence, which we can do via a lookup in the hashmap
  ;; this generates
  (zipmap input-patterns
          (reverse (zero-pad (int->bdigits number) 8))))

(defn rule
  "Returns a function that will process a triad of input values according to a given rule #. Since rules are simple lookup tables, this maps to nothing more than a get really. We use a function here only to be able to close over the rule-mappings and only evaluate those once."
  [number]
  (let [mappings (rule-mappings number)]
    (fn [triad] (get mappings triad))))

(defn bookend
  "Pads a seq with a given value on both sides.
   We use this to make calculating the edge values easier."
  [x v]
  (concat [v] x [v]))

(defn simulate
  "Runs a single iteration of a given rule-fn on a given-state"
  [rule-fn state]
  (let [rule (rule-mappings 110)]
    ;; We bookend the value below to add a 0 on both sides of the previous state
    ;; as it makes calculations simpler
    (for [triad (partition 3 1 (bookend state 0))]
      (rule-fn triad))))

(defn simulation
  "Returns a lazy-seq of future states for a given rule-fn and state"
  [rule-fn state]
  (let [new-state (simulate rule-fn state)]
    (cons new-state (lazy-seq (simulation rule-fn new-state)))))

(defn draw-buffer
  "Redraw what's on screen given a buffer of cell data at a given scale"
  [buffer scale]
  ;; We use letfn here because we want both of these functions to
  ;; have access to the variables `buffer` and `scale`
  ;; We use two nested `map-index` calls to iterated over the canvas
  (letfn [(draw-row [y row]
           (dorun (map-indexed (fn [x col] (draw-cell x y col)) row)))
          (draw-cell [x y col]
           (apply qc/fill (if (= 1 col) live-color dead-color))
           (qc/rect (* scale x) (* scale y) scale scale))]
    (dorun (map-indexed draw-row buffer))))

(defn setup
  "Setup the UI"
  []
  (qc/smooth) ;; Enable AA
  (qc/frame-rate 24))

(defn run-rule [rule-num {:keys [width height scale]}]
  (let [width (or width 100)
        height (or height 100)
        scale (or scale 5)
        initial (repeatedly height #(rand-int 2))
        sim (simulation (rule rule-num) initial)
        time-slices (atom (partition height 1 sim))]
    
    (println "Rule " rule-num " mappings:")
    (pprint (rule-mappings rule-num))
    (qc/defsketch automata
      :title (str "Rule " rule-num)
      :setup setup
      :draw (fn drawfn [] ;; Named anonymous functions are easier to stacktrace
              (draw-buffer (first @time-slices) scale)
              (swap! time-slices (fn [_] (rest @time-slices))))
      :size [(* scale width) (* scale height)])))

(defn -main [rule-num & args]
  (run-rule (Integer/valueOf rule-num)
            {:width 100 :height 100 :scale 4}))