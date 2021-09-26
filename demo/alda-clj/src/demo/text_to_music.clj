(ns demo.text-to-music
  (:require [alda.core          :refer :all]
            [clojure.java.io    :as    io]
            [clojure.java.shell :as    sh]
            [clojure.string     :as    str]))

(def DICTIONARY-WORDS
  (delay (line-seq (io/reader (io/file "/usr/share/dict/words")))))

(def INSTRUMENTS
  (delay (-> (sh/sh "alda" "instruments")
             :out
             str/split-lines)))

(defn random-instrument
  []
  (rand-nth @INSTRUMENTS))

(defn random-words
  [n]
  (take n (shuffle @DICTIONARY-WORDS)))

(defn random-word
  []
  (first (random-words 1)))

(def MIDI-NOTE-MIN 25)
(def MIDI-NOTE-MAX 100)

;; This results in numbers that are always in a sort of high range for letters,
;; which isn't very sonically interesting:
;; ["obsoleted" (89 98 85 89 92 99 84 99 100)]
;; ["earmuffs" (99 97 86 91 83 98 98 85)]
;; ["snowstorms" (85 90 89 81 85 84 89 86 91 85)]
(defn char->midi-note
  "Uses the code point of the provided character to determine a MIDI note number
   in the range MIDI-NOTE-MIN - MIDI-NOTE-MAX.

   If the code point is out of that range, we do some math to derive a number
   that is in the range."
  [character]
  (let [code-point (int character)
        range-size (- MIDI-NOTE-MAX MIDI-NOTE-MIN)]
    (cond
      (< code-point MIDI-NOTE-MIN)
      (let [distance        (- MIDI-NOTE-MIN code-point)
            distance-modulo (rem distance range-size)]
        (+ MIDI-NOTE-MIN distance-modulo))

      (> code-point MIDI-NOTE-MAX)
      (let [distance        (- code-point MIDI-NOTE-MAX)
            distance-modulo (rem distance range-size)]
        (- MIDI-NOTE-MAX distance-modulo))

      :else
      code-point)))

(defn char->midi-note-in-range
  "Uses the code point of the provided character to determine a MIDI note number
   in the provided range.

   If the code point is out of that range, we do some math to derive a number
   that is in the range."
  [character midi-note-min midi-note-max]
  (let [code-point (int character)
        range-size (- midi-note-min midi-note-max)]
    (cond
      (< code-point midi-note-min)
      (let [distance        (- midi-note-min code-point)
            distance-modulo (rem distance range-size)]
        (+ midi-note-min distance-modulo))

      (> code-point midi-note-max)
      (let [distance        (- code-point midi-note-max)
            distance-modulo (rem distance range-size)]
        (- midi-note-max distance-modulo))

      :else
      code-point)))

(defn octave-min-max->midi-note-min-max
  "Given a lower and upper octave (which can be the same octave, for a 1-octave
   span), returns the MIDI note numbers for the bottom note in the lower octave
   and the top note in the upper octave."
  [octave-min octave-max]
  {:pre [(<= octave-min octave-max)]}
  [(+ 12 (* 12 octave-min))
   (+ 23 (* 12 octave-max))])

(defn random-min-max-octave
  []
  (let [octave-min (inc (rand-int 6))
        octave-max (-> octave-min
                       (+ (rand-int 3))
                       (min 6))]
    [octave-min octave-max]))

(defn random-note-range
  []
  (apply
    octave-min-max->midi-note-min-max
    (random-min-max-octave)))

(defn character->note-length
  [character]
  (cond
    (#{\y \Y} character)
    6

    (#{\a \e \i \o \u \A \E \I \O \U} character)
    8

    :else
    4))

(defn character->note
  [character midi-note-min midi-note-max]
  (note (midi-note (char->midi-note-in-range
                     character
                     midi-note-min
                     midi-note-max))
        (note-length (character->note-length character))))

(def whitespace?
  #{\space \tab \newline})

(defn generate-score
  [words repetitions]
  (let [score-info
        {:tempo (+ 60 (rand-int 200))
         :parts (for [word words]
                  {:instrument (random-instrument)
                   :note-range (random-note-range)
                   :word       word})}]
    (doseq [{:keys [instrument word]} (:parts score-info)]
      (prn [instrument word]))
    [(tempo! (:tempo score-info))
     (for [{:keys [instrument note-range word]} (:parts score-info)
           :let [[midi-note-min midi-note-max] note-range]]
       [(part instrument)
        (repeat
          repetitions
          [(for [character word]
             (if (whitespace? character)
               (pause (note-length 4 {:dots 1}))
               (character->note
                 character
                 midi-note-min
                 midi-note-max)))
           (pause)])])]))

(def me-up-at-does
  (slurp (io/file (io/resource "e.e. cummings - (Me up at does).txt"))))

(comment
  (random-instrument)
  (random-word)
  (random-words 3)

  (char->midi-note-in-range \a 60 80)
  (map #(char->midi-note-in-range % 60 80) "hello")

  (connect!)
  (stop!)
  (new-score!)

  ;; 3 short, repeating parts
  ;; Generated from 3 random words from the dictionary
  (do
    (new-score!)
    (play! (generate-score
             (repeatedly 3 random-word)
             8)))

  ;; Same thing, but with non-random words
  (do
    (new-score!)
    (play! (generate-score
             ["Strange Loop"
              "Alda workshop"
              "2021"]
             4)))

  ;; My name as a motif
  (do
    (new-score!)
    (play! (generate-score
             ["dave yarwood"]
             4)))

  ;; e.e. cummings - (Me up at does)
  (println me-up-at-does)

  ;; ...converted into music
  (do
    (new-score!)
    (play! (generate-score
             [me-up-at-does]
             1))))
