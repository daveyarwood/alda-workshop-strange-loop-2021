(ns demo.basics
  (:require [alda.core :refer :all]))

(comment
  ;; Shells out to the alda CLI
  (alda "version")
  (alda "ps")

  ;; Connect/disconnect from your Alda REPL
  (connect!)
  (disconnect!)

  ;; Forget about current tempo, octave, instruments, etc.
  ;; Start from a clean slate.
  (new-score!)

  ;; Stop playback
  (stop!)

  ;; Equivalent to running "alda play -c 'piano: o4 c1 / e- / g'"
  (play!
    (part "piano")
    (octave 4)
    (chord (note (pitch :c) (note-length 1))
           (note (pitch :e :flat))
           (note (pitch :g))))

  ;; Working with sequences of musical events (notes, rests, etc.)
  (play!
    (part "piano")
    (set-note-length 16)
    (interpose (pause)
               (for [letter [:c :d :e :f :g]]
                 (note (pitch letter)))))

  ;; Generate a sequence of random notes
  (play!
    (part "piano")
    ;; (quant 2000)
    (for [[length note-number]
          (repeatedly 8 #(list
                           (+ 250 (rand-int 750))
                           (+ 25 (rand-int 70))))]
      (note (midi-note note-number)
            (duration (ms length))))))

















;;; algorithmic composition

(comment
  (do
    (new-score!)
    (stop!))

  (def REST-RATE 0.15)
  (def MS-LOWER 30)
  (def MS-UPPER 3000)
  (def MAX-OCTAVE 8)

  (defn random-note
    "Returns a random note in a random octave with a random duration in
     milliseconds.

     May randomly return a rest with a random duration in milliseconds, instead."
    []
    (let [ms (ms (rand-nth (range MS-LOWER MS-UPPER)))]
      (if (< (rand) REST-RATE)
        (pause (duration ms))
        (let [o (rand-int (inc MAX-OCTAVE))
              n [(keyword (str (rand-nth "abcdefg")))
                 (rand-nth [:sharp :flat :natural])]]
          [(octave o)
           (note (apply pitch n) (duration ms))]))))

  (play!
    (part "midi-electric-piano-1")
    (panning 25)
    (repeatedly 20 random-note)

    (part "midi-timpani")
    (panning 50)
    (repeatedly 20 random-note)

    (part "midi-celesta")
    (panning 75)
    (repeatedly 20 random-note)))
