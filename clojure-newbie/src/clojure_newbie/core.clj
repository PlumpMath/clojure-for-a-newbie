(ns clojure-newbie.core
  (:gen-class)
  (:refer-clojure)
  (require [clojure.set :as set]))
(def filename "suspects.csv")

(defn -main
  "I din't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot."))

;; Chapter III
;; First we define the test data structure.
(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

;; Then we define some helpers. As for philosophy KISS, their meaning is kinda atomic and somewhat straightforward.
(defn matching-ordinal
  "Replace a name containg \"left-\" but it's ith counterpart."
  [ith]
  #(clojure.string/replace % #"^left-" (str ith "-")))

(defn ordinals-seq
  "Expect a number and return the sequence of ordinals from one below or equal to the parameter."
  [cap]
  (loop [current 0
         ordinals []]
    (if (< current cap)
      (recur (inc current)
             (conj ordinals (#(clojure.pprint/cl-format nil "~:R" %) (inc current))))
      ordinals)))

;; Chapter III exercice 1
(str (vector 1 2 3)
     (list 1 2 3
           (hash-map :a 1 :b 2)
           (hash-set 1 1 2)))

;; Chapter III exercice 2
(map #(+ 100 %) [0 50])

;; Chapter III exercice 3
(map ((fn [shift] #(- % shift)) 6) [0 3 6])

;; Chapter III exercice 4
((fn [func array] (set (map func array))) inc [1 2])

;; Chapter III exercice 5
(defn matching-parts
  ([part]
   (matching-parts part 5))
  ([part occurence]
   (reduce (fn [final-set ; new set of parts related to the old part
               function] ; replace an ordinal
             (conj final-set
                   {:name (function (:name part))
                    :size (:size part)}))
           #{}
           (map matching-ordinal (ordinals-seq occurence)))))

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size and return a full body."
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (matching-parts part 5)))
          []
          asym-body-parts))

;; Chapter III exercice 6
(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size and return a full body."
  ([asym-body-parts]
   (symmetrize-body-parts asym-body-parts 5))
  ([asym-body-parts occurence]
   (reduce (fn [final-body-parts part]
             (into final-body-parts (matching-parts part occurence)))
           []
           asym-body-parts)))

;; Chapter IV
(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})
(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

;; Helper
(glitter-filter 3 (mapify (parse (slurp filename))))

;; Chapter IV exercice 1
(defn glitter-filter
  [minimum-glitter records]
  (map :name (filter #(>= (:glitter-index %) minimum-glitter) records)))

;; Chapter IV exercice 2
(defn mapper
  [unmapped-row]
  (reduce (fn [row-map [vamp-key value]]
            (assoc row-map vamp-key (convert vamp-key value)))
          {}
          (map vector vamp-keys unmapped-row))
  )

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map mapper rows))


(defn append
  "Append a new suspect to the list"
  [suspect]
  (spit filename
        (#(str (:name %) "," (:glitter-index %)) suspect)
        :append true))

;; chapter IV exercice 3
(defn validate
  [functions record]
  (reduce #(and %1 (%2 record)) true functions))

;; Helper
(def validation-functions
  [#(contains? % :name )
   #(contains? % :glitter-index)])

(defn append
  "Append a new suspect to the list"
  [suspect]
  (if (validate validation-functions suspect)
    (do (spit filename
              (#(str (:name %) "," (:glitter-index %)) suspect)
              :append true)
        "Record appended")
    "Parameter suspect form is invalid"))

;; Chapter IV exercice 4
(defn serialize
  [suspect]
  (#(str (:name %) "," (:glitter-index %)) suspect))

(defn append
  "Append a new suspect to the list"
  [suspect]
  (if (validate validation-functions suspect)
    (do (spit filename
              (serialize suspect)
              :append true)
        "Record appended")
    "Parameter suspect form is invalid"))

(defn back-to-csv
  [map-of-maps]
  (reduce #(str %1 (serialize %2) "\n") "" map-of-maps))

;; Helper
(def map-of-maps
  [{:name "a" :glitter-index 3}
   {:name "b" :glitter-index 4}])

;; Chapter V
(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))
(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))
(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-moveu board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))
