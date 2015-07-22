(ns clojure-newbie.core
  (:gen-class)
  (:refer-clojure))
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

;; Chapter V exercice 1
(defn attr
  [field]
  (comp field :attributes))

;; Chapter V exercice 2
(defn my-comp
  "Implement built-in comp"
  [& functions]
  (fn [operands]
    (reduce #(%2 %)
            operands
            (reverse functions))))

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

;; Chapter V exercice 3
(defn my-assoc-in
  ;; Is it idiomatic?
  [m [k & ks] v]
  (loop [m m
         k k
         remaining ks]
    (if (= 1 (count ks))
      (assoc m ks v)
      (let [[k1 & ks1] ks]
        (recur (assoc m k nil)
               k1
               ks1)))))

(defn my-assoc-in2
  ;; Is it idiomatic to have implicit recurrence?
  [m [k & ks] v]
  (assoc (if (= 1 (count ks))
           m
           (my-assoc-in2 m ks v))
    ks v))

(defn mai3
  "Implement built-in assoc-in"
  [m [k & ks] v]
  (str m)
  (let [v1 (if (contains? m k)
             (mai3 (get m k) ks v)
             v)]
    (assoc m k v1)))

(defn mai4
  ""
  [map keys value]
  (loop [[key & keys] (reverse keys)
         final map
         initial (assoc {} key value)]
    (if (not (empty? keys))
      (let [keys (reverse keys)]
        (println "key:" key)
        (println "rem:" keys)
        (recur keys
               final
               (assoc {} key initial)))
      (assoc final key initial))))
