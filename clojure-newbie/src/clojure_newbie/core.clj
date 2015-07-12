(ns clojure-newbie.core
  (:gen-class)
  (:refer-clojure))

(defn -main
  "I din't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot."))

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

;; Don't know which one is better. They have pretilly the same algorithm but I believe using loop and recur is more idiomatic.
(defn other-ordinals-seq
  "Expect a number and return the sequence of ordinals from one below or equal to the parameter."
  ([cap]
   (other-ordinals-seq [] cap))
  ([seq cap]
    (if (= cap 1)
      seq
      (other-ordinals-seq
       (into seq (#(clojure.pprint/cl-format nil "~:R" %)
                  (cap))
             (- cap 1))))))

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


(defn radial-symmetry-body-parts
  "Expects a seq of maps that have a :name and :size and return a full body."
  ([asym-body-parts]
   (radial-symmetry-body-parts asym-body-parts 5))
  ([asym-body-parts occurence]
   (reduce (fn [final-body-parts part]
             (into final-body-parts (matching-parts part occurence)))
           []
           asym-body-parts)))
