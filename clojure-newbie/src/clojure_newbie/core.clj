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

;; dummy way ><
(defn matching-part
  [part]
  (set
   [{:name (clojure.string/replace (:name part) #"^left-" "first-")
     :size (:size part)}
    {:name (clojure.string/replace (:name part) #"^left-" "second-")
     :size (:size part)}
    {:name (clojure.string/replace (:name part) #"^left-" "third-")
     :size (:size part)}
    {:name (clojure.string/replace (:name part) #"^left-" "fourth-")
     :size (:size part)}
    {:name (clojure.string/replace (:name part) #"^left-" "fifth-")
     :size (:size part)}]))

(defn radial-symmetry-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))
