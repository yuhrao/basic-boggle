(ns boggle.core
  (:require [portal.api :as p]))

;; Matrix utils
(defn build-visited-matrix
  "Build a matrix with same size of the board fulfilled with `false` values.
  Visited positions should be marked with `true`"
  [board]
  (let [row-count (count board)
        column-count (count (first board))
        column (->> (range column-count)
                    (map (constantly false))
                    (into []))]
    (->> (range row-count)
         (map (constantly column))
         (into []))))

(defn mark-as-visited [matrix [x y]]
  (update matrix x (fn [c]
                     (update c y (fn [_] true)))))

;; Char position map
(defn row->position-map [row-number chars]
  (let [char+idx-seq (->> chars
                          (interleave (range (count chars)))
                          (partition 2))]
    (reduce (fn [position-map [idx char]]
              (let [position [row-number idx]]
                (if (contains? position-map char)
                  (update position-map char conj position)
                  (assoc position-map char [position]))))
            {}
            char+idx-seq)))

(defn merge-position-maps [target m]
  (reduce (fn [*target [k v]]
            (if (contains? target k)
              (update *target k (comp vec concat) v)
              (assoc *target k v)))
          target m))

(defn build-char-position-map [board]
  (let [row+idx-seq (->> board
                         (interleave (range (count board)))
                         (partition 2))]
    (reduce (fn [position-map [idx row]]
              (let [row-position-map (row->position-map idx row)]
                (merge-position-maps position-map
                                     row-position-map)))
            {}
            row+idx-seq)))
(defn as-pos-int [v]
  (if (< v 0)
    (* -1 v)
    v))

(defn filter-neighbours [posit-map curr-char next-char]
  (let [combinations (for [curr-char-posits (get posit-map curr-char)
                           next-char-posits (get posit-map next-char)]
                       [curr-char-posits next-char-posits])]
    (filter (fn [[[x1 y1] [x2 y2]]]
              (let [dx (as-pos-int (- x2 x1))
                    dy (as-pos-int (- y2 y1))]
                (and (<= dx 1)
                     (<= dy 1)))) combinations)))

(defn filter-visited [visited-matrix match-cases]
  (filter
    (fn [[[x1 y1] [x2 y2]]]
      (let [first-visited? (-> visited-matrix
                               (get x1)
                               (get y1)
                               not)
            second-visited? (-> visited-matrix
                                (get x2)
                                (get y2)
                                not)]
        (and first-visited? second-visited?)))
    match-cases))

(defn word-exists? [board word]
  (let [posit-map (build-char-position-map board)]
    (if (every? (fn [char] (contains? posit-map (str char))) word)
      (loop [itx-number 0
             letters (map str (seq word))
             visited-matrix (build-visited-matrix board)]
        (let [current-case (->> (filter-neighbours posit-map (first letters) (second letters))
                                (filter-visited visited-matrix)
                                first)]
          (cond
            (<= (count letters) 1) true
            (nil? current-case) false
            :else (recur
                    (inc itx-number)
                    (drop 1 letters)
                    (-> visited-matrix
                        (mark-as-visited (first current-case))
                        (mark-as-visited (second current-case)))))
          ))
      false)))

(defn execute [{:keys [input output]
                :as   case}]
  (let [{:keys [board word]} input
        result (word-exists? board word)
        op-result (merge case
                         {:result   result
                          :board    board
                          :word     word
                          :success? (= result output)})]
    op-result))

(defn load-samples []
  (-> (slurp "samples.edn")
      (clojure.edn/read-string)))

(defn run-sample [sample-number]
  (let [sample (nth (load-samples) sample-number)]
    (execute sample)))

(defn run-all-samples []
  (->> (load-samples)
       (map execute)))

(defn -main [run-type & _]
  (let [result (cond
                 (= run-type "all") (run-all-samples)
                 (Character/isDigit (char run-type)) (run-sample (.parseInt run-type)))]
    (p/open)
    (add-tap #'p/submit)
    (tap> result)))

(comment

  (run-sample 1)

  (run-all-samples)

  )
