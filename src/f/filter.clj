(ns f.filter
  (:use [f.db :only [qspamtext qhamtext d]])
  (:require [incanter.stats :as stats]))

(def max-ham-score 0.4)
(def min-spam-score 0.6)

(def spam-db (ref {:spam {:count 0}
                   :ham {:count 0}}))

(defn clear-db! []
  (dosync
   (ref-set spam-db {:spam {:count 0}
                     :ham {:count 0}})))

(defn classify-score [score]
  (cond
   (<= score max-ham-score) :ham
   (>= score min-spam-score) :spam
   :else :unsure))

(defn words [text] (re-seq #"[a-z]+" (.toLowerCase text)))

;;TODO :count should be equal to all ffrequencies
(defn stats [text type]
  (let [freqs (frequencies (words text))
        freqs (assoc freqs :count (reduce + (vals freqs)))]
    freqs))

(defn train! [texts type]
  (dosync
   (alter spam-db (fn [db texts]
                    (update-in db [type]
                               (fn [db] (reduce
                                        #(merge-with + %1 %2)
                                        db
                                        (map #(stats % type) texts))))) texts)))

(defn spam-probability [db feature]
  (let [s (/ (get (:spam db) feature 0) (max 1 (:count (:spam db))))
        h (/ (get (:ham db) feature 0) (max 1 (:count (:ham db))))]
      (/ s (+ s h))))

(defn bayesian-spam-probability
  "Calculates probability a feature is spam on a prior. assumed-
probability is the prior assumed for each feature, and weight is
weight to be given to the prior (i.e. the number of data points to
count it as). Defaults to 1/2 and 1."
  [db feature & {:keys [assumed-probability weight] :or
              {assumed-probability 1/2 weight 1}}]
  (let [basic-prob (spam-probability db feature)
        total-count (+ (get (:spam db) feature 0) (get (:ham db) feature 0))]
    (/ (+ (* weight assumed-probability)
          (* total-count basic-prob))
       (+ weight total-count))))

(defn fisher
  "Fisher computation described by Robinson."
  [probs num]
  (- 1 (stats/cdf-chisq
        (* -2 (reduce + (map #(Math/log %1) probs)))
        :df (* 2 num))))


;;TODO filter if in db crap
(defn score [db features]
  (let [spam-probs (map #(bayesian-spam-probability db %) features)
        ham-probs (map #(- 1 %1) spam-probs)
        num (count features)
        h (- 1 (fisher spam-probs num))
        s (- 1 (fisher ham-probs num))]
    (/ (+ (- 1 h) s) 2)))

(defn feature-known [db feature]
  (contains? (:spam db) feature))

(defn classify [text]
  "Returns a vector of the form [classification score]"
  (let [spam-db @spam-db]
    (->> text
         words
         (filter #(feature-known spam-db %))
         (score spam-db))))



(defn init-spam-db! []
  (train! (take 1000 (map first (qspamtext (d)))) :spam)
  (train! (take 1000 (map first (qhamtext (d)))) :ham))

(defn classify-id [id]
  (let [db (d)] (map #(classify (nth (first (qbeitrag db %)) 4)) (ffirst (qindex db)))))


