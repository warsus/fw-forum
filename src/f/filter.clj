(ns f.filter
  (:use [f.db :only [qspamtext d qbeitrag qindexraw qbeitragvon qhamtext]])
  (:require [incanter.stats :as stats]
            [datomic.api :as d]))

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

(defn words [text] (re-seq #" \w+ " (.toLowerCase text)))

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
  (or (contains? (:spam db) feature)
      (contains? (:ham db) feature)))

(defn classify [text]
  "Returns a vector of the form [classification score]"
  (let [spam-db @spam-db]
    (->> text
         words
         (filter #(feature-known spam-db %))
         (score spam-db))))

(defn init-spam-db! []
  (train! (map first (qspamtext (d))) :spam)
  (train! (map first (qhamtext (d))) :ham))

(defn classify-id []
  (let [db (d)] (map #(classify (:beitrag/text %)) (qindexraw db))))

(defn classify-entities [es]
    (let [db (d)] (map #(classify (:beitrag/text %)) es)))

(defn qspam [db]
  (q {:find '[?id]
      :where '[[?id :beitrag/spam true]
               [?id :beitrag/id ?bid]]} db))

(defn qham [db]
  (q {:find '[?id]
      :where '[[?id :beitrag/spam false]
               [?id :beitrag/id ?bid]]} db))

(defn words [text] (re-seq #"[a-z]+" (.toLowerCase text)))

(defn wc [text] (count (words text)))

(defn word-frequencies [text]
  (if (string? text) (frequencies (words text))
      (reduce #(merge-with + %1 %2) (map word-frequencies text))))

(defn qfeatures [db word]
  (q {:find '[(sum ?wc)]
      :in '[$ ?word]
      :where '[[_ :beitrag/text ?text]
               [(f.db/wc ?text) ?wc]]} db word))

(defn qspamtext [db]
  (q {:find '[?text]
      :where '[[?id :beitrag/spam true]
               [?id :beitrag/id ?bid]
               [?id :beitrag/text ?text]]} db))

(defn qhamtext [db]
  (q {:find '[?text]
      :where '[[?id :beitrag/spam false]
               [?id :beitrag/id ?bid]
               [?id :beitrag/text ?text]]} db))


(defn dump-text [user]
  (map (comp #(spit user % :append true) pr-str beitrag->hash) (map #(nth % 2) (qbeitragvon (d) user))))

(defn dump-post-query [user posts]
  (map (comp #(spit user (str % "\n") :append true) pr-str beitrag->hash) (map #(nth % 2) posts)))


(defn qduplicates [db]
  (q {:find '[?bid1 ?bid2]
      :where '[[?bid1 :beitrag/text ?text1]
               [?bid2 :beitrag/text ?text2]
               [(= ?text1 ?text2)]]} db))

;; (defn mark-ham! [db bids]
;;   (dorun
;;    @(d/transact conn (map
;;                       (fn [bid] {:db/id (d/tempid :db.part/user)
;;                                 :beitrag/id 1
;;                                 :beitrag/spam false})
;;                       bids))))

