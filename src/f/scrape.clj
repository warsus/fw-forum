(ns f.scrape
  (:import java.text.SimpleDateFormat)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:use [datomic.api :only [q tempid db transact ] :as d]
        f.db
        f.frequency
        [clojure.java.io :as io]
        [f.filter :only [init-spam-db! classify]]))

(def scrape-dir (io/file "scrape"))
(def *base-url* "http://forum.freiwirtschaft.org/forum.php?seite=")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn beitrag-ids->entity-ids [db bids]
  (if (empty? bids) #{} (q {:find '[?id]
                            :in '[$ [?bid]]
                            :where '[[?id :beitrag/id ?bid]]} db bids)))

(def formatter (SimpleDateFormat. "yyyy-MM-dd hh:mm:ss"))

(defn fetch-beitrag [res]
  (let [res (html/html-resource res)
        select #(html/select res %)]
    {:beitrag/text (apply str (html/emit* (-> (select [:.beitragstext_1]) first)))
     :beitrag/user  (-> (select [:.basisinformation #{:.gast :.stammposter}]) first html/text)
     :beitrag/titel   (-> (select [:h2]) first html/text )
     :beitrag/id (read-string (re-find #"\d+" (-> (select [:h2 :a]) first :attrs :name )))
     :beitrag/datum (.parse formatter (re-find #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}" (html/text (first (select [:div.basisinformation])))))
     :beitrag/antworten  (set (map #(read-string (re-find #"\d+" (get-in % [:attrs :href]))) (select [:td.antworten :> :ul :> :li :> :a])))}))

(defn fb [bid]
  (fetch-beitrag (io/file (str "scrape/" bid))))

(defn scrape! [range]
  (do (map
       (fn [a]
         (try
           ((comp #(d/transact conn %) fetch-beitrag #(java.net.URL. (str *base-url* %))) a)
           (catch NullPointerException e1 (print a)))) range)))

(defn init-drive!
  ([range]
     (dorun (map
             (fn [a] (try ((comp #(d/transact conn %) fetch-beitrag #(java.io.File. (str "scrape/" %))) a)
                         (catch NullPointerException e1 (print a))))
             range)))
  ([] (init-drive! (map #(.getName %) (.listFiles scrape-dir)))))

(defn scrape-text []
  (for [i (range 2)]
    (.write (.getContent (java.net.URL. (str "http://forum.freiwirtschaft.org/forum.php?seite=" i))) (java.io.FileOutputStream. (str i)))))

(defn mark-spam! [db bids]
  @(d/transact conn (map
                     (fn [bid] {:db/id (d/tempid :db.part/user)
                               :beitrag/id bid
                               :beitrag/spam true})
                     bids)))

(defn mark-ham! [db bids]
  @(d/transact conn  (take 200 (drop 400 (map
                                   (fn [bid] {:db/id (d/tempid :db.part/user)
                                             :beitrag/id bid
                                             :beitrag/spam false})
                                   bids)))))

(defn mark-forum-ham! []
  (mark-ham! (d) (map first (q {:find '[?bid]
                                 :where '[[?b :beitrag/id ?bid] [(< ?bid 7844)]]} (d)))))

(defn mark-forum-spam! []
  (mark-spam! (d) (map first (q {:find '[?bid]
                                 :where '[[?b :beitrag/id ?bid] [(> ?bid 411223)]]} (d)))))

(defn init-forum-test-db! []
  (load-schema)
  (init-drive! (take 1000 (map #(.getName %) (.listFiles scrape-dir))))
  (mark-forum-ham!)
  (mark-forum-spam!)
  (init-spam-db!))

(defn directory->file []
  (let [os (io/writer "forum.edn")
        files (.listFiles scrape-dir)
        posts (map
               (fn [a] (try
                        (fetch-beitrag a)
                        (catch NullPointerException e1 nil)))
               files)]
    (dorun (map (fn [s]
                  (.write os (str s "\n"))
                  (.flush os)) posts))
    (.close os)))

(defn transform-files []
  (let [files  (.listFiles scrape-dir)]
    (loop [i 0]
      (when-let [a (nth files i)]
        (do (try
              (with-open [input (io/input-stream a)
                          output (io/writer (str "edn/"(.getName a)))]
                (.write output (str (fetch-beitrag input)  "\n")))
              (catch NullPointerException e1 nil))
            (recur (inc i)))))))

(defn is-spam [e]
  (or (not (contains? e :beitrag/text)) (> (f.filter/classify (:beitrag/text e)) 0.6)))

(defn word-probability [word]
  (apply * (map #(get adjacent-probabilities (apply str %) 1e-50) (partition 2 1 word))))

(defn is-spam-simple [e]
  (if (contains? e :beitrag/titel)
    (or (re-matches #"[0-9]{6,}" (:beitrag/titel e))
        (re-matches #"[a-z]+[A-Z]\w*" (:beitrag/titel e))
        (if-let [word (re-matches #"^\w*$" (:beitrag/titel e))]
          (> 1E-70 (word-probability word))))
    false))

(defn beitrag->edn [writer e]
  (let [m (select-keys e [:beitrag/id :beitrag/titel :beitrag/datum :beitrag/user :beitrag/text :beitrag/antworten])
        m (if (contains? m :beitrag/titel) (update-in m [:beitrag/titel] #(.trim %)) m)
        m (update-in m [:beitrag/antworten] #(map :beitrag/id %))]
    (.write writer (str m "\n"))
    (.flush writer)))



(defn find-mark-spam! [db]
  (let [writer (io/writer "spam.edn")
        maxbid (first (ffirst (q {:find '[(max 1 ?bid)]
                                  :where '[[_ :beitrag/id ?bid]]} db)))
        intervalls (partition 2 1 (take-while #(< % maxbid) (map #(* 1000 %) (range))))
        spam (map (fn [[start end]]
                    (as-> (d/index-range (d) :beitrag/id start end) range
                          (map #(->> % :e (d/entity db)) range)
                          (filter #(is-spam-simple %) range)
                          (dorun (map #(beitrag->edn writer %) range)))) intervalls)]
    (dorun spam)
    (.close writer)))

(defn filter-ham! [db]
  (let [writer (io/writer "ham.edn")
        maxbid (first (ffirst (q {:find '[(max 1 ?bid)]
                                  :where '[[_ :beitrag/id ?bid]]} db)))
        intervalls (partition 2 1 (take-while #(< % maxbid) (map #(* 1000 %) (range))))
        spam (map (fn [[start end]]
                    (as-> (d/index-range (d) :beitrag/id start end) range
                          (map #(->> % :e (d/entity db)) range)
                          (filter #(not (is-spam-simple %)) range)
                          (dorun (map #(beitrag->edn writer %) range)))) intervalls)]
    (dorun spam)
    (.close writer)
    true))

(defn map->transaction [e]
  (as-> (assoc e :db/id (d/tempid :db.part/user)) e
        (let [{as :beitrag/antworten} e
              tempids (take (count as) (tempids))]
          (cons (assoc e :beitrag/antworten tempids) (map (fn [a id] {:beitrag/id a :db/id id}) as tempids)))))

(def count-tx (ref 0))

(defn transact-edn-file [conn name]
  (dosync ref-set count-tx 0)
  (->> (map
        edn/read-string (filter (comp not empty?) (line-seq (io/reader name))))
       (partition 100)
       (map (fn [chunck]
              (dosync (alter count-tx inc))
              ((fn foo [x]
                 (if (> x 10) (throw (Exception. "Timeout")))
                 (try @(d/transact conn (mapcat map->transaction chunck))
                      (catch Exception e (do (print x) (Thread/sleep 5000) (foo (inc x)))))) 0)))))

(defn init-forum-db! []
  (dorun
   (do
     (d/delete-database uri)
     (d/create-database uri)
     (dosync (ref-set conn (d/connect uri)))
     (load-schema @conn)
     (transact-edn-file @conn "forum.edn")
     ;; (mark-forum-ham!)
     ))
  ;; (mark-forum-spam!)
  )

;;TODO: compute parents beforehando
(defn init-filtered-db! []
  (dorun
   (do
     (d/delete-database uri)
     (d/create-database uri)
     (dosync (ref-set conn (d/connect uri)))
     (load-schema @conn)
     (transact-edn-file @conn "ham.edn"))))

(defn letter-frequencies [] (frequencies (apply concat (mapcat (comp words first) (qhamtext (d))))))
(defn adjacent-frequency []
  (frequencies
   (for [qresults (qhamtext (d))
         word    (words (first qresults))
         pairs    (partition 2 1 word)]
     (apply str pairs))))

(defn probability [m]
  (let [total (apply + (map second m))
        _ (print total)]
    (into {} (map (fn [[char count]] [char (float (/ count total))]) m ))))

(def letter-probabilities
  {\a 0.060436323, \b 0.025006518, \c 0.0307059, \d 0.050851054, \e 0.15444924, \f 0.015449124, \g 0.033937804, \h 0.04526603, \i 0.08197057, \j 0.002315321, \k 0.014592584, \l 0.0394331, \m 0.026286412, \n 0.09559016, \o 0.027192589, \p 0.013790551, \q 2.7052648E-4, \r 0.070854835, \s 0.066152126, \t 0.06664402, \u 0.036464754, \v 0.009489809, \w 0.018737635, \x 0.001682247, \y 0.0016642056, \z 0.010766553})


