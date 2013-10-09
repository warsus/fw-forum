(ns f.scrape
  (:import java.text.SimpleDateFormat)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:use [datomic.api :only [q tempid db transact ] :as d]
        f.db
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

(defn tempids []
  (drop 1 (iterate (fn [x] (tempid :db.part/user)) nil)))

(def formatter (SimpleDateFormat. "yyyy-MM-dd hh:mm:ss"))

(defn fetch-beitrag [res]
  (let [select #(html/select res %)]
    {:beitrag/text (-> (select [:.beitragstext_1]) first html/text)
     :beitrag/user  (-> (select [:.basisinformation #{:.gast :.stammposter}]) first html/text)
     :beitrag/titel   (-> (select [:h2]) first html/text )
     :beitrag/id (read-string (re-find #"\d+" (-> (select [:h2 :a]) first :attrs :name )))
     :beitrag/datum (.parse formatter (re-find #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}" (html/text (first (select [:div.basisinformation])))))
     :beitrag/antworten (map #(read-string (re-find #"\d+" (get-in % [:attrs :href]))) (select [:.antworten :li :> :a]))
     }))

(defn scrape! [range]
  (do (map
       (fn [a]
         (try
           ((comp #(d/transact conn %) fetch-beitrag #(html/html-resource (java.net.URL. (str *base-url* %)))) a)
           (catch NullPointerException e1 (print a)))) range)))

(defn init-drive!
  ([range]
     (dorun (map
             (fn [a] (try ((comp #(d/transact conn %) fetch-beitrag #(html/html-resource (java.io.File. (str "scrape/" %)))) a)
                         (catch NullPointerException e1 (print a))))
             range)))
  ([] (init-drive! (map #(.getName %) (.listFiles scrape-dir)))))

(defn scrape-text []
  (for [i (range 2)]
    (.write (.getContent (java.net.URL. (str "http://forum.freiwirtschaft.org/forum.php?seite=" i))) (java.io.FileOutputStream. (str i)))))

(defn init-forum-test-db! []
  (load-schema)
  (init-drive! (take 1000 (map #(.getName %) (.listFiles scrape-dir))))
  (mark-forum-ham!)
  (mark-forum-spam!)
  (init-spam-db!))

(defn directory->file []
  (let [os (io/writer "edn/forum.edn")
        files (.listFiles scrape-dir)
        posts (map
               (fn [a] (try
                        (with-open [input (io/input-stream a)]
                          (fetch-beitrag (html/html-resource a)))
                        (catch NullPointerException e1 nil)))
               files)]
    (.write os "(")
    (dorun (map (fn [s] (.write os (str s "\n"))
                  (.flush os)) posts))
    (.write os ")")
    (.close os)))

(defn transform-files []
  (let [files  (.listFiles scrape-dir)]
    (loop [i 0]
      (when-let [a (nth files i)]
        (do (try
              (with-open [input (io/input-stream a)
                          output (io/writer (str "edn/"(.getName a)))]
                (.write output (str (fetch-beitrag (html/html-resource input))  "\n")))
              (catch NullPointerException e1 nil))
            (recur (inc i)))))))

(defn compute-ham [db]
  (q {:find '[?bid]
      :where
      '[[?id :beitrag/id ?bid]
        [?id :beitrag/text ?text]
        [(f.filter/classify ?text) ?score]
        [(> ?score 0.4)]]} db))

(defn compute-spam [db]
  (q {:find '[?bid]
      :where
      '[[?id :beitrag/id ?bid]
        [?id :beitrag/text ?text]
        [(f.filter/classify ?text) ?score]
        [(< ?score 0.4)]]} db))

(defn transact-edn-file []
  (dorun  (->> (map edn/read-string (filter (comp not empty?) (line-seq (io/reader "edn/forum.edn"))))
               (map #(dissoc % :beitrag/antworten))
               (map #(assoc %2 :db/id %1) (tempids))
               (partition 100)
               (map #(d/transact conn %)))))

(defn transact-edn-links []
  (dorun (->> (map edn/read-string (filter (comp not empty?) (line-seq (io/reader "edn/forum.edn"))))
              (map #(select-keys % [:db/id :beitrag/id :beitrag/antworten]))
              (map #(assoc %2 :db/id %1) (tempids))
              (map (fn [b] (update-in b [:beitrag/antworten] #(filter (comp not nil?) (for [x %]
                                                                                       (ffirst (qbeitrag (d) x)))))))
              (partition 100)
              (map #(d/transact conn %)))))

;;TODO: compute parents beforehando
(defn init-forum-db! []
  (load-schema)
  (transact-edn-file)
  (transact-edn-links)
  ;; (mark-forum-ham!)
  ;; (mark-forum-spam!)
  )
