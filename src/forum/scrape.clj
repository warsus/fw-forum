(ns forum.scrape

  (:import java.text.SimpleDateFormat)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.java.io :as io])
  (:use [datomic.api :only [q tempid db transact ] :as d] forum.db
        [clojure.java.io :as io]
        [forum.filter :only [init-spam-db! classify]]
        [clojure.edn :as edn]))


(def scrape-dir (io/file "scrape"))
(def *base-url* "http://forum.freiwirtschaft.org/forum.php?seite=")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

;; (defn fetch-beitrag [id]
;;   (cons id (map html/text (html/select  #{[:h2] [:.basisinformation :.gast] [:.beitragstext_1] [:.vorgaenger :li :a]}))))

(defn beitrag-ids->entity-ids [db bids]
  (if (empty? bids) #{} (q {:find '[?id]
                            :in '[$ [?bid]]
                            :where '[[?id :beitrag/id ?bid]]} db bids)))

(defn tempids []
  (drop 1 (iterate (fn [x] (tempid :db.part/user)) nil)))

(def formatter (SimpleDateFormat. "yyyy-MM-dd hh:mm:ss"))

(defn fetch-beitrag [res]
  (let [select #(html/select res %)
        id1 (tempid :db.part/user)]
    (cons {:beitrag/text (-> (select [:.beitragstext_1]) first html/text)
           :beitrag/user  (-> (select [:.basisinformation #{:.gast :.stammposter}]) first html/text)
           :beitrag/titel   (-> (select [:h2]) first html/text )
           :beitrag/id (read-string (re-find #"\d+" (-> (select [:h2 :a]) first :attrs :name )))
           :beitrag/datum (.parse formatter (re-find #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}" (html/text (first (select [:div.basisinformation])))))
           :db/id id1
           :beitrag/antworten (map first
                                   (beitrag-ids->entity-ids
                                    (d) (map #(read-string (re-find #"\d+" (get-in % [:attrs :href]))) (select [:.antworten :li :> :a]))))}

          (map (fn [id tid]
                 {:beitrag/id id
                  :db/id tid
                  :beitrag/antworten [id1]})
               (map #(read-string (re-find #"\d+" (get-in % [:attrs :href]))) (select [:.vorgaenger :li :> :a]))
               (tempids)))))

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

(defn init! []
  (load-schema)
  (scrape! [2 1]))

(init!)
;; (defn hn-points []
;;   (map html/text (html/select (fetch-url *base-url*) [:td.subtext html/first-child])))

;; (defn print-headlines-and-points []
;;   (doseq [line (map #(str %1 " (" %2 ")") (hn-headlines) (hn-points))]
;;     (println line)))

(defn scrape-text []
  (for [i (range 2)]
    (.write (.getContent (java.net.URL. (str "http://forum.freiwirtschaft.org/forum.php?seite=" i))) (java.io.FileOutputStream. (str i)))))

(defn init-forum-db! []
  (load-schema)
  (init-drive! (map #(.getName %) (.listFiles scrape-dir)))
  (mark-forum-ham!)
  (mark-forum-spam!))

(defn init-forum-test-db! []
  (load-schema)
  (init-drive! (take 1000 (map #(.getName %) (.listFiles scrape-dir))))
  (mark-forum-ham!)
  (mark-forum-spam!)
  (init-spam-db!))

(defn directory->file []
  (let [os (io/writer "edn/forum.edn" :append true)
        files (.listFiles scrape-dir)
        posts (map
               (fn [a] (try
                        (with-open [input (io/input-stream a)]
                          (first (fetch-beitrag (html/html-resource a))))
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
                (.write output (str (first (fetch-beitrag (html/html-resource input)))  "\n")))
              (catch NullPointerException e1 nil))
            (recur (inc i)))))))

(defn compute-ham [db]
  (q {:find '[?bid]
      :where
      '[[?id :beitrag/id ?bid]
        [?id :beitrag/text ?text]
        [(forum.filter/classify ?text) ?score]
        [(> ?score 0.4)]]} db))

(defn compute-spam [db]
  (q {:find '[?bid]
      :where
      '[[?id :beitrag/id ?bid]
        [?id :beitrag/text ?text]
        [(forum.filter/classify ?text) ?score]
        [(< ?score 0.4)]]} db))

(defn read-edn-file []
  (read (java.io.PushbackReader. (io/reader "edn/forum.edn"))))

(defn read-edn-file2 []
  (read (slurp "edn/forum.edn")))
