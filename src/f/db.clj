(ns f.db
  (:require [clojure.java.io :as io])
  (:use [datomic.api :only [q db] :as d])
  (:use clojure.repl)
  (:use clojure.pprint)
  (:use hiccup.core)
  (:require [clojure.set :as s]))

(def e d/entity)

(defn tempids []
  (drop 1 (iterate (fn [x] (d/tempid :db.part/user)) nil)))

(def  thread-local-utc-date-format
  ;; SimpleDateFormat is not thread-safe, so we use a ThreadLocal proxy for access.
  ;; http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4228335
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
        (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))))

(def production?
  (= "production" (get (System/getenv) "APP_ENV")))

(def development?
  (not production?))

(defn now []
  (java.util.Date.))

(def uri  "datomic:sql://forum?jdbc:postgresql://localhost:5432/datomic?user=datomic&password=datomic")


(d/create-database uri)

(def conn
  (ref (d/connect uri)))

(defn d []
  (db @conn))

(defn beitrag->hash [id]
  (d/touch (e (d) id)))

(defn load-schema [conn]
  (let [schema
        [{:db/id #db/id[:db.part/db]
          :db/ident :beitrag/text
          :db/valueType :db.type/string
          :db/fulltext true
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag text"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/datum
          :db/valueType :db.type/instant
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag text"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/titel
          :db/fulltext true
          :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/user
          :db/valueType :db.type/string
          :db/fulltext true
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/zeit
          :db/valueType :db.type/instant
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag Zeit"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/antworten
          :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many
          :db/index true
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/id
          :db/index true
          :db/unique :db.unique/identity
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "id"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/spam
          :db/index true
          :db/valueType :db.type/boolean
          :db/cardinality :db.cardinality/one
          :db/doc "id"
          :db.install/_attribute :db.part/db}]]
    @(d/transact conn schema)))

(defn qbeitrag
  ([db] (q {:find '[?b ?titel ?user ?id ?text]
            :where '[[?b beitrag/id ?id]
                     [?b beitrag/titel ?titel]
                     [?b beitrag/user ?user]
                     [?b beitrag/text ?text]]} db))
  ([db id] (q {:find '[?b ?titel ?user ?id ?text]
               :in   '[$ ?id]
               :where  '[[?b beitrag/id ?id]
                         [?b beitrag/titel ?titel]
                         [?b beitrag/user ?user]
                         [?b beitrag/text ?text]]} db id)))

(defn bids->id [db bids]
  (q {:find '[?id]
      :in '[$ [?bid]]
      :where '[[?id :beitrag ?bid]]} db bids))

(defn qbeitragbyid [db id]
  (let [e (d/entity db id)]
    [(get e :beitrag/id) (get e :beitrag/titel) (get e :beitrag/user)]))

(defn qbeitragvon ([db user]
                     (q {:find '[?user ?bid ?b]
                         :in '[$ ?user]
                         :where '[[?b beitrag/id ?bid]
                                  [?b beitrag/user ?user]]} db user)))

(defn qbeitragvonmatch ([db pattern]
                     (q {:find '[?user]
                         :in '[$ ?pattern]
                         :where '[[?b beitrag/id ?bid]
                                  [(fulltext $ :beitrag/user ?pattern) [[?b ?user]]
                                   ]]} db pattern)))

(defn qantworten
  ([db]
     (q {:find '[?a ?b]
         :where '[[?b :beitrag/antworten ?a]]} db))
  ([db bid]
     (q {:find '[?a ?bid]
         :in '[$ ?id]
         :where '[[?b :beitrag/id ?id]
                  [?a :beitrag/id ?bid]
                  [?b :beitrag/antworten ?a]]} db bid)))

(def parent-rule '[[[parent ?b1 ?b2]
                    [?b1 :beitrag/antworten ?b2]]
                   [[parent ?b1 ?b3]
                    [?b1 :beitrag/antworten ?b2]
                    [parent ?b2 ?b3]]])

(defn qparents [e]
  (reverse (take-while #(not (nil? %)) (rest (iterate #(first (:beitrag/_antworten %)) e)))))

(defn within [intervall datum]
  (.contains intervall (.getTime datum)))

(defn qwithin [db date1 date2]
  (let [interval (org.joda.time.Interval. date1 date2)]
    (q {:find '[?bid]
        :in  '[$ ?interval]
        :where '[[?id :beitrag/id ?bid]
                 [?id :beitrag/datum ?datum]
                 [(f.db/within ?interval ?datum)]]} db interval)))

(defn qindexraw
  ([db]
     (map #(d/entity db (:e %)) (reverse (take-last 50 (d/datoms db :avet :beitrag/id)))))
  ([db page] (map #(d/entity db (:e %)) (reverse (take 50 (take-last (* page 50) (d/datoms db :avet :beitrag/id)))))))

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

(defn qe [db [[s p o :as c] & clauses]]
  (q {:find [s]
      :where (cons c clauses)} db))

(defn dt [e]
  (d/touch (d/entity (d) e)))

(defn dtid [id]
  (dt (ffirst (qbeitrag (d) id))))

(defn qbeitragvonmit [db user pattern]
  (q {:find '[?id ?bid]
      :where '[[?id :beitrag/id ?bid]
               [?id :beitrag/user ?user]
             [(fulltext $ :beitrag/text ?pattern) [[?id]]]]
      :in '[$ ?user ?pattern]} db user pattern))

(defn qbeitragtext [db pattern]
  (q {:find '[?id]
      :where '[[?id :beitrag/id ?bid]
                 [(fulltext $ :beitrag/text ?pattern) [[?id]]]]
      :in '[$ ?pattern]} db pattern))

(defn qusers [db]
  (map first (q {:find '[?user]
                 :where '[[?id :beitrag/user ?user]]}
                db)))

(defn qusersposts [db user])



(defn dump-text [user]
  (map (comp #(spit user % :append true) pr-str beitrag->hash) (map #(nth % 2) (qbeitragvon (d) user))))
;; (defn qtext [db text]
;;   (q {:find }))


