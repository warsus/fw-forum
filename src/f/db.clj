(ns f.db
  (:require [clojure.java.io :as io])
  (:use [datomic.api :only [q db] :as d])
  (:use clojure.repl)
  (:use clojure.pprint)
  (:use hiccup.core)
  (:require [clojure.set :as s]))

(def e d/entity)

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

(def uri "datomic:free://localhost:4334/forum")

(d/create-database uri)

(def conn
  (d/connect uri))

(defn d []
  (db conn))

(defn load-schema []
  (let [schema
        [{:db/id #db/id[:db.part/db]
          :db/ident :beitrag/text
          :db/valueType :db.type/string
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
          :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :beitrag/user
          :db/valueType :db.type/string
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
          :db/ident :spam/beitrag
          :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :spam/id
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
          :db/unique :db.unique/identity
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :ham/beitrag
          :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :ham/id
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
          :db/unique :db.unique/identity
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :word/ham-count
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :word/ham-count
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :word/word
          :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
          :db/unique :db.unique/identity
          :db.install/_attribute :db.part/db}
         {:db/id #db/id[:db.part/db]
          :db/ident :word/spam-count
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "Antworten"
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

(defn qbeitragbyid [db id]
  (let [e (d/entity db id)]
    [(get e :beitrag/id) (get e :beitrag/titel) (get e :beitrag/user)]))

(defn qbeitragvon ([db user]
                     (q {:find '[?b]
                         :in '[$ ?user]
                         :where '[[?b beitrag/id _]
                                  [?b beitrag/user ?user]]} db user)))

(defn antworten
  ([db]
     (q {:find '[?a ?b]
         :where '[[?b :beitrag/antworten ?a]]} db))
  ([db bid]
     (q {:find '[?a ?bid]
         :in '[$ ?id]
         :where '[[?b :beitrag/id ?id]
                  [?a :beitrag/id ?bid]
                  [?b :beitrag/antworten ?a]]} db bid)))

(defn qqparents [db id]
  (get (d/entity db id) :beitrag/_antworten))

(def parent-rule '[[[parent ?b1 ?b2]
                    [?b1 :beitrag/antworten ?b2]]
                   [[parent ?b1 ?b3]
                    [?b1 :beitrag/antworten ?b2]
                    [parent ?b2 ?b3]]])

(defn qparents
  ([db bid]
     (q {:find '[?bid2 ?titel ?user ?pid]
         :in '[$ % ?bid]
         :where '[[parent ?pid ?bid]
                  [?pid :beitrag/id  ?bid2]
                  [?pid :beitrag/titel ?titel]
                  [?pid :beitrag/user ?user]]} db parent-rule bid))
  ([db] (q {:find '[?pid ?titel ?user ?bid2 
                    ]
            :in '[$ %]
            :where '[[parent ?pid ?bid]
                     [?pid :beitrag/titel ?titel]
                     [?pid :beitrag/id  ?bid2]
                     [?pid :beitrag/user ?user]]} db parent-rule)))

(defn within [intervall datum]
  (.contains intervall (.getTime datum)))

(defn qwithin [db date1 date2]
  (let [interval (org.joda.time.Interval. date1 date2)]
    (q {:find '[?bid]
        :in  '[$ ?interval]
        :where '[[?id :beitrag/id ?bid]
                 [?id :beitrag/datum ?datum]
                 [(f.db/within ?interval ?datum)]]} db interval)))

;; (defn qindex2 [db]
;;   (q {:find '[(max 10 ?datum)]
;;       :where '[[?id :beitrag/id ?bid]
;;                [?id :beitrag/user ?user]
;;                [?id :beitrag/datum ?datum]
;;                [?id :beitrag/titel ?titel]]} db))

(defn qindex [db]
  (q '[:find (max 50 ?bid)
       :where [?id :beitrag/id ?bid]
       [?id :beitrag/user ?user]
       [?id :beitrag/datum ?datum]
       [?id :beitrag/titel ?titel]] db))


(defn qduplicates [db]
  (q {:find '[?bid1 ?bid2]
      :where '[[?bid1 :beitrag/text ?text1]
               [?bid2 :beitrag/text ?text2]
               [(= ?text1 ?text2)]]} db))

(defn mark-spam! [db bids]
  (do
    @(d/transact conn (map
                       (fn [bid] {:db/id (d/tempid :db.part/user)
                                 :spam/id 1
                                 :spam/beitrag (ffirst (qbeitrag db bid))})
                       bids))
    true))

(defn mark-ham! [db bids]
  (do
    @(d/transact conn (map
                       (fn [bid] {:db/id (d/tempid :db.part/user)
                                 :ham/id 1
                                 :ham/beitrag (ffirst (qbeitrag db bid))})
                       bids))
    true))

(defn qspam [db]
  (q {:find '[?id ?bid]
      :where '[[?s :spam/id 1]
               [?s :spam/beitrag ?id]
               [?id :beitrag/id ?bid]]} db))
(defn qham [db]
  (q {:find '[?id ?bid]
      :where '[[?s :ham/id 1]
               [?s :ham/beitrag ?id]
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

(defn qhamtext [db]
  (q {:find '[?text]
      :where '[[?s :ham/id 1]
               [?s :ham/beitrag ?id]
               [?id :beitrag/id ?bid]
               [?id :beitrag/text ?text]]} db))

(defn qspamtext [db]
  (q {:find '[?text]
      :where '[[?s :spam/id 1]
               [?s :spam/beitrag ?id]
               [?id :beitrag/id ?bid]
               [?id :beitrag/text ?text]]} db))

(defn mark-forum-ham! []
  (mark-ham! (d) (map second (filter #(< (second %) 18000) (qbeitrag (d))))))

(defn mark-forum-spam! []
  (mark-spam! (d) (map second (filter #(> (second %) 18000) (qbeitrag (d))))))

