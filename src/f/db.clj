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

(defn now []
  (java.util.Date.))

(def uri  "datomic:sql://forum?jdbc:postgresql://localhost:5432/datomic?user=datomic&password=datomic")

(def conn
  (ref nil))

(defn init-db []
  (d/create-database uri)
  (dosync (ref-set conn (d/connect uri))))

(defn d []
  (db @conn))

(defn beitrag->hash [id]
  (d/touch (e (d) id)))

(defn load-schema [conn]
  (let [schema
        [{:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/text
          :db/valueType :db.type/string
          :db/fulltext true
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag text"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/datum
          :db/valueType :db.type/instant
          :db/index true
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag text"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/titel
          :db/fulltext true
          :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/user
          :db/valueType :db.type/ref
          :db/index true
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/fulltext true
          :db/ident :user/name
          :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one
          :db/unique :db.unique/identity
          :db/doc "Beitrag titel"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/zeit
          :db/valueType :db.type/instant
          :db/cardinality :db.cardinality/one
          :db/doc "Beitrag Zeit"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/antworten
          :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many
          :db/index true
          :db/doc "Antworten"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/id
          :db/index true
          :db/unique :db.unique/identity
          :db/valueType :db.type/long
          :db/cardinality :db.cardinality/one
          :db/doc "id"
          :db.install/_attribute :db.part/db}
         {:db/id (d/tempid :db.part/db)
          :db/ident :beitrag/spam
          :db/index true
          :db/valueType :db.type/boolean
          :db/cardinality :db.cardinality/one
          :db/doc "id"
          :db.install/_attribute :db.part/db}]]
    @(d/transact conn schema)))

(defn qbeitrag
  ([db] (q {:find '[?b]
            :where '[[?b beitrag/id ?id]]} db))
  ([db id] (q {:find '[?b]
               :in   '[$ ?id]
               :where  '[[?b beitrag/id ?id]]} db id)))

(defn bids->id [db bids]
  (map #(:db/id (d/entity db [:beitrag/id %])) bids))

(defn qbeitragvon ([db user]
                   (q {:find '[?user ?bid ?b]
                       :in '[$ ?username]
                       :where '[[?b :beitrag/id ?bid]
                                [?b :beitrag/user ?user]
                                [?user :user/name ?username]]} db user)))

(defn qbeitragvonmatch
  ([db pattern]
   (q {:find '[?user]
       :in '[$ ?pattern]
       :where '[[?b beitrag/id ?bid]
                [(fulltext $ :beitrag/user ?pattern) [[?b ?user]]]]} db pattern)))

(def parent-rule '[[[parent ?b1 ?b2]
                    [?b1 :beitrag/antworten ?b2]]
                   [[parent ?b1 ?b3]
                    [?b1 :beitrag/antworten ?b2]
                    [parent ?b2 ?b3]]])

(defn qparents [e]
  (reverse (take-while #(not (nil? %)) (rest (iterate #(first (:beitrag/_antworten %)) e)))))

(defn within [intervall datum]
  (.contains intervall (.getTime datum)))

(defn qwithin [db start end]
  (map (comp #(d/pull (d) '[:beitrag/id :beitrag/user :beitrag/titelq] %) :e) (d/index-range db :beitrag/datum start end)))

;;This seems not very efficient at first glance but works for now
(defn qindexraw
  ([db]
   (map #(d/entity db (:e %)) (reverse (take-last 50 (d/datoms db :avet :beitrag/id)))))
  ([db page] (map #(d/entity db (:e %)) (reverse (take 50 (take-last (* page 50) (d/datoms db :avet :beitrag/id)))))))

(defn qbeitragvonmit [db user pattern]
  (q {:find '[?id ?bid]
      :where '[[?id :beitrag/id ?bid]
               [?id :beitrag/user ?user]
               [?user :user/name ?username]
               [(fulltext $ :beitrag/text ?pattern) [[?id]]]]
      :in '[$ ?username ?pattern]} db user pattern))

(defn qbeitragtext [db pattern]
  (q {:find '[?id]
      :where '[[?id :beitrag/id ?bid]
               [(fulltext $ :beitrag/text ?pattern) [[?id]]]]
      :in '[$ ?pattern]} db pattern))

(defn qusers
  ([db]
   (q '[:find ?id ?name
        :where [?id :user/name ?name]]
      db))
  ([db name]
   (q {:find '[?user ?name]
       :where '[[?user :user/name ?name]]
       :in '[$ ?name]}
      db name)))

(defn qmatchingusers [db pattern]
  (q {:find '[?e]
      :where '[[(fulltext $ :user/name "Ivan") [[?e]]]]
      :in '[$ ?pattern]} db pattern))

(defn qmatchingusersposts [db pattern]
  (q {:find '[?b ?e]
      :where '[[(fulltext $ :user/name "Ivan") [[?e]]]
               [?b :beitrag/user ?e]
               [(fulltext $ :beitrag/text " irc ") [[?b]]]]
      :in '[$ ?pattern]} db pattern))

(defn qrepliestouser [db uid]
  (q {:find '[?bid2]
      :in '[[$ ?uid]]
      :where '[[?bid :beitrag/user ?uid]
               [?bid :beitrag/antworten ?bid2]]} db uid))

(def conversation-rule
  '[[[conversation ?b1 ?u1 ?u2]
     [?b1 :beitrag/user ?u1]
     [?b2 :beitrag/user ?u2]
     [?b1 :beitrag/antworten ?b2]]
    [[conversation ?b1 ?u2 ?u1]
     [?b1 :beitrag/user ?u1]
     [?b2 :beitrag/user ?u2]
     [?b1 :beitrag/antworten ?b2]]])



(defn conversations-of
  ([db u1 u2]
   (q {:find '[?bid]
       :where '[[conversation ?bid ?u1 ?u2]]
       :in '[$ % ?u1 ?u2]} db conversation-rule u1 u2))
  ([db u1]
      (q {:find '[?u2]
       :where '[[conversation ?bid ?u1 ?u2]]
       :in '[$ % ?u1]} db conversation-rule u1)))

(defn conversations-of-names
  ([db u1 u2]
   (conversations-of db (:db/id (d/entity (d) [:user/name u1])) (:db/id (d/entity (d) [:user/name u2]))))
  ([db u1]
      (conversations-of db (:db/id (d/entity (d) [:user/name u1])))))

(defn qcount [db]
  (q '[:find (count ?eid) 
      :where [?id :beitrag/id]
      ] db))
