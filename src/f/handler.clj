(ns f.handler
  (:use [clojure.set :only [rename-keys]]
        ring.adapter.jetty
        [compojure.core :as compojure :only (GET POST ANY defroutes)])
  (:require [f.db :as db]
            [clojure.repl :as r]
            [clojure.instant :as i]
            [datomic.api :as d]
            [net.cgrand.enlive-html :as en]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [hiccup.core :as hc])
  (:import java.text.SimpleDateFormat))

(en/deftemplate index "template-index.html"
  [ctxt]
  [:td#baum] (en/html-content (:index ctxt)))

(defn message-id->message-link [id]
  (str "/message/" id))

(defn html-index-liste [bs]
  (map (fn [{titel :beitrag/titel user :beitrag/user id :beitrag/id}] [:ul [:li [:a {:href (message-id->message-link id)} titel] [:b user]]])
   bs))

(en/deftemplate message "template-message.html"
  [ctxt]
  [:td.vorgaenger :ul] (en/html-content (:vorgaenger ctxt))
  [:td.antworten :ul] (en/html-content (:antworten ctxt))
  [:div.basisinformation :span.gast] (en/content (str (:user ctxt) " (" (:datum ctxt) ")"))
  [:title] (en/content (:titel ctxt))
  [:h1] (en/content (:titel ctxt))
  [:h2] (en/content (:titel ctxt))
  [:div.beitragstext_1] (en/html-content  (:text ctxt)))

(defn html-vorgaenger [{id :beitrag/id titel :beitrag/titel person :beitrag/user }]
  [:li
   [:a {:href (message-id->message-link id)} titel]
   [:b.gast person]])

(defn html-antworten-liste [{user :beitrag/user id :beitrag/id titel :beitrag/titel antworten :beitrag/antworten}]
  (conj [:li [:a {:href (message-id->message-link id)} titel] [:b.gast user]] (if-not (empty? antworten) [:ul (map html-antworten-liste antworten)])))

(defn message-entity->ctxt [db {antworten :beitrag/antworten titel :beitrag/titel text :beitrag/text id :db/id :as e}]
  (-> e
      (select-keys [:beitrag/titel :beitrag/text :beitrag/user :beitrag/datum])
      (assoc :beitrag/antworten (hc/html (map html-antworten-liste (:beitrag/antworten e))))
      (assoc :vorgaenger (hc/html (map html-vorgaenger (db/qparents3 e))))
      (rename-keys {:beitrag/titel :titel :beitrag/text :text :beitrag/antworten :antworten :beitrag/user :user :beitrag/datum :datum})))

(def formatter (org.joda.time.format.DateTimeFormat/forPattern "yyyy-MM-dd"))

(defroutes ^{:private true} routes
  (GET "/" []
       (let [db (db/fd)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qindexraw db))])}]
         (index ctxt)))
  (GET "/index/:page" [page]
       (let [page (read-string page)
             db (db/fd)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qindexraw db page))])}]
         (index ctxt)))
  (GET "/message/:id" [id]
       (let [id (read-string id)
             db (db/fd)
             message-entity (d/touch (d/entity db (ffirst (db/qbeitrag db id))))
             ctxt (message-entity->ctxt db  message-entity)]
         (message ctxt)))
  (GET "/beitrag.php" []
       (slurp "src/template-beitrag.html"))
  (GET "/aktuell.php" {{tag1 :tag_1 tag2 :tag_2} :params}
       (let [tag1 (.parseDateTime formatter tag1)
             tag2 (.parseDateTime formatter tag2)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qwithin (db/fd) tag1 tag2))])}]
         ;; (pr-str tag1)
         (index ctxt)))
  (POST "/beitrag.php" []
        (slurp "src/template-beitrag.html"))
  (route/files "/einstellungen"))

(def app
  (-> routes
      handler/site))

(defn start []
  (defonce server (run-jetty #'app {:port 8888 :join? false})))
