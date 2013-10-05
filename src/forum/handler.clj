(ns forum.handler
  (:use    [clojure.set :only [rename-keys]]
           ring.adapter.jetty           
           [compojure.core :as compojure :only (GET POST ANY defroutes)])
  (:require
   [forum.db :as db]
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
  (map 
   (fn [[id titel user]] [:ul [:li [:a {:href (message-id->message-link id)} titel] [:b user]]])
   bs))

(en/deftemplate message "template-message.html"
  [ctxt]
  [:td.vorgaenger :ul] (en/html-content (:vorgaenger ctxt))
  [:td.antworten :ul] (en/html-content (:antworten ctxt))
  [:div.basisinformation :span.gast] (en/content (:user ctxt))
  [:h1] (en/content (:titel ctxt))
  [:h2] (en/content (:titel ctxt))
  [:div.beitragstext_1] (en/content (:text ctxt)))

(defn html-vorgaenger-liste [db bid]
  [:ul (map (fn [[id titel person]] [:li
                                    [:a {:href (message-id->message-link id)} titel]
                                    [:b.gast person]]) (db/qparents db bid))])

(defn html-antworten-liste [{id :beitrag/id titel :beitrag/titel antworten :beitrag/antworten}]
  (conj [:li [:a {:href (message-id->message-link id)} titel]] (if-not (empty? antworten) [:ul (map html-antworten-liste antworten)])))

(defn message-entity->ctxt [db {antworten :beitrag/antworten titel :beitrag/titel text :beitrag/text id :db/id :as e}]
  (-> e
   (select-keys [:beitrag/titel :beitrag/text :beitrag/user])
   (assoc :beitrag/antworten (hc/html (map html-antworten-liste (:beitrag/antworten e))))
   (assoc :vorgaenger (hc/html (html-vorgaenger-liste db id)))
   (rename-keys {:beitrag/titel :titel :beitrag/text :text :beitrag/antworten :antworten :beitrag/user :user})))

(def formatter (org.joda.time.format.DateTimeFormat/forPattern "yyyy-MM-dd"))

(defroutes ^{:private true} routes
  (GET "/" []
       (let [db (db/d)
             ctxt {:index (hc/html [:ul (html-index-liste (map #(db/qbeitragbyid db %) (ffirst (db/qindex db))))])}]
         (index ctxt)))
  (GET "/message/:id" [id]
       (let [id (read-string id)
             db (db/d)
             message-entity (d/touch (d/entity db (ffirst (db/qbeitrag db id))))
             ctxt (message-entity->ctxt db  message-entity)]
         (message ctxt)))
  (GET "/beitrag.php" []
       (slurp "src/template-beitrag.html"))
  (GET "/aktuell.php" {{tag1 :tag_1 tag2 :tag_2} :params}
       (let [tag1 (.parseDateTime formatter tag1)
             tag2 (.parseDateTime formatter tag2)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qwithin (db/d) tag1 tag2))])}]
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

