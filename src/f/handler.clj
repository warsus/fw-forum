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
            [hiccup.core :as hc]
            [hiccup.element :as he])
  (:import java.text.SimpleDateFormat))

(en/deftemplate index "template-index.html"
  [ctxt]
  [:td#baum] (en/html-content (:index ctxt)))

(defn message-id->message-link [id]
  (str "/message/" id))

(defn user-name->user-link [uid name]
  (he/link-to (str "/user/" uid) name))

(defn message-preview [{titel :beitrag/titel {user :user/name uid :db/id} :beitrag/user id :beitrag/id :or {id 0}}]
  [:ul [:li (he/link-to (message-id->message-link id) titel) " - "[:b (user-name->user-link uid user)]]])

(defn html-index-liste [bs]
  (map message-preview bs))

(en/deftemplate message "template-message.html"
  [ctxt]
  [:td.vorgaenger :ul] (en/html-content (:vorgaenger ctxt))
  [:td.antworten :ul] (en/html-content (:antworten ctxt))
  [:div.basisinformation :span.gast] (en/content (str (:user ctxt) " (" (:datum ctxt) ")"))
  [:title] (en/content (:titel ctxt))
  [:h1] (en/content (:titel ctxt))
  [:h2] (en/content (:titel ctxt))
  [:div.beitragstext_1] (en/html-content  (:text ctxt)))

(defn html-vorgaenger [{id :beitrag/id titel :beitrag/titel {person :user/name} :beitrag/user}]
  [:li
   (he/link-to (message-id->message-link id) titel)
   [:b.gast person]])

(defn html-antworten-liste [{{user :user/name} :beitrag/user id :beitrag/id titel :beitrag/titel antworten :beitrag/antworten}]
  (conj [:li (he/link-to (message-id->message-link id) titel) [:b.gast user]]
        (if-not (empty? antworten) [:ul (map html-antworten-liste antworten)])))

(defn message-entity->ctxt [db {antworten :beitrag/antworten titel :beitrag/titel text :beitrag/text id :db/id :as e}]
  (-> e
      (select-keys [:beitrag/titel :beitrag/text :beitrag/user :beitrag/datum])
      (update-in [:beitrag/user] :user/name)
      (assoc :beitrag/antworten (hc/html (map html-antworten-liste (:beitrag/antworten e))))
      (assoc :vorgaenger (hc/html (map html-vorgaenger (db/qparents e))))
      (rename-keys {:beitrag/titel :titel :beitrag/text :text :beitrag/antworten :antworten :beitrag/user :user :beitrag/datum :datum})))

(def formatter (org.joda.time.format.DateTimeFormat/forPattern "yyyy-MM-dd"))

(defroutes ^{:private true} routes
  (GET "/" []
       (let [db (db/d)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qindexraw db))])}]
         (index ctxt)))
  (GET "/index/:page" [page]
       (let [page (read-string page)
             db (db/d)
             ctxt {:index (hc/html [:ul (html-index-liste (db/qindexraw db page))])}]
         (index ctxt)))
  ;;TODO: hm some postso dont turn uup whut?
  (GET "/user/:id" [id]
       (let [id (Long/parseLong id)
             db (db/d)
             ctxt {:index (hc/html [:ul (html-index-liste (:beitrag/_user (d/pull db [{:beitrag/_user '[*]}] id)))])}]
         (index ctxt)))
  (GET "/von/:name" [name]
       (let [db (db/d)]
         (hc/html [:ul (map
                        (comp (fn [{id :beitrag/id titel :beitrag/titel}] [:li (he/link-to (message-id->message-link id) titel)])
                              #(db/e db %) #(nth % 2))
                        (db/qbeitragvon db name))])))
  (GET "/users" []
       (let [db (db/d)]
         (hc/html [:ul  (map
                         (fn [[_ name]]
                           [:li (he/link-to (str "/von/" name) name)]) (db/qusers db))])))
  (GET "/ivan" []
       (let [db (db/d)]
         (hc/html [:ul (map (comp
                             (fn [{id :beitrag/id titel :beitrag/titel}] [:li [:a {:href (message-id->message-link id)} titel]])
                             #(db/e db %) #(nth % 0)) (db/qmatchingusersposts db "Ivan"))])))
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
  (route/files "/einstellungen")
  (GET "/lol" [] "lol"))

(def app
  (-> routes
      handler/site))

(defn start []
  (defonce server (run-jetty #'app {:port 8888 :join? false})))

