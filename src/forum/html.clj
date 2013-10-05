(ns forum.html
  (:use hiccup.core
        [hiccup.form :as hf]
        [hiccup.element :as he]
        [hiccup.page :as hp]))

(def message
  (hp/html4
   [:head
    [:link {:href "default.css" :type "text/css" :rel "stylesheet"}]]))

(defn id->link [id]
  (str "/message/" id))

(defn parent [id title user date]
  (html [:li.next [:a {:href (id->link id)} title] [:b.gast user] [:i date]]))

(defn beitrag->html [{user :beitrag/user titel :beitrag/titel text :beitrag/text datum :beitrag/datum :as b}]
  (html [:div.basisinformation [:p [:b (str "Geschrieben von " user " " datum)]]]
        [:div.beitragstext_1 [:blockqoute text]]
        (map childs (:beitrag/antworten b))))

(defn parents [entity]
  [:ul (loop [parent (:_beitrag/antworten entity)]
         (when parent
           [:li {:href (id->link (:beitrag/id entity))} (:beitrag/titel entity) (:beitrag/user entity)]
           (recur parent)))])

(defn childs [entity]
  (html [:ul [:li {:href (id->link (:beitrag/id entity))} (:beitrag/titel entity) (:beitrag/user entity)]
         (or [:ul (map childs (:beitrag/antworten entity))] [])]))
