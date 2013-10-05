(ns forum.core
  (:use [forum.handler :only [start]]
        [forum.scrape :only [init-drive! init-forum-db!]]
        [forum.db :only [load-schema]]))

(defn -main
  [mode]
  (if (= mode "production")
    (start)
    (do
      (init-forum-db!)
      (start))))
