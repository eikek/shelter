(ns shelter.store
  (:require
   [shelter.config :as config]
   [clojure.java.jdbc :as sql]))

;; default values
(config/set {:database "users.db"})


(defn make-db [file]
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname file})

(defmacro with-conn
  "Runs BODY inside a connection to FILE where BINDING holds the
  current connection."
  [binding & body]
  `(sql/with-db-connection [~binding (make-db (config/get :database))]
     (do
       (sql/db-do-commands ~binding "pragma foreign_keys = ON;")
       ~@body)))
