(ns shelter.migration
  (:require
   [shelter.config :as config]
   [shelter.store :as store]
   [clojure.java.jdbc :as sql]))

(defn get-version
  "Return the current database migration version."
  [conn]
  (try
    (-> (sql/query conn ["select max(version) as version from shelter_migration_log"])
        first
        (get :version))
    (catch Exception e
      nil)))

(defn insert-log
  "Insert a log entry in migration table."
  [conn version text]
  (let [curtime (System/currentTimeMillis)
       curvers (or (get-version conn) -1)]
    (if (>= curvers version)
      (throw (Exception. "Cannot set a version that is <= the current one.")))
    (sql/insert! conn :shelter_migration_log {:created curtime :version version :description text})))

(defn create-sqlite-initial-tables [conn]
  (let [tables [ "create table if not exists shelter_migration_log (
                    created int, version int, description text)"
                 "create table if not exists shelter_account (
                    login text primary key,
                    locked int, name text, email text unique,
                    lastlogin int, logincount int, failedlogins int)"
                 "create table if not exists shelter_alias (
                    loginalias text primary key,
                    login text,
                    unique (loginalias, login)
                    foreign key (login) references shelter_account(login) on delete cascade)"
                 "create table if not exists shelter_application (
                    appid text primary key,
                    appname text,
                    url text,
                    description text)"
                 "create table if not exists shelter_account_app (
                    login text, appid text,
                    primary key (login, appid)
                    foreign key (login) references shelter_account(login) on delete cascade
                    foreign key (appid) references shelter_application(appid) on delete cascade)"
                 "create table if not exists shelter_secret (
                    login text, appid text, type text, hash text, data text,
                    primary key (login, appid, type)
                    foreign key (login) references shelter_account(login) on delete cascade
                    foreign key (appid) references shelter_application(appid) on delete cascade)"
                 "create table if not exists shelter_account_property (
                    login text, appid text, name text, email text, homepage text,
                    primary key (login, appid)
                    foreign key (login) references shelter_account(login) on delete cascade
                    foreign key (appid) references shelter_application(appid) on delete cascade)"]]
    (sql/db-do-commands conn tables)
    (insert-log conn 1 "Initial tables created.")
    (println "Initial tables created.")))

(defn migrate-db [file]
  (sql/with-db-connection [conn (store/make-db file)]
    (let [current-version (or (get-version conn) -1)]
      (when (< current-version 1)
        (create-sqlite-initial-tables conn)))))


(config/watch :migrate
              (fn [key old new]
                (if (not= (:database old) (:database new))
                  (do (println "Migrate database schema…")
                      (migrate-db (:database new))))))

(defn -main []
  (println "Updating database schema…")
  (migrate-db "database.db")
  (println "Done."))
