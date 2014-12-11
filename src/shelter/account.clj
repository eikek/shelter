(ns
    ^{:author "Eike Kettner",
      :doc "Shelter is a set of functions for managing user accounts.


Functions in this namespace modify and read the account database.
"}
  shelter.account
  (:require
   [clojure.java.jdbc :as sql]
   [shelter.config :as config]
   [shelter.migration :as migration]
   [shelter.store :as store]
   [shelter.secret :as secret]))

(defn- something-exists?
  [conn table query]
  (not= (-> (sql/query conn
                       (into [(str "select count(*) as count from shelter_" (name table) " where " (first query))]
                             (rest query)))
            (first)
            (:count))
        0))

(defn login-exists?
  "Check whether the given LOGIN exists. Do not resolve aliases."
  [conn login]
  (something-exists? conn :account ["login = ?" login]))

(defn alias-exists?
  "Check whether the given ALIAS exists."
  [conn alias]
  (something-exists? conn :alias ["loginalias = ?" alias]))

(defn account-exists?
  "Check whether an account already exists for LOGIN. Resolve
  aliases."
  [conn login]
  (or (login-exists? conn login) (alias-exists? conn login)))

(defn alias-resolve
  "Resolve the given alias to the login it belongs.
  If a login is given, it is returned."
  [conn alias]
  (or (-> (sql/query conn ["select * from shelter_alias where loginalias = ?" alias])
          (first)
          (:login))
      (-> (sql/query conn ["select login from shelter_account where login = ?" alias])
          (first)
          (:login))))

(defn alias-list
  "List all aliases of the given login."
  [conn login]
  (let [resolved (alias-resolve conn login)]
    (mapv #(:loginalias %)
          (sql/query conn ["select loginalias from shelter_alias where login = ?" resolved]))))

(defn alias-add
  "Add an ALIAS for the account LOGIN.
  Aliases must be globally unique."
  [conn login alias]
  (let [resolved (alias-resolve conn login)
        data {:loginalias alias :login resolved}]
    (if (and resolved (not (account-exists? conn alias)))
      (do
        (sql/insert! conn :shelter_alias data)
        data))))

(defn alias-remove
  "Remove the mapping to the given alias."
  [conn alias]
  (not= 0
        (first (sql/delete! conn :shelter_alias [ "loginalias = ?" alias ]))))


(defn app-exists?
  "Check whether an app exists with id ID."
  [conn id]
  (something-exists? conn :application ["appid = ?" id]))

(defn app-list
  "Return the list of registered applications."
  [conn]
  (sql/query conn "select * from shelter_application"))

(defn app-add
  "Add a new application into the database."
  [conn id name]
  (if (not (app-exists? conn id))
    (let [app {:appid id :appname name}]
      (sql/insert! conn :shelter_application app)
      app)))

(defn app-remove
  "Remove the given app and all its references."
  [conn appid]
  (if (app-exists? conn appid)
    (do (doseq [kw [:shelter_account_app :shelter_secret :shelter_account_property :shelter_application]]
          (sql/delete! conn kw ["appid = ?" appid]))
        true)))

(defn app-enabled?
  "Check whether APPID is enabled for LOGIN."
  [conn login appid]
  (something-exists? conn :account_app ["login = ? and appid = ?" login appid]))

(defn app-enable
  "Grant the account LOGIN all apps in APPIDS."
  [conn login appids]
  (let [ids (if (string? appids) [appids] appids)
        resolved (alias-resolve conn login)]
    (if (and resolved (every? true? (map #(app-exists? conn %) ids)))
      (do (doseq [aid ids]
            (let [data {:login login :appid aid}]
              (if (not (app-enabled? conn login aid))
                (sql/insert! conn :shelter_account_app data))))
          true))))


(defn app-disable
  "Revoke apps in APPIDs from account LOGIN."
  [conn login appids]
  (let [ids (if (string? appids) [appids] appids)
        resolved (alias-resolve conn login)]
    (if (and resolved (every? true? (map #(app-exists? conn %) ids)))
      (do (doseq [aid ids]
            (let [data ["login = ? and appid = ?"  resolved aid]]
              (sql/delete! conn :shelter_account_app data)))
          true))))

(defn app-list-enabled
  "Return all granted apps for account LOGIN."
  [conn login]
  (let [resolved (alias-resolve conn login)]
    (if resolved
      (sql/query conn [(str "select t1.* from shelter_application t1, shelter_account_app t2"
                            " where t1.appid = t2.appid and t2.login = ?")
                       resolved]))))


(defn secret-exists?
  [conn login & [appid]]
  (if appid
    (something-exists? conn :secret ["login = ? and appid = ?" login appid])
    (something-exists? conn :secret ["login = ? and appid is null" login])))

(defn secret-update
  "Update the secret for the given account and optionally
  application. A secret is created, if it does not exist yet. SECRET
  should be a secret map as created by `secret/make-secret'."
  ([conn login secret & [appid]]
   (let [resolved (alias-resolve conn login)]
     (if (and resolved (if appid (app-exists? conn appid) true))
       (let [data (-> secret
                    (assoc :login resolved)
                    (assoc :appid appid)
                    (update-in [:type] name)
                    (update-in [:hash] name))
             update? (secret-exists? conn resolved appid)
             result (apply
                     (if update? sql/update! sql/insert!)
                     (filterv #(not= nil %)
                              [conn :shelter_secret data
                               (if update?
                                 (if appid
                                   ["type = 'password' and login = ? and appid = ?" resolved appid]
                                   ["type = 'password' and login = ? and appid is null" resolved]))]))]
         (not (empty? result)))))))

(defn secret-update-password
  "Update the secret using the given plain text password."
  [conn login password & [appid]]
  (let [secret (secret/make-password password)]
    (secret-update conn login secret appid)))

(defn secret-reset-password
  "Reset the password for the given account to some random
  value and return it."
  [conn login & [appid]]
  (let [pw (secret/random-string)]
    (if (secret-update-password conn login pw appid)
      pw)))

(defn secret-get
  "Return the secrets of the given account for the given
  application. If there is no application specific secret, the default
  secrets are returned as if APPID were nil. There may be multiple
  secrets (with different type), return them in a sequence."
  [conn login & [appid]]
  (let [resolved (alias-resolve conn login)]
    (if (and resolved (if appid (app-exists? conn appid) true))
      (let [query "select type, hash, data from shelter_secret where "
            secrets (if appid
                      (sql/query conn [(str query "login = ? and appid = ?") resolved appid])
                      (sql/query conn [(str query " appid is null and login = ?") resolved]))]
        (mapv (fn [m]
                (-> m
                  (update-in [:hash] keyword)
                  (update-in [:type] keyword)))
              secrets)))))

(defn account-properties-get
  "Return the property map for the given account and application."
  [conn login & [appid]]
  (let [resolved (alias-resolve conn login)
        query "select * from shelter_account_property where "]
    (if (and resolved (if appid (app-exists? conn appid) true))
      (first
       (if appid
         (sql/query conn [(str query "appid = ? and login = ?") appid resolved])
         (sql/query conn [(str query "appid is null and login = ?") resolved]))))))

(defn account-properties-set
  "Sets the property map PROPS to the account and
  application. Properties are currently: name, email and homepage."
  [conn login appid props]
  (let [resolved (alias-resolve conn login)]
    (if (and resolved (if appid (app-exists? conn appid) true))
      (let [this-props (account-properties-get conn login appid)
            result (if (empty? this-props)
                     (sql/insert! conn :shelter_account_property
                                  (-> props
                                    (assoc :login resolved)
                                    (assoc :appid appid)))
                     (sql/update! conn :shelter_account_property
                                  (conj this-props (-> props
                                                     (dissoc :login)
                                                     (dissoc :appid)))
                                  (if appid
                                    ["appid = ? and login = ?" appid resolved]
                                    ["appid is null and login = ?" resolved])))]
        (not (empty? result))))))

(defn account-list
  "Return a list with basic properties of each account."
  [conn]
  (mapv (fn [login]
          {:login login
           :apps (mapv :appid (app-list-enabled conn login))
           :aliases (alias-list conn login)})
        (map :login(sql/query conn "select login from shelter_account"))))


(defn account-details-get
  "Get the account details, the registration information and admin
  info. Return a map with following properties: locked bool, name
  text, email text,lastlogin int, logincount int, failedlogins int"
  [conn login]
  (let [resolved (alias-resolve conn login)
        results (if resolved
                  (sql/query conn [(str "select locked,name,email,lastlogin,logincount,failedlogins "
                                        "   from shelter_account where login = ?") resolved]))]
    (if (not (empty? results))
      (-> (first results)
        (update-in [:locked] #(not= % 0))
        (update-in [:logincount] #(or % 0))
        (update-in [:failedlogins] #(or % 0))))))

(defn account-details-set
  "Set the details map to the account. If DETAILS is a function, it is
  expected to return a new details map given the current details."
  [conn login details]
  (let [resolved (alias-resolve conn login)]
    (if resolved
      (let [this-details (account-details-get conn login)
            new-map (if (fn? details)
                      (details this-details)
                      (conj this-details details))
            result (sql/update! conn :shelter_account
                                (-> new-map
                                  (dissoc :login)
                                  (update-in [:locked] #(if % 1 0)))
                                ["login = ?" resolved])]
        (not (empty? result))))))


(defn account-get
  "Load an account and all its properties by a given name."
  [conn name]
  (let [resolved (alias-resolve conn name)]
    (if resolved
      {:name resolved
       :details (account-details-get conn resolved)
       :aliases (alias-list conn resolved)
       :apps (app-list-enabled conn resolved)
       :profiles (reduce
                  conj
                  {:default {:secrets (secret-get conn resolved nil)
                             :properties (account-properties-get conn resolved nil)}}
                  (map (fn [appid]
                         {(keyword appid) {:secrets (secret-get conn resolved appid)
                                           :properties (account-properties-get conn resolved appid)}})
                       (map :appid (app-list-enabled conn resolved))))})))

(defn account-locked?
  "Check whether the given account is locked."
  [conn login]
  (:locked (account-details-get conn login)))

(defn account-set-locked
  "Lock or unlock an account. LOCKED is coerced into a boolean."
  [conn login locked]
  (account-details-set conn login {:locked (boolean locked)}))


(defn account-register
  "Register a new account. Activate the account if a password is
  given. Otherwise create a locked account. DETAILS may be an initial
  details map."
  [conn login & [pass details]]
  (if (not (account-exists? conn login))
    (let [ins (do
                (sql/insert! conn :shelter_account {:login login})
                (account-details-set conn login (assoc details :locked (empty? pass))))]
      (if (and pass ins)
        (secret-update-password conn login pass))
      ins)))
