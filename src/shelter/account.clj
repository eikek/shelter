(ns shelter.account
  (:require
   [clojure.java.jdbc :as sql]
   [shelter.config :as config]
   [shelter.migration :as migration]
   [shelter.store :as store]
   [shelter.secret :as secret]))

;; default values
(config/set {:password-chars "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ123456789-_+*?"})


(defmacro ^:private query-exists? [name query doc]
  (if (= 1 (reduce (fn [r c] (if (= c \?) (+ 1 r) r)) 0 query))
    `(defn ~(symbol name)
       ~doc
       ([conn# id#]
          (not= () (sql/query conn# [~query id#])))
       ([id#]
          (store/with-conn conn#
            (~(symbol name) conn# id#))))
    `(defn ~(symbol name)
       ~doc
       ([conn# id1# id2#]
          (not= () (sql/query conn# [~query id1# id2#])))
       ([id1# id2#]
          (store/with-conn conn#
            (~(symbol name) conn# id1# id2#))))))

(query-exists? "app-exists?"
               "select appid from shelter_application where appid = ?"
               "Return true if an application already exists with this ID.")

(query-exists? "account-login-exists?"
               "select login from shelter_account where login = ?"
               "Return true if an account with LOGIN already exists.")

(query-exists? "app-granted?"
               "select * from shelter_account_app where login = ? and appid = ?"
               "Return true if there is a mapping for login and appid.")

(query-exists? "alias-exists?"
               "select loginalias from shelter_alias where loginalias = ?"
               "Return true if the given alias already exists.")

(query-exists? "secret-default-exists?"
               "select data from shelter_secret where login = ? and appid is null"
               "Return true if a default secret exists for a given login.")

(query-exists? "secret-app-exists?"
               "select data from shelter_secret where login = ? and appid = ?"
               "Return true if a application specific secret exists for a given login and appid.")

(defn resolve-alias
  "Resolve the given alias to the login it belongs.
  If a login is given, it is returned."
  ([alias]
   (store/with-conn conn
     (resolve-alias conn alias)))
   ([conn alias]
    (or (-> (sql/query conn ["select * from shelter_alias where loginalias = ?" alias])
            (first)
            (:login))
        (-> (sql/query conn ["select login from shelter_account where login = ?" alias])
            (first)
            (:login)))))

(defn account-exists?
  "Checks whether an account with the given login or alias exists."
  ([conn login-or-alias]
   (or (account-login-exists? login-or-alias)
       (if (not (empty? (sql/query conn ["select loginalias from shelter_alias where loginalias = ?" login-or-alias])))
         true)))
  ([login-or-alias]
   (store/with-conn conn
     (account-exists? conn login-or-alias))))

(defn list-aliases
  "List all aliases of the given login."
  ([login]
   (store/with-conn conn
     (list-aliases conn login)))
  ([conn login]
   (let [resolved (resolve-alias conn login)]
     (vec (map #(:loginalias %)
               (sql/query conn ["select loginalias from shelter_alias where login = ?" resolved]))))))

(defn add-alias
  "Add an ALIAS for the account LOGIN.
  Aliases must be globally unique."
  ([login alias]
   (store/with-conn conn
     (add-alias conn login alias)))
  ([conn login alias]
   (let [resolved (resolve-alias conn login)
         data {:loginalias alias :login resolved}]
     (or (if (not (account-login-exists? conn resolved)) {:error "Account does not exist."})
         (if (account-exists? conn alias) {:error "The alias is already in use."})
         (do
           (sql/insert! conn :shelter_alias data)
           data)))))

(defn remove-alias
  "Remove the mapping to the given alias."
  ([alias]
   (store/with-conn conn
     (remove-alias conn alias)))
  ([conn alias]
   (let [rc (first (sql/delete! conn :shelter_alias [ "loginalias = ?" alias ]))]
     (if (= 0 rc)
       {:error "The alias does not exist."}
       true))))

(defn list-applications
  ([] (store/with-conn conn
        (list-applications conn)))
  ([conn]
   (sql/query conn "select * from shelter_application")))

(defn add-application
  "Adds a new application into the database."
  ([id name]
   (store/with-conn conn
     (add-application conn id name)))
  ([conn id name]
   (if (app-exists? conn id)
     {:error "An application exists with this name."}
     (let [app {:appid id :appname name}]
       (sql/insert! conn :shelter_application app)
       app))))

(defn grant-app
  "Grant the account LOGIN all apps in APPIDS."
  ([login appids]
   (store/with-conn conn
     (grant-app conn login appids)))
  ([conn login appids]
   {:pre [ (vector? appids) ]}
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "Account does not exist."})
         (if (some false? (map app-exists? appids))
           {:error "The appids vector contains unknown appids."})
         (doseq [aid appids]
           (let [data {:login login :appid aid}]
             (if (not (app-granted? conn login aid))
               (sql/insert! conn :shelter_account_app data))))
         true))))

(defn revoke-app
  "Revoke apps in APPIDs from account LOGIN."
  ([login appids]
   (store/with-conn conn
     (revoke-app conn login appids)))
  ([conn login appids]
   {:pre [ (vector? appids) ]}
   (let [resolved (resolve-alias conn login)]
   (or (if (not resolved)
         {:error "Account does not exist."})
       (if (some false? (map app-exists? appids))
         {:error "The appids vector contains unknown appids."})
       ;;todo improve by constructing a vector of all conditions
       (doseq [aid appids]
         (let [data ["login = ? and appid = ?"  resolved aid]]
           (sql/delete! conn :shelter_account_app data)))
       true))))

(defn account-apps
  "Return all granted apps for account LOGIN."
  ([login]
   (store/with-conn conn
     (account-apps conn login)))
  ([conn login]
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "Account does not exist."})
         (sql/query conn ["select t1.* from shelter_application t1, shelter_account_app t2 where t1.appid = t2.appid and t2.login = ?" resolved])))))

(defn update-password
  "Update the password for the given account and optionally
  application. A password is created, if it does not exist yet."
  ([login password appid]
   (store/with-conn conn
     (update-password conn login password appid)))
  ([conn login password appid]
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "Account does not exist."})
         (if (and appid (not (app-exists? appid)))
           {:error "The application does not exist."})
         (let [secret (secret/make-password password)
               data (-> secret (assoc :login resolved) (assoc :appid appid))
               update? (if appid (secret-app-exists? conn resolved appid)
                           (secret-default-exists? conn resolved))]
           (apply
            (if update? sql/update! sql/insert!)
            (filterv #(not= nil %)
                     [conn :shelter_secret
                      data (if update?
                             (if appid
                               ["type = ':password' and login = ? and appid = ?" resolved appid]
                               ["type = ':password' and login = ? and appid is null" resolved]))])))))))

(defn reset-password
  "Reset the password of LOGIN to some random value."
  ([login appid]
   (store/with-conn conn
     (reset-password conn login appid)))
  ([conn login appid]
   (let [alpha (config/get :password-chars)
         len 15
         pw (clojure.string/join
             (map #(.charAt alpha %)
                  (take len (repeatedly #(int (rand (.length alpha)))))))]
     (update-password login pw appid)
     pw)))

(defn get-secrets
  "Return the secrets of the given account for the given
  application. If there is no application specific secret, the default
  secrets are returned as if APPID were nil. There may be multiple
  secrets (with different type), return them in a sequence."
  ([login appid]
   (store/with-conn conn
     (get-secrets conn login appid)))
  ([conn login appid]
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "The account does not exist."})
         (if (and appid (not (app-exists? conn appid))) {:error "The application does not exist."})
         (let [query "select type, hash, data from shelter_secret where "
               sec-app (if appid
                         (sql/query conn [(str query "login = ? and appid = ?")
                                          resolved appid]))]
           (or sec-app
               (sql/query conn [(str query " appid is null and login = ?") resolved])))))))


(defn get-properties
  ([login appid]
   (store/with-conn conn
     (get-properties conn login appid)))
  ([conn login appid]
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "The account does not exist."})
         (if (and appid (not (app-exists? conn appid))) {:error "The application does not exist."})
         (first
          (if appid
            (sql/query conn ["select * from shelter_account_property where appid = ? and login = ?" appid resolved])
            (sql/query conn ["select * from shelter_account_property where appid is null and login = ?" resolved])))))))

(defn set-properties
  ([login appid props]
   (store/with-conn conn
     (set-properties conn login appid props)))
  ([conn login appid props]
   (let [resolved (resolve-alias conn login)]
     (or (if (not resolved) {:error "The account does not exist."})
         (if (and appid (not (app-exists? conn appid))) {:error "The application does not exist."})
         (let [this-props (get-properties conn login appid)]
           (if (empty? this-props)
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
                            ["appid is null and login = ?" resolved]))))))))


(defn retrieve
  "Loads an account by a given name."
  ([name]
   (store/with-conn conn
     (retrieve conn name)))
  ([conn name]
   (let [resolved (resolve-alias conn name)]
     (or (if (not resolved) {:error "The account does not exist."})
         {:name resolved
          :aliases (list-aliases conn resolved)
          :apps (account-apps conn resolved)
          :profiles (reduce
                     conj
                     {:default {:secrets (get-secrets conn resolved nil)
                                :properties (get-properties conn resolved nil)}}
                     (map (fn [appid]
                            {(keyword appid) {:secrets (get-secrets conn resolved appid)
                                              :properties (get-properties conn resolved appid)}})
                          (map :appid (account-apps conn resolved))))}))))

(defn register
  "Register a new ACCOUNT. The only required data is `login'.
Values for email, name and pass are optional."
  [login & [pass name email]]
  {:pre [ login ] }
  (let [secret (if pass (secret/make-password pass))]
    (store/with-conn conn
      (if (account-exists? conn login)
        {:error "This login name is used. Please try another one."}
        (do
          (sql/insert! conn :shelter_account
                       {:login login :name name :email email})
          (if secret
            (sql/insert! conn :shelter_secret (assoc secret :login login)))
          true)))))

(defn list-accounts
  ([]
   (store/with-conn conn
     (list-accounts conn)))
  ([conn]
   (vec (map (fn [login]
          {:login login
           :apps (vec (map :appid (account-apps conn login)))
           :aliases (list-aliases conn login)})
        (map :login(sql/query conn "select login from shelter_account"))))))

(defn validate
  "Load the account of NAME and validate it by checking DATA against
  the accounts secrets. Use the default profile unless APP is
  given. DATA is usually a plain password."
  [name data & [app]]
  (when-let [account (retrieve name)]
    (let [secrets (get-secrets name app)]
      (if (and (not (:error secrets)) (not-empty secrets))
        (secret/verify secrets data)))))
