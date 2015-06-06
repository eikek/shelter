;;; rest.clj -- rest inferface to shelter.

(ns shelter.rest
  (:require
   [clojure.string :as s]
   [shelter.config :as config]
   [shelter.account :as account]
   [shelter.secret :as secret]
   [shelter.store :as store]
   [compojure.core :as web]
   [compojure.route :as web-route]
   [compojure.core :refer [GET POST]]
   [ring.util.response :refer :all]
   [ring.middleware.cookies :as cookies]
   [ring.middleware.keyword-params :as kwp]
   [ring.middleware.params :as params]
   [ring.middleware.json :as json]
   [ring.adapter.jetty :as jetty]))


;; default values
(config/set {:rest-port 9919
             :token-validity (* 20 60 1000)
             :cookie-secret (secret/random-string 27)
             :cookie-name "shelterauth"
             :cookie-path "/"
             :cookie-domain nil
             :cookie-http-only true
             :cookie-secure false
             :cookie-max-age nil})

(defonce ^:private rest-server (atom nil))

(def ^:private rest-routes (atom []))

(defn- register-route [key route & [append]]
  (swap! rest-routes
         (fn [routes]
           (if (some #(= (first %) key) routes)
             ;; replace existing route
             (map (fn [r] (if (= key (first r)) [key route] r))
                  routes)
             ;; otherwise ap/prepend
             (if append
               (conj routes [key route])
               (into [[key route]] routes))))))

(defn route-keys
  "Return the keys for which routes are registered."
  []
  (map first @rest-routes))

(defn prepend-route [key route]
  (register-route key route))

(defn append-route [key route]
  (register-route key route true))

(defn add-routes
  "Add multiple routes at once in key-value pair list. "
  [& route-defs]
  (let [kv (apply hash-map route-defs)]
  (doseq [routekey (keys kv)]
    (register-route routekey (routekey kv)))))

(defn clear-routes
  "This clears all registerd routes!"
  []
  (reset! rest-routes []))

(defn- create-handler []
  (apply web/routes
         (conj (mapv second @rest-routes)
               (web-route/not-found "Not found."))))

(defn- stop-jetty [server]
  (if (and server (.isRunning server))
             (do (println "Stopping rest server…")
                 (.stop server))))

(defn- start-jetty []
  (println (str "Starting rest server on port " (config/get :rest-port) "…"))
  (jetty/run-jetty (create-handler)
                   {:port (config/get :rest-port)
                    :join? false}))

(defn apply-routes
  "Apply the routes by making them available to a running server. If
  no server is running, it is started, otherwise it is restarted with
  the new routes."
  []
  (swap! rest-server
         (fn [server]
           (stop-jetty server)
           (start-jetty))))

(defn stop-server
  "Stops the rest server if it is running."
  []
  (swap! rest-server
         (fn [server]
           (stop-jetty server)
           nil)))

(defn start-server
  "Starts the rest server with the current set of routes. Do nothing
  if already running."
  []
  (swap! rest-server
         (fn [server]
           (if (or (not server) (not (.isRunning server)))
             (start-jetty)
             server))))


(defn- sign [salt time login app secrets]
  (let [data (str salt "$" time "$" login "$" app)
        secret (apply str (sort (map :data secrets)))
        sig (secret/sign (str (config/get :cookie-secret) secret) data)]
    [data sig]))

(defn- compare-sigs [sig1 sig2]
  (every? identity (map = sig1 sig2)))

(defn- current-millis []
  (System/currentTimeMillis))

(defn make-authtoken
  "Create an authenticator for the given login and app."
  [login & [app]]
  (let [appid (not-empty app)
        salt (secret/random-string 12)
        secrets (store/with-conn conn (account/secret-get conn login appid))]
    (if (not-empty secrets)
      (s/join "$" (sign salt (current-millis) login appid secrets)))))

(defn verify-authtoken
  "Verifies if TOKEN is valid. If APPID is given, it is checked if the
  token is issued for this app. If nil, don't care about which app the
  token is for. To verify the default app, use the special value
  `:default'. To be more clear, the following table shows the results
  depending on app (as extracted from the token) and appid:

  | app | appid | result         |
  |-----+-------+----------------|
  | x   | nil   | ok             |
  | ()  | nil   | ok             |
  | x   | x     | ok             |
  | ()  | :def  | ok             |
  | ()  | x     | if no pw for x |
  | x   | y     | ERR            |
  | x   | :def  | ERR            |
"
  [token & [appid]]
  (store/with-conn conn
    (let [[salt time login app sigthere] (s/split token #"\$")
          millis (try (Long/parseLong time) (catch Exception e -1))
          secrets (account/secret-get conn login (not-empty app))
          [_ sighere] (sign salt time login (not-empty app) secrets)
          checks [(not (account/account-locked? conn login))
                  (> (+ millis (config/get :token-validity))
                     (System/currentTimeMillis))
                  (or (nil? appid)
                      (= :default appid)
                      (account/app-enabled? conn login appid))
                  (or (nil? appid)
                      (= app appid)
                      (and (= appid :default) (empty? app))
                      (and (empty? app) (not (account/secret-exists? conn login appid))))
                  (compare-sigs sigthere sighere)]]
      (every? identity checks))))

(defn- authtoken-username-app
  "Given an authenticator token, return the username and app-id."
  [token]
  (if token
    (let [[salt time login app sigthere] (s/split token #"\$")]
      [login (if (empty? app) nil app)])))

(defn- assoc-if [m key value]
  (if value
    (assoc m key value)
    m))

(defn- authtoken-cookie [request & [make-token-fn]]
  (let [token-fn (or make-token-fn make-authtoken)
        login (get-in request [:params :login])
        app   (get-in request [:params :app])
        value (if login (token-fn login app))]
    (if value
      (fn [response]
        (let [cookies (:cookies response)]
          (assoc response
                 :cookies
                 (assoc cookies
                        (config/get :cookie-name)
                        (-> {:value value}
                          (assoc-if :path (config/get :cookie-path))
                          (assoc-if :domain (config/get :cookie-domain))
                          (assoc :secure (config/get :cookie-secure))
                          (assoc :http-only (config/get :cookie-http-only))
                          (assoc-if :max-age (config/get :cookie-max-age)))))))
      identity)))


(defn wrap-authtoken-cookie
  "Write a authenticator cookie into the response."
  [handler & [token-fn]]
  (fn [request]
    (let [wrap-cookie (authtoken-cookie request token-fn)
          response (handler request)]
      (wrap-cookie response))))

(defn wrap-logout
  "Add a cookie header to the response to make clients delete the
  cookie."
  [handler]
  (fn [request]
    (let [cookie-name (config/get :cookie-name)
          response (handler request)]
      (assoc response
             :cookies
             {cookie-name (-> {:value "" :max-age 1}
                              (assoc-if :path (config/get :cookie-path))
                              (assoc-if :domain (config/get :cookie-domain))
                              (assoc :secure (config/get :cookie-secure))
                              (assoc :http-only (config/get :cookie-http-only)))}))))

(defn wrap-verify-auth-cookie
  "Wrap the request and only proceed if it contains a valid
  authenticator token cookie. The username and app-id is extracted
  from the cookie and put into the params map of the request.

  The APPID parameter can be used to either specify an app that the
  cookie must match, use `:app-from-request' to take the app from the
  request (params or header 'x-shelter-app') or if nil it is not taken
  into account. If `:app-from-request' is specified, you must apply
  `wrap-keyword-params' middleware."
  [handler & [token-fn appid]]
  (fn [request]
    (let [cookie-name (config/get :cookie-name)
          token (get-in request [:cookies cookie-name :value])
          app   (if (= appid :app-from-request)
                  (or (get-in request [:params :app])
                      (get-in request [:params "app"])
                      (get-in request [:headers "x-shelter-app"]))
                  appid)
          [login loginapp] (if token (authtoken-username-app token))]
      (if (and token (verify-authtoken token app))
        (let [params (-> (:params request)
                         (assoc :login login)
                         (assoc :app loginapp))
              wrappedreq (assoc request :params params)]
          ((authtoken-cookie wrappedreq token-fn) (handler wrappedreq)))
        (let [failhandler (fn [request]
                            (-> (response "Unauthorized.")
                                (status 401)))]
          (if token
            ((wrap-logout failhandler) request)
            (failhandler request)))))))

(defn make-handler
  "Create an handler that extracts parameters given by the names
  PARAM-NAMES and applies them to the function CHECKFN. This function
  is expected to return `true' for success and `false' for
  failure. The response is created accordingly and the body is a map
  containing this information, too."
  [checkfn & param-names]
  (fn [request]
    (let [params (:params request)
          values (map #(% params) param-names)]
      (if (some nil? (drop-last values))
        {:status 400 :body {:success false :message "Invalid request."}}
        (if (apply checkfn values)
          {:status 200 :headers {} :body {:success true}}
          {:status 401 :headers {} :body {:success false :message "Request failed."}})))))

(defn make-logout-handler
  "A handler that responds with empty body and a cookie header to make
  the client delete the auth cookie."
  []
  (-> (fn [request]
        {:status 200 :headers {} :body {:success true}})
      json/wrap-json-response
      wrap-logout
      cookies/wrap-cookies))


(defn make-verify-cookie-handler
  "A simple ring handler that only checks if a cookie is a valid
  authenticator."
  []
  (-> (make-handler (fn [] true))
    (wrap-verify-auth-cookie nil :app-from-request)
    cookies/wrap-cookies
    kwp/wrap-keyword-params
    params/wrap-params
    json/wrap-json-params
    json/wrap-json-response))


(defn make-verify-form-handler
  "Create a handler that accepts requests with a form body containing
  an object with `login', `password' and optionally `app' values.

  The function VERIFY-FN is expected to use the three arguments to
  verify them against an account.

  The response contains a json body with success/failure state which
  is also encoded in the status code. A Set-Cookie header is added
  that contains an authenticator token. The token creation is
  delegated to the function TOKEN-FN. It takes the login and
  app (which maybe nil) and creates an authenticator token. By default
  `make-authtoken' is used."
  [verify-fn & [token-fn]]
  (-> (make-handler verify-fn :login :password :app)
    (wrap-authtoken-cookie token-fn)
    kwp/wrap-keyword-params
    params/wrap-params
    cookies/wrap-cookies
    json/wrap-json-response))

(defn make-verify-json-handler
  "Create a handler that accepts request with a json body containing
  an object with `login', `password' and optionally `app' values.

  The function VERIFY-FN is expected to use the three arguments to
  verify them against an account. A Set-Cookie header is added that
  contains an authenticator token. The token creation is delegated to
  the function TOKEN-FN. It takes the login and app (which maybe nil)
  and creates an authenticator token. By default `make-authtoken' is
  used."
  [verify-fn & [token-fn]]
  (-> (make-handler verify-fn :login :password :app)
    (wrap-authtoken-cookie token-fn)
    kwp/wrap-keyword-params
    json/wrap-json-params
    cookies/wrap-cookies
    json/wrap-json-response))

(defn make-setpassword-form-handler
  "Create a handler that accepts requests containing form data to set
  a new password. It must specify `login', `password' and
  `newpassword' to first authenticate and then set a new password. The
  optional `app' parameter can be used to apply this to a specific
  app.

  The request must contain an authenticator cookie to obtain the login
  from. A new Set-Cookie header is added that contains a new
  authenticator token. The token creation is delegated to the function
  TOKEN-FN. It takes the login and app (which maybe nil) and creates
  an authenticator token. By default `make-authtoken' is used."
  [setpass-fn & [token-fn]]
  (-> (make-handler setpass-fn :login :password :newpassword :app)
    kwp/wrap-keyword-params
    (wrap-verify-auth-cookie token-fn)
    cookies/wrap-cookies
    params/wrap-params
    json/wrap-json-response))

(defn make-setpassword-json-handler
  "Creates a handler that accepts requests containing json data to set
  a new password. It must specify `login', `password' and
  `newpassword' to first authenticate and then set a new password. The
  optional `app' parameter can be used to apply this to a specific
  app.

  The request must contain an authenticator cookie to obtain the login
  from. A new Set-Cookie header is added that contains a new
  authenticator token. The token creation is delegated to the function
  TOKEN-FN. It takes the login and app (which maybe nil) and creates
  an authenticator token. By default `make-authtoken' is used."
  [setpass-fn & [token-fn]]
  (-> (make-handler setpass-fn :login :password :newpassword :app)
    kwp/wrap-keyword-params
    (wrap-verify-auth-cookie token-fn)
    cookies/wrap-cookies
    json/wrap-json-params
    json/wrap-json-response))

(defn make-setpassword-force-form-handler
  "Create a handler that accepts requests containing form data to set
  a new password. It must specify `login' and `newpassword'. The
  optional `app' parameter can be used to apply this to a specific
  app. Note that this is dangerous as no authentication happens before
  and a new password for any user can be set."
  [setpass-fn]
  (-> (make-handler setpass-fn :login :newpassword :app)
    kwp/wrap-keyword-params
    params/wrap-params
    json/wrap-json-response))

(defn make-list-apps-json-handler
  "Create a ring handler that returns a list of applications enabled
  for the current user."
  []
  (let [handler (fn [request]
                  (let [login (:login (:params request))]
                    (if login
                      (store/with-conn conn
                        (if (account/account-exists? conn login)
                          {:status 200 :body {:success true :apps (account/app-list-enabled conn login)}}
                          {:status 403 :body {:success false }}))
                      {:status 403 :body {:success false}})))]
    (-> handler
      wrap-verify-auth-cookie
      cookies/wrap-cookies
      kwp/wrap-keyword-params
      json/wrap-json-response)))

(defn make-account-exists-handler
  "Create a ring handler that checks whether an account exists. If a
  `app' parameter is also specified, the account must be enabled for
  app."
  [account-exists-fn]
  (-> (make-handler account-exists-fn :login :app)
    kwp/wrap-keyword-params
    params/wrap-params
    json/wrap-json-response))
