;;; rest.clj -- rest inferface to shelter.

(ns shelter.rest
  (:require
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


(defn make-authtoken
  "Create an authenticator for the given login and app."
  [login & [app]]
  (let [salt (secret/random-string 12)
        data (str salt "$" (System/currentTimeMillis) "$" login "$" app)
        secrets (store/with-conn conn
                  (if (not (account/account-locked? conn login))
                    (sort (map :data (account/secret-get conn login app)))))]
    (if (not-empty secrets)
      (let [sig (secret/sign (str (config/get :cookie-secret) secrets) data)]
        (str data "$" sig)))))

(defn verify-authtoken
  [token]
  (let [[salt time login app sigthere] (clojure.string/split token #"\$")
        data (str salt "$" time "$" login "$" app)
        secrets (store/with-conn conn
                  (if (not (account/account-locked? conn login))
                    (sort (map :data (account/secret-get conn login (if (empty? app) nil app))))))
        sighere (secret/sign (str (config/get :cookie-secret) secrets) data)]
    (if (= sigthere sighere)
      (> (+ (Long/parseLong time) (config/get :token-validity))
         (System/currentTimeMillis))
      false)))

(defn- authtoken-username-app
  "Given an authenticator token, return the username and app-id."
  [token]
  (let [[salt time login app sigthere] (clojure.string/split token #"\$")]
    [login (if (empty? app) nil app)]))

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
    (let [wrap-cookie (authtoken-cookie request token-fn)]
      (-> request
        handler
        wrap-cookie))))

(defn wrap-verify-auth-cookie
  "Wrap the request and only proceed if it contains a valid
  authenticator token cookie. The username and app-id is extracted
  from the cookie and put into the params map of the request."
  [handler & [token-fn]]
  (fn [request]
    (let [token (get-in request [:cookies (config/get :cookie-name) :value])]
      (if (and token (verify-authtoken token))
        (let [userapp (authtoken-username-app token)
              params (-> (:params request)
                       (assoc :login (first userapp))
                       (assoc :app (second userapp)))
              wrappedreq (assoc request :params params)]
          ((authtoken-cookie wrappedreq token-fn) (handler wrappedreq)))
        (-> (response "Not authenticated.")
          (status 403)
          (assoc :cookies { (config/get :cookie-name) {:value "delete" :max-age 1}}))))))

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

(defn make-verify-cookie-handler
  "A simple ring handler that only checks if a cookie is a valid
  authenticator."
  []
  (-> (make-handler (fn [] true))
    wrap-verify-auth-cookie
    cookies/wrap-cookies
    kwp/wrap-keyword-params
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
    wrap-authtoken-cookie
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
