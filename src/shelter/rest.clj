;;; rest.clj -- rest inferface to shelter.

(ns shelter.rest
  (:require
   [shelter.config :as config]
   [shelter.account :as account]
   [shelter.secret :as secret]
   [compojure.core :as web]
   [compojure.route :as web-route]
   [ring.adapter.jetty :as jetty]))


;; default values
(config/set {:rest-port 9919})

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
  "Add multiple routes at once. Route defintions are given like this:

(add-routes
 {:key :verify
  :route (POST \"/verify\" [name pass] (response \"OK\"))}) "
  [& route-defs]
  (doseq [rdef route-defs]
    (let [{:keys [key route append]} rdef]
      (register-route key route append))))

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
             (do (println "Stoppting rest server…")
                 (.stop server))))

(defn- start-jetty []
  (println "Starting rest server…")
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
