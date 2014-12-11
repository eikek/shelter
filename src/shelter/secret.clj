(ns shelter.secret
  (:require [shelter.config :as config])
  (:import [org.mindrot.jbcrypt BCrypt]
           [com.lambdaworks.crypto SCryptUtil]))

(config/set {:password-chars "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ123456789-_+*?"
             :password-length 15
             :scrypt {:n 15 :r 12 :p 1}
             :bcrypt {:salt_rounds 11}
             :default-hash :bcrypt})


;; my jvm crashed when using native lib, did not crash with scala
(System/setProperty "com.lambdaworks.jni.loader" "nil")

(defn make-password
  "Create a hashed version of a given plain text password. Return a
  secret, which is a map holding the hashed data with additional
  properties."
  [plain & more]
  (let [moremap (apply hash-map more)
        hash (or (:hash moremap) (config/get :default-hash))
        opts (conj (config/get hash) moremap)]
    (cond
     (= hash :scrypt) {:type :password
                       :hash :scypt
                       :data (SCryptUtil/scrypt plain
                                                (int (Math/pow 2 (opts :n)))
                                                (:r opts)
                                                (:p opts))}
     :else
       {:type :password
        :hash :bcrypt
        :data (BCrypt/hashpw plain (BCrypt/gensalt (:salt_rounds opts)))})))

(defn random-string
  "Create a new string of length LEN using characters from CHARS. Use
  some default if not specififed."
  [& [len chars]]
  (let [alpha (or chars (config/get :password-chars))
        length (or len (config/get :password-length))]
    (clojure.string/join
     (map #(.charAt alpha %)
          (take length (repeatedly #(int (rand (.length alpha)))))))))

(defn verify-password
  "Verify if a plain password matches a given secret."
  [secret plain]
  (let [type (:type secret)
        data (:data secret)]
    (if (= type :scrypt)
      (SCryptUtil/check plain data)
      (BCrypt/checkpw plain data))))

(defn verify
  "Verify credentials DATA against the given SECRETS."
  [secrets data]
  (map (fn [secret]
         [(:type secret)
          (verify-password secret data)])
       secrets))
