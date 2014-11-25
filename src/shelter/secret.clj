(ns shelter.secret
  (:import [org.mindrot.jbcrypt BCrypt]
           [com.lambdaworks.crypto SCryptUtil]))


;; my jvm crashed when using native lib, did not crash with scala
(System/setProperty "com.lambdaworks.jni.loader" "nil")

(defn make-password
  "Create a hashed version of a given plain text password. Return a
  secret, which is a map holding the hashed data with additional
  properties."
  [plain & more]
  (let [opts (apply hash-map more)
        hash (:hash opts)]
    (cond
     (= hash :scrypt)
     (let [n (int (Math/pow 2 (or (opts :n) 15)))
           r (or (:r opts) 12)
           p (or (:p opts) 1)]
       {:type :password
        :hash :scypt
        :data (SCryptUtil/scrypt plain n r p)})
     :else
     (let [rounds (or (:salt_rounds opts) 12)]
       {:type :password
        :hash :bcrypt
        :data (BCrypt/hashpw plain (BCrypt/gensalt rounds))}))))

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
