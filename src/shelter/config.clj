;;; config.clj -- part of shelter

;; This module defines the shelter configuration map and functions to
;; modify it.

(ns shelter.config
  (:refer-clojure :exclude [set get])
  (:require [clojure.pprint :as pprint]))

(defonce config (atom {}))

(defn swap [f]
  (swap! config f))

(defn set [cfg]
  (swap! config
         (fn [values]
           (conj values cfg))))

(defn get [key]
  (clojure.core/get @config key))

(defn show []
  (pprint/pprint @config))

(defn watch [key f]
  (add-watch config key
             (fn [key ref old new]
               (f key old new))))
