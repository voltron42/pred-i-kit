(ns pred-i-kit.core
  (:require [clojure.spec.alpha :as s]
            [clj-time.core :as t])
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentHashSet)
           (org.joda.time DateTime)))

(defn instance-of [classes]
  (if-not (coll? classes)
    (partial instance? classes)
    (fn [value]
      (some #(instance? % value) classes))))

(defn datetime? []
  (instance-of DateTime))


(defmulti matches?
          (fn [bound]
            (type bound)))

(defmethod matches? :default [bound]
  (partial = bound))

(defmethod matches? PersistentHashSet [bound]
  (partial contains? bound))

(defmethod matches? Pattern [regex]
  (fn [value]
    (let [matches (re-matches regex value)
          matches (if (coll? matches) matches [matches])]
      (contains? (set matches) value))))

(defn named-as
  ([my-namespace my-name]
   (fn [value]
     (try
       (and ((matches? my-namespace) (namespace value)) ((matches? my-name) (name value)))
       (catch Throwable t
         false))))
  ([my-name]
   (fn [value]
     (try
       (and (nil? (namespace value)) ((matches? my-name) (name value)))
       (catch Throwable t
         false)))))



(defn exact-count [bound]
  #(->> % count (= bound)))

(s/fdef exact-count
        :args (s/cat :bound int?)
        :ret (s/fspec :args (s/cat :value (s/or :c coll? :s string?))
                      :ret boolean?))

(defn min-count [bound]
  #(->> % count (<= bound)))

(s/fdef min-count
        :args (s/cat :bound int?)
        :ret (s/fspec :args (s/cat :value (s/or :c coll? :s string?))
                      :ret boolean?))

(defn max-count [bound]
  #(->> % count (>= bound)))

(s/fdef max-count
        :args (s/cat :bound int?)
        :ret (s/fspec :args (s/cat :value (s/or :c coll? :s string?))
                      :ret boolean?))

(defn count-between [low high]
  #(and ((min-count low) %)
        ((max-count high) %)))

(s/fdef count-between
        :args (s/and (s/cat :low int? :high int?)
                     #(< (:low %) (:high %)))
        :ret (s/fspec :args (s/cat :value (s/or :c coll? :s string?))
                      :ret boolean?))

(defn count-mod [dividend remainder]
  #(-> % count (mod dividend) (= remainder)))

(s/fdef count-mod
        :args (s/and (s/cat :dividend int? :remainder int?)
                     #(> (:dividend %) (:remainder %)))
        :ret (s/fspec :args (s/cat :value (s/or :c coll? :s string?))
                      :ret boolean?))

(defn exact [bound] (partial = bound))

(s/fdef exact
        :args (s/cat :bound number?)
        :ret (s/fspec :args (s/cat :value number?)
                      :ret boolean?))

(defn minimum [bound] (partial <= bound))

(s/fdef minimum
        :args (s/cat :bound number?)
        :ret (s/fspec :args (s/cat :value number?)
                      :ret boolean?))

(defn maximum [bound] (partial >= bound))

(s/fdef maximum
        :args (s/cat :bound number?)
        :ret (s/fspec :args (s/cat :value number?)
                      :ret boolean?))

(defn between [low high]
  #(and ((minimum low) %)
        ((maximum high) %)))

(s/fdef between
        :args (s/and (s/cat :low number? :high number?)
                     #(< (:low %) (:high %)))
        :ret (s/fspec :args (s/cat :value number?)
                      :ret boolean?))

(defn exact-date [bound]
  (partial t/equal? bound))

(s/fdef exact-date
        :args (s/cat :bound datetime?)
        :ret (s/fspec :args (s/cat :bound datetime?)
                      :ret boolean?))

(defn is-date-after? [bound]
  (partial t/after? bound))

(s/fdef is-date-after?
        :args (s/cat :bound datetime?)
        :ret (s/fspec :args (s/cat :bound datetime?)
                      :ret boolean?))

(defn is-date-before? [bound]
  (partial t/before? bound))

(s/fdef is-date-before?
        :args (s/cat :bound datetime?)
        :ret (s/fspec :args (s/cat :bound datetime?)
                      :ret boolean?))

(defn is-date-between? [low high])

(s/fdef is-date-between?
        :args (s/and (s/cat :low datetime? :high datetime?)
                     #(t/before? (:low %) (:high %)))
        :ret (s/fspec :args (s/cat :bound datetime?)
                      :ret boolean?))
