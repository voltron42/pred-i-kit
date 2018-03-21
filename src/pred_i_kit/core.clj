(ns pred-i-kit.core
  (:import (java.util.regex Pattern)
           (clojure.lang PersistentHashSet)))

(defn- comparing [compare value-fn bound]
  (fn [value]
    (try
      (compare bound (value-fn value))
      (catch Throwable _ false))))

(defn exact-count [bound]
  (comparing = count bound))

(defn min-count [bound]
  (comparing <= count bound))

(defn max-count [bound]
  (comparing >= count bound))

(defn exact-value [bound]
  (comparing = identity bound))

(defn min-value [bound]
  (comparing <= identity bound))

(defn max-value [bound]
  (comparing >= identity bound))

(defn- check-range [value-fn]
  (fn
    ([low high]
     (comparing
       #(and (<= low %2) (>= high %2))
       value-fn
       nil))
    ([low high step]
     (comparing
       #(and (<= low %2)
             (>= high %2)
             (= (mod %2 step) (mod low step)))
       value-fn
       nil))))

(def count-range (check-range count))

(def value-range (check-range identity))

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

