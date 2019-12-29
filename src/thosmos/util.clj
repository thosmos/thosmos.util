(ns thosmos.util
  (:require [clojure.core.rrb-vector :as fv]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [clojure.walk :as walk])
  (:import (java.nio.file FileSystems)
           (java.util Date)
           [java.time ZonedDateTime Instant ZoneId]))

(defmacro functionize [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defn merge-tree [a b]
  (if (and (map? a) (map? b))
    (merge-with #(merge-tree %1 %2) a b)
    b))

(defn sort-maps-by
  "Sort a sequence of maps (ms) on multiple keys (ks)"
  [ms ks]
  (sort-by #(vec (map % ks)) ms))

;; from https://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
(defn eager-mapcat
  [f coll]
  (lazy-seq
    (if (not-empty coll)
      (concat
        (f (first coll))
        (eager-mapcat f (rest coll))))))

;; http://www.markhneedham.com/blog/2014/04/06/clojure-not-so-lazy-sequences-a-k-a-chunking-behaviour/
(defn unchunk [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
        (unchunk (next s))))))

(defn format-instant
  ([instant] (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") instant))
  ([instant format] (.format (java.text.SimpleDateFormat. format) instant)))

(defn zoned-datetime-from-instant [ins]
  (-> ins
    (.getTime)
    (/ 1000)
    Instant/ofEpochSecond
    (ZonedDateTime/ofInstant
      (ZoneId/of "America/Los_Angeles"))))

(defn vec-remove [pos coll]
  (fv/catvec (fv/subvec coll 0 pos) (fv/subvec coll (inc pos) (count coll))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [^Double precision ^Double d]
  (let [factor (Math/pow 10.0 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn square [n] (* n n))

(defn mean [a] (/ (reduce + a) (count a)))

(defn std-dev
  [a]
  (let [mn (mean a)]
    (Math/sqrt
      (/ (reduce #(+ %1 (square (- %2 mn))) 0 a)
        (dec (count a))))))

(defn percent-prec [avg stddev]
  (* (/ stddev avg) 100))

;; if it's nil, make it 1
(defn inc! [val]
  (inc (or val 0)))

;; if it's nil, make it a []
(defn conjv [coll val]
  (println "conjv DEPRECRATED, use conjv!")
  (conj (or coll []) val))

(defn conjv! [coll val]
  (conj (or coll []) val))

;(defn ppspit [f content]
;  (with-open [^java.io.Writer w (apply clojure.java.io/writer f nil)]
;    (pprint content w)))

(defn spitpp [f foo] (spit f (with-out-str (pprint foo))))

(defn ppstr [foo]
  (with-out-str (pprint foo)))

(defn ns-kw [ns nm]
  (keyword (name ns) (name nm)))

(defn check [type data]
  (if (s/valid? type data)
    true
    (throw (AssertionError. (s/explain type data)))))

(defn match-files [^java.io.File f pattern]
  (.matches (.getPathMatcher (FileSystems/getDefault) (str "glob:*" pattern)) (.toPath f)))

(defn walk-directory [^String dir pattern]
  (let [fdir ^java.io.File (io/file dir)]
    (map #(.getPath ^java.io.File %)
      (filter #(match-files % pattern) (.listFiles fdir)))))

(defn walk-remove-ns [map]
  (walk/postwalk
    (fn [form]
      (if (map? form)
        (reduce-kv (fn [acc k v] (assoc acc (keyword (name k)) v)) {} form)
        form))
    map))

(defn walk-modify-k-vals [m k f]
  (walk/postwalk
    (fn [form]
      (if (map? form)
        (reduce-kv
          (fn [acc ky v]
            (assoc acc ky (if (= ky k) (f v) v))) {} form)
        form))
    m))

(defn limit-fn
  "Collection pagination mimicking the MySql LIMIT"
  ([quantity coll]
   (limit-fn quantity 0 coll))
  ([quantity start-from coll]
   (let [quantity   (or quantity 20)
         start-from (or start-from 0)]
     (take quantity (drop start-from coll)))))

(defn parse-date [date-str]
  (try
    (Date/parse date-str)
    (catch Exception _)))

;(defn parse-to-local-time [date-str]
;  (when
;    (and date-str (not= date-str ""))
;    (->
;      (parse-date date-str)
;      ;(Date/parse "12/30/1899 10:10:00")
;      c/from-long
;      (t/from-time-zone (t/time-zone-for-id "GMT"))
;      (t/to-time-zone (t/time-zone-for-id "US/Pacific-New"))
;      (l/format-local-time :hour-minute)
;      )
;    ))