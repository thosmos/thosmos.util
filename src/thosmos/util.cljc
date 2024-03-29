(ns thosmos.util
  #?(:cljs (:require-macros [thosmos.util]))
  (:require
    [clojure.core.rrb-vector :as fv]
    #?(:clj  [clojure.pprint :refer [pprint]]
       :cljs [cljs.pprint :refer [pprint]])
    #?(:clj [clojure.spec.alpha :as s])
    #?(:clj [clojure.java.io :as io])
    [clojure.string :refer [blank? join split]]
    #?(:cljs [goog.Uri :as uri])
    [#?(:cljs cljs.core.async
        :clj clojure.core.async) :refer [<! put! chan timeout close!]]
    [clojure.walk :as walk])
  #?(:clj (:import (java.nio.file FileSystems)
            (java.util Date)
            [java.time ZonedDateTime Instant ZoneId])))


(defn if-pos? [v]
  (if (and v (pos? v)) v))

#?(:clj (defmacro functionize [macro]
          `(fn [& args#] (eval (cons '~macro args#)))))

(defn merge-tree [a b]
  (if (and (map? a) (map? b))
    (merge-with #(merge-tree %1 %2) a b)
    b))

(defn sort-maps-by
  "Sort a sequence of maps (ms) on multiple keys (ks)"
  [ms ks]
  (sort-by #(vec (map % ks)) ms))
;; sort list of maps by multiple values
;;(sort-by (juxt :a :b) [{:a 1 :b 3} {:a 1 :b 2} {:a 2 :b 1}])
;;=> [{:a 1 :b 2} {:a 1 :b 3} {:a 2 :b 1}]

(defn nest-by
  [ks coll]
  (let [keyfn (apply juxt ks)]
    (reduce (fn [m x] (assoc-in m (keyfn x) x)) {} coll)))

;; Create lookup maps via a specific key
(defn index-by [key-fn coll]
  (into {} (map (juxt key-fn identity) coll)))
;; #'user/index-by
;(index-by :id [{:id 1 :name "foo"}
;               {:id 2 :name "bar"}
;               {:id 3 :name "baz"}])
;;=> {1 {:name "foo", :id 1},
;;    2 {:name "bar", :id 2},
;;    3 {:name "baz", :id 3}}

;; from https://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
#?(:clj
   (defn eager-mapcat
     [f coll]
     (lazy-seq
       (if (not-empty coll)
         (concat
           (f (first coll))
           (eager-mapcat f (rest coll)))))))

;; http://www.markhneedham.com/blog/2014/04/06/clojure-not-so-lazy-sequences-a-k-a-chunking-behaviour/
#?(:clj
   (defn unchunk [s]
     (when (seq s)
       (lazy-seq
         (cons (first s)
           (unchunk (next s)))))))

#?(:clj
   (defn format-instant
     ([instant] (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") instant))
     ([instant format] (.format (java.text.SimpleDateFormat. format) instant))))


#?(:clj
   (defn zoned-datetime-from-instant [ins]
     (-> ins
       (.getTime)
       (/ 1000)
       Instant/ofEpochSecond
       (ZonedDateTime/ofInstant
         (ZoneId/of "America/Los_Angeles")))))

(defn vec-remove [pos coll]
  (fv/catvec (fv/subvec coll 0 pos) (fv/subvec coll (inc pos) (count coll))))

(defn vec-add [pos thing coll]
  (fv/catvec (fv/subvec coll 0 pos) (fv/vector thing) (fv/subvec coll pos (count coll))))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  #?(:clj [^Double precision ^Double d]
     :cljs [precision d])
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
  (conj (into [] coll) val))
(def conjv! conjv)

;(defn ppspit [f content]
;  (with-open [^java.io.Writer w (apply clojure.java.io/writer f nil)]
;    (pprint content w)))

#?(:clj
   (defn spitpp [f foo] (spit f (with-out-str (pprint foo)))))

(defn ppstr [foo]
  (with-out-str (pprint foo)))

(defn ns-kw [ns nm]
  (if (and ns (not= ns ""))
    (keyword (name ns) (name nm))
    (keyword (name nm))))

#?(:clj
   (defn check [type data]
     (if (s/valid? type data)
       true
       (throw (AssertionError. (s/explain type data))))))

#?(:clj
   (defn match-files [^java.io.File f pattern]
     (.matches (.getPathMatcher (FileSystems/getDefault) (str "glob:*" pattern)) (.toPath f))))

#?(:clj
   (defn walk-directory [^String dir pattern]
     (let [fdir ^java.io.File (io/file dir)]
       (map #(.getPath ^java.io.File %)
         (filter #(match-files % pattern) (.listFiles fdir))))))

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

#?(:clj (defn parse-date [date-str]
          (try
            (Date/parse date-str)
            (catch Exception _))))

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


;(defmacro tassoc
;  "assoc[iate]. When applied to a map, returns a new map of the
;   same (hashed/sorted) type, that contains the mapping of key(s) to
;   val(s). When applied to a vector, returns a new vector that
;   contains val at index. Note - index must be <= (count vector).
;   A key of `:=` and v of `foo` will instead set the key to `(name foo)`"
;  ([coll k v]
;   (let [k# (if (= := k) (keyword v) v)]
;     `(assoc ~coll ~k# ~v)))
;  ([coll k v & kvs]
;   (let [ret (tassoc coll k v)]
;     (if kvs
;       (tassoc ret (first kvs) (second kvs) (nnext kvs))
;       ret))))

;(defn tassoc*
;  ([coll k v]
;   (let [k (if (= := k) (keyword v) v)]
;     (assoc coll k v)))
;  ([coll k v & kvs]
;   (let [ret (tassoc coll k v)]
;     (if kvs
;       (recur ret (first kvs) (second kvs) (nnext kvs))
;       ret))))

(defmacro ->
  "Threads the expr through the forms. Inserts x as the second item
  in the first form, making a list of it if it is a lambda or not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form     (first forms)
            threaded (if (and (seq? form) (not (#{'fn 'fn*} (first form))))
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the last item
  in the first form, making a list of it if it is a lambda or not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form     (first forms)
            threaded (if (and (seq? form) (not (#{'fn 'fn*} (first form))))
                       (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))


(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn <<< [f & args]
  (let [c (chan)]
    (apply f (concat args [(fn [x]
                             (if #?(:cljs (or (nil? x) (undefined? x))
                                    :clj (nil? x))
                               (close! c)
                               (put! c x)))]))
    c))

(defn if-pos [v]
  (if (and v (pos? v)) v))

#?(:cljs (defn parse-url
           "Parse `url` into a hash map."
           [url]
           (if-not (blank? url)
             (let [uri        (uri/parse url)
                   query-data (.getQueryData uri)]
               {:scheme       (keyword (.getScheme uri))
                :server-name  (.getDomain uri)
                :server-port  (if-pos (.getPort uri))
                :uri          (.getPath uri)
                :query-string (if-not (.isEmpty query-data)
                                (str query-data))
                :query-params :use-cljs-http}))))


