(ns thosmos.datomic)
;  (:require
;    [datomic.api :as d]
;    [datomic.db]
;    [datomic.function]
;    [datomic.codec]
;    [clojure.edn :as edn]
;    [clojure.java.io :as io]))
;
;(defn load-resource [path]
;  (load-file (.getPath (io/resource path))))
;
;(defn slurp-resource [path]
;  (try (slurp (io/resource path)) (catch Exception ex nil)))
;
;(def slurp-resource-memo
;  (memoize slurp-resource))
;
;(declare readers)
;
;(defn load-edn [path]
;  (edn/read-string {:readers readers} (slurp-resource path)))
;
;;; the idea here is that you can load an edn file that has #load-edn tags in it which loads in other edns ... meta
;(def readers {'thosmos.datomic/load-edn       load-edn
;              'thosmos.datomic/slurp-resource slurp-resource
;              'db/id                          datomic.db/id-literal
;              'db/fn                          datomic.function/construct
;              'base64                         datomic.codec/base-64-literal})
;
;;(defn dec-txd [action-id]
;;  [:dec action-id :action/pledges 1])
;;
;;(defn inc-pledges-txd [action-id]
;;  [:inc action-id :action/pledges 1])
;
;(defn retract-entity-txd [entity]
;  [:db.fn/retractEntity (:db/id entity)])
;
;(defn retract-pledges [uid cid]
;  [:retract-pledges uid cid])
;
;(defn entity [db id]
;  (d/entity db id))
;
;(defn touch
;  "takes 'entity ids' results from a query
;    e.g. '[272678883689461 272678883689462 272678883689459 272678883689457]'"
;  ([db coll]
;   (let [e (partial entity db)]
;     (map #(-> % e d/touch) coll)))
;  ([db pattern coll]
;   (map #(d/pull db pattern %) coll)))
;
;(defn find-entities
;  "find all entities with a given attribute and optional value and pattern"
;  ([db attr]
;   (d/q '[:find [?e ...]
;          :in $ ?attr
;          :where [?e ?attr]]
;     db attr))
;  ([db attr value]
;   (d/q '[:find [?e ...]
;          :in $ ?attr ?value
;          :where [?e ?attr ?value]]
;     db attr value))
;  ([db pattern attr value]
;   (d/q '[:find [(pull ?e q) ...]
;          :in $ ?attr ?value q
;          :where [?e ?attr ?value]]
;     db attr value pattern)))
;
;(defn pull-entity
;  "find one entity with a given attribute and value and optional pattern"
;  ([db attr val]
;   (pull-entity db '[*] attr val))
;  ([db attr val pattern]
;   (d/q '[:find (pull ?e patt) .
;          :in $ ?attr ?val patt
;          :where [?e ?attr ?val]]
;     db attr val pattern)))
;
;(defn get-value
;  "find one value of one attribute given a dbid"
;  ([db id attr]
;   (d/q '[:find ?v .
;          :in $ ?e ?a
;          :where [?e ?a ?v]]
;     db id attr)))
;
;(defn retract-entities [conn es]
;  (map #(d/transact conn [[:db.fn/retractEntity %]]) es))
;
;(defn retract-refs [conn eid attr refs]
;  (let [rtx (vec (for [ref refs]
;                   [:db/retract eid attr (:db/id ref)]))]
;    (d/transact conn rtx)))
;
;(defn es->retract-tx [es]
;  (vec (map #(do [:db.fn/retractEntity %]) es)))
;
;(defn tx->datums [{:as tx :keys [db-after tx-data]}]
;  (vec (d/q '[:find ?e ?aname ?v ?added
;              :in $ [[?e ?a ?v _ ?added]]
;              :where
;              [?a :db/ident ?aname]]
;         db-after tx-data)))