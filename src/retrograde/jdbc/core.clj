(ns retrograde.jdbc.core
  (:require [clojure.edn :as edn]
            [retrograde.core :as rg]
            [retrograde.time :refer [->epoch ->instant]]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

;;; JDBC Helpers

(defn- execute-one!
  ([ctx query]
   (execute-one! ctx query {}))
  ([ctx query opts]
   (jdbc/execute-one! (.conn ctx)
                      (sql/format query (.sql-opts ctx))
                      (merge {:builder-fn rs/as-unqualified-kebab-maps} opts))))

;;; Hashing

(defn- ->hash
  [memory]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        bytes (.digest digest (.getBytes (pr-str memory) "UTF-8"))]
    (->> bytes
         (map #(format "%02x" %))
         (apply str))))

;;; Queries

(defmulti insert-mem-rep-if-absent-query
  (fn [dbtype _relation _id _data] dbtype))

(defmethod insert-mem-rep-if-absent-query :default
  [_dbtype _relation _id _data]
  (throw (UnsupportedOperationException. "insert-mem-rep-if-absent-query not implemented")))

(defn- select-mem-rep-query
  [{:keys [mem-rep]} id]
  {:select [[:id] [:data]]
   :from mem-rep
   :where [:= :id id]})

(defn- insert-engram-query
  [{:keys [engram]} k mem-rep-id expires-at]
  {:insert-into engram
   :values [{:key k
             :created (->epoch (java.time.Instant/now))
             :mem-rep-id mem-rep-id
             :expires-at (->epoch expires-at)
             :decay-level 0}]})

(defn- update-engram-query
  [{:keys [engram]} record]
  {:update engram
   :set (-> record
            (dissoc :id :created)
            (update :expires-at ->epoch))
   :where [:= :id (:id record)]})

(defn- apply-engram-filters
  [query {:keys [filter order]} & {:keys [alias]}]
  (letfn [(add-where-clause [query condition]
            (update query :where #(if % [:and % condition] condition)))]
    (let [prefix (if alias (str alias ".") "")
          kw (fn [s] (keyword (str prefix s)))]
      (cond-> query
        (:id filter)
        (add-where-clause [:in (kw "id") (:id filter)])

        (:key filter)
        (add-where-clause [:in (kw "key") (:key filter)])

        (:expires-until filter)
        (add-where-clause [:<= (kw "expires-at") (->epoch (:expires-until filter))])

        (:expires-after filter)
        (add-where-clause [:>= (kw "expires-at") (->epoch (:expires-after filter))])

        order
        (assoc :order-by order)))))

(defn- select-engram-query
  ([{:keys [engram]} engram-id]
   (select-engram-query {:engram engram} engram-id {}))
  ([{:keys [engram]} engram-id options]
   (apply-engram-filters
    {:select [:*]
     :from engram
     :where [:= :id engram-id]}
    options)))

(defn- select-engram-with-data-query
  ([{:keys [engram mem-rep]} engram-id]
   (select-engram-with-data-query {:engram engram :mem-rep mem-rep} engram-id {}))
  ([{:keys [engram mem-rep]} engram-id options]
   (apply-engram-filters
    {:select [[:e.id :id]
              [:e.key :key]
              [:e.created :created]
              [:e.expires-at :expires-at]
              [:e.decay-level :decay-level]
              [:m.data :data]]
     :from [[engram :e]]
     :join [[mem-rep :m] [:= :e.mem-rep-id :m.id]]
     :where [:= :e.id engram-id]}
    options
    :alias "e")))

(defn- select-engrams-query
  [{:keys [engram]} {:keys [filter order]}]
  (apply-engram-filters
   {:select [:*]
    :from engram}
   {:filter filter :order order}))

(defn- select-engrams-with-data-query
  [{:keys [engram mem-rep]} options]
  (apply-engram-filters
   {:select [[:e.id :id]
             [:e.key :key]
             [:e.created :created]
             [:e.expires-at :expires-at]
             [:e.decay-level :decay-level]
             [:m.data :data]]
    :from [[engram :e]]
    :join [[mem-rep :m] [:= :e.mem-rep-id :m.id]]}
   options
   :alias "e"))

;;; Writer

(defn- row->record
  [m]
  (when (seq m)
    (-> m
        (update :created ->instant)
        (update :expires-at ->instant))))

(deftype Writer [conn dbtype sql-opts tables]
  java.io.Closeable
  (close [_]
    (.close conn))

  rg/Closed
  (closed? [_]
    (.isClosed conn))

  rg/Writer
  (commit! [_]
    (.commit conn))

  (rg/rollback! [_]
    (.rollback conn))

  (delete-all! [writer]
    (execute-one! writer {:delete-from (:engram tables)})
    (execute-one! writer {:delete-from (:mem-rep tables)})
    nil)

  (put-mem-rep! [writer data]
    (let [id (->hash data)]
      (execute-one!
       writer
       (insert-mem-rep-if-absent-query dbtype (:mem-rep tables) id (pr-str data)))
      id))

  (read-mem-rep [writer mem-rep-id]
    (let [result (execute-one! writer (select-mem-rep-query tables mem-rep-id))]
      (when (seq result)
        (-> result :data edn/read-string))))

  (create-record! [writer k mem-rep-id expires-at]
    (let [id (-> (execute-one! writer
                               (insert-engram-query tables k mem-rep-id expires-at)
                               {:return-keys [:id]})
                 vals
                 first)]
      (rg/read-record writer id)))

  (update-record! [writer record]
    (execute-one! writer (update-engram-query tables record))
    (rg/read-record writer (:id record)))

  (read-record [writer engram-id]
    (-> (execute-one! writer (select-engram-query tables engram-id))
        row->record))

  (reduce-records [_ f init query]
    (let [q (sql/format (select-engrams-query tables query) sql-opts)
          r (jdbc/plan conn q {:builder-fn rs/as-unqualified-kebab-maps})]
      (reduce (fn [acc row]
                (f acc (row->record row)))
              init
              r))))

;;; Reader

(defn- row->engram
  [m]
  (when (seq m)
    (-> m
        (update :data edn/read-string)
        (update :created ->instant)
        (update :expires-at ->instant))))

(deftype Reader [conn sql-opts tables]
  java.io.Closeable
  (close [_]
    (.close conn))

  rg/Closed
  (closed? [_]
    (.isClosed conn))

  rg/Reader
  (read-engram [reader engram-id]
    (let [result (execute-one! reader
                               (select-engram-with-data-query tables engram-id)
                               {:builder-fn rs/as-unqualified-kebab-maps})]
      (row->engram result)))

  (stream-engrams [_ xform f init query]
    (let [q (sql/format (select-engrams-with-data-query tables query) sql-opts)
          r (jdbc/plan conn q {:builder-fn rs/as-unqualified-kebab-maps})]
      (transduce
       (comp (map row->engram) xform)
       f
       init
       r))))

;;; DDL

(defmulti create-tables
  "Create database tables based on dbtype."
  (fn [ds _opts] (-> ds :dbtype)))

;;; Store

(deftype Store [ds opts]
  rg/Store
  (init [_]
    (with-open [conn (jdbc/get-connection ds {:auto-commit false})]
      (try
        (run! (partial jdbc/execute-one! conn) (create-tables ds opts))
        (catch Throwable e
          (try
            (.rollback conn)
            (catch Throwable rollback-error
              (.addSuppressed e rollback-error))
            (finally (throw e)))))
      (.commit conn)))

  (open-write [_]
    (let [conn (jdbc/get-connection ds {:auto-commit false})]
      (->Writer conn (:dbtype ds) (:sql opts) (:tables opts))))

  (open-read [_]
    (let [conn (jdbc/get-connection ds {:auto-commit true})]
      (->Reader conn (:sql opts) (:tables opts)))))