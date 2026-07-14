(ns retrograde.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.cache :as cache]
            [retrograde.specs :as specs]))

(def ^:private mem-rep-cache-threshold 100)

;;; Protocols

(defprotocol Store
  "Protocol for persistent engram storage.

  Implementors must provide initialization and transaction management."
  (init [store]
    "Initializes the store, creating necessary tables or data structures.")
  (open-write [store]
    "Opens a write transaction.
    Returns a `Writer` instance that must be closed after use.")
  (open-read [store]
    "Opens a read transaction.
    Returns a `Reader` instance that must be closed after use."))

(defn store?
  "Tests if `x` implements `Store` protocol."
  [x]
  (satisfies? Store x))

(defprotocol Closed
  "Protocol for checking if a resource is closed."
  (closed? [x]
    "Returns true if the resource is closed, false otherwise."))

(defprotocol Reader
  "Protocol for reading engrams from the store."
  (read-engram [reader engram-id]
    "Reads a single engram by ID.
    Returns the engram map if found, nil otherwise.")
  (stream-engrams [reader xform f init query]
    "Streams engrams using a transducer.

    Parameters:
    - xform: transducer to transform the stream
    - f: reducing function
    - init: initial value for reduction
    - query: map with :filter and :order keys

    Returns the result of the transduction."))

(defprotocol Writer
  "Protocol for writing and modifying engrams in the store."
  (commit! [writer]
    "Commits the current transaction.
    Must be called to persist changes.

    If commit! throws, the exception is propagated and callers should not assume
    rollback! has been called. Implementations should make closing a writer with
    an unresolved failed commit safe for their backing store.")
  (rollback! [writer]
    "Rolls back the current transaction.
    Discards any uncommitted changes.")
  (delete-all! [writer]
    "Deletes all engrams from the store.")
  (put-mem-rep! [writer mem-rep]
    "Stores a memory representation (data).
    Returns the mem-rep-id for the stored data.")
  (read-mem-rep [writer mem-rep-id]
    "Reads a memory representation by ID.")
  (create-record! [writer k mem-rep-id expires-at]
    "Creates a new engram record.

    Parameters:
    - k: key string
    - mem-rep-id: ID of the stored memory representation
    - expires-at: optional Instant when the engram expires

    Returns the created record map with :id, :key, :mem-rep-id, :created,
    :expires-at, and :decay-level fields.")
  (update-record! [writer record]
    "Updates an existing engram record.

    Implementations must preserve the existing :created timestamp.")
  (read-record [writer engram-id]
    "Reads an engram record by ID.")
  (reduce-records [writer f init query]
    "Reduces over engram records using a reducing function.

    Parameters:
    - f: reducing function taking accumulator and record
    - init: initial accumulator value
    - query: map with :filter and :order keys

    Returns the final accumulator value."))

;;; Public API

;; Transaction Helpers

(defn- rollback!-safely-and-throw
  [w cause]
  (try
    (rollback! w)
    (catch Throwable rollback-error
      (.addSuppressed cause rollback-error)))
  (throw cause))

(defmacro ^:private with-write-transaction
  [[w store] & body]
  `(with-open [~w (open-write ~store)]
     (let [result# (try
                     (do ~@body)
                     (catch Throwable e#
                       (rollback!-safely-and-throw ~w e#)))]
       (commit! ~w)
       result#)))

;; Validation Helpers

(defn- throw-if-invalid
  [spec value msg]
  (when-not (s/valid? spec value)
    (throw (ex-info msg
                    {:explain (s/explain-data spec value)}))))

;; Clear Storage

(defn clear-all!
  "Deletes all engrams from the store."
  [store]
  {:pre [(store? store)]}
  (with-write-transaction [w store]
    (delete-all! w)))

;; Direct Engram Access

(defn memorize!
  "Stores a new engram in the store.

  Parameters:
  - `store`: The store to write to
  - `k`: A key string to identify the engram (max 100 characters)
  - `mem-rep`: The data/memory representation to store. It must be serializable
               with `clojure.core/pr-str` and readable with `clojure.edn/read-string`.
  - `:expires-at`: Optional keyword argument with an Instant when the engram expires

  Returns the created engram map with :id, :key, :data, :created, :expires-at,
  and :decay-level fields."
  [store k mem-rep & {:keys [expires-at]}]
  {:pre [(store? store)]}
  (throw-if-invalid ::specs/key k "Invalid key")
  (throw-if-invalid ::specs/data mem-rep "Invalid memory representation")
  (throw-if-invalid ::specs/expires-at expires-at "Invalid expiry date")

  (with-write-transaction [w store]
    (let [mem-rep-id (put-mem-rep! w mem-rep)
          record (create-record! w k mem-rep-id expires-at)]
      (-> record
          (dissoc :mem-rep-id)
          (assoc :data mem-rep)))))

(defn recall
  "Retrieves an engram from the store by its ID.

  Parameters:
  - `store`: The store to read from
  - `id`: The engram ID

  Returns the engram map if found, nil otherwise."
  [store id]
  {:pre [(store? store)]}
  (throw-if-invalid ::specs/id id "Invalid id")

  (with-open [r (open-read store)]
    (read-engram r id)))

;; Streaming

(defn- ->query [filter order]
  (cond-> {}
    filter (assoc :filter filter)
    order (assoc :order order)))

(defn transduce-engrams
  "Transduces engrams from the store using a transducer.

  Parameters:
  - `store`: The store to read from
  - `xform`: A transducer function to transform the engram stream
  - `f`: The reducing function
  - `init`: The initial value for the reduction
  - `:filter`: Optional keyword argument with a filter map to select engrams.
            Supported filters:
            - `:key` - collection of key strings to match
            - `:id` - collection of engram IDs to match
            - `:expires-until` - Instant, selects engrams expiring before this time
            - `:expires-after` - Instant, selects engrams expiring after this time
  - `:order`: Optional keyword argument with a vector of [field direction] tuples
             (default: [[:key :asc] [:created :asc]])
             Allowed fields: :id, :key, :created, :expires-at
             Allowed directions: :asc, :desc

  Returns the result of the transduction.

  Example:
    (transduce-engrams store (map :key) conj [] :filter {:key [\"key1\" \"key2\"]})"
  [store xform f init & {:keys [filter order]
                         :or {order [[:key :asc] [:created :asc]]}}]
  {:pre [(store? store)]}
  (throw-if-invalid fn? xform "Invalid transducer")
  (throw-if-invalid fn? f "Invalid reducing function")
  (throw-if-invalid (s/nilable ::specs/filter) filter "Invalid filter")
  (throw-if-invalid ::specs/order order "Invalid order")

  (with-open [r (open-read store)]
    (stream-engrams r xform f init (->query filter order))))

;; Altering

(defn- lookup-mem-rep [w cache mem-rep-id]
  (if (cache/has? cache mem-rep-id)
    [(cache/lookup cache mem-rep-id)
     (cache/hit cache mem-rep-id)]
    (let [mem-rep (read-mem-rep w mem-rep-id)]
      [mem-rep
       (cache/miss cache mem-rep-id mem-rep)])))

(defn reconsolidate!
  "Applies a transformation function to selected engrams and updates them in the store.

  Parameters:
  - `store`: The store to write to
  - `f`: A function that takes an engram and returns either:
         - An updated engram map (will be saved)
         - :retrograde.core/skip (engram will be skipped)
  - `:filter`: Optional keyword argument with a filter map to select engrams.
            Supported filters:
            - `:key` - collection of key strings to match
            - `:id` - collection of engram IDs to match
            - `:expires-until` - Instant, selects engrams expiring before this time
            - `:expires-after` - Instant, selects engrams expiring after this time
  - `:order`: Optional keyword argument with a vector of [field direction] tuples
             (default: [[:created :asc]])
             Allowed fields: :id, :key, :created, :expires-at
             Allowed directions: :asc, :desc

  The function `f` receives each engram as a full map with :id, :key, :data,
  :created, :expires-at, and :decay-level fields. It should return an updated
  engram map or :retrograde.core/skip to skip the update. The :id field must
  remain unchanged. Any change to :created is ignored; the original timestamp is
  preserved.

  Returns a vector of the updated engrams. Skipped engrams are not included.

  Example:
    (reconsolidate!
      store
      (fn [engram]
        (update engram :data dissoc :email))
      :filter {:expires-until (java.time.Instant/now)}
      :order [[:expires-at :asc]])"
  [store f & {:keys [filter order]
              :or {order [[:created :asc]]}}]
  {:pre [(store? store)]}
  (throw-if-invalid fn? f "Invalid transformation function")
  (throw-if-invalid (s/nilable ::specs/filter) filter "Invalid filter")
  (throw-if-invalid ::specs/order order "Invalid order")

  (with-write-transaction [w store]
    (->> (->query filter order)
         (reduce-records
          w
          (fn [{:keys [result cache] :as state}
               {:keys [mem-rep-id] :as record}]
            (let [[mem-rep cache'] (lookup-mem-rep w cache mem-rep-id)
                  old-engram (-> record
                                 (dissoc :mem-rep-id)
                                 (assoc :data mem-rep))
                  new-engram (-> (f old-engram))]
              (if (= new-engram ::skip)
                (assoc state :cache cache')
                (do
                  (when-not (s/valid? ::specs/engram new-engram)
                    (throw (ex-info "Invalid engram"
                                    {:explain (s/explain-data ::specs/engram new-engram)})))

                  (when (not= (:id old-engram) (:id new-engram))
                    (throw (ex-info "Engram ID has changed"
                                    {:old old-engram :new new-engram})))

                  (let [new-engram' (assoc new-engram :created (:created old-engram))
                        mem-rep-id' (put-mem-rep! w (:data new-engram))]
                    (update-record! w (-> new-engram'
                                          (dissoc :data)
                                          (assoc :mem-rep-id mem-rep-id')))
                    {:result (conj result new-engram')
                     :cache (cache/miss cache'
                                        mem-rep-id'
                                        (:data new-engram'))})))))
          {:result []
           :cache (cache/lu-cache-factory {} :threshold mem-rep-cache-threshold)})
         :result)))