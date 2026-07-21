# retrograde

A key-value database with stateful versioning and time-dependent degeneration - inspired by the principles of human memory.

## Overview

`retrograde` simulates natural forgetfulness in digital systems through controlled information loss and transformations. Instead of simply deleting data, it degenerates over time.

## Core Concepts

### Engrams (Memory Traces)
- Connect keys to semantic content
- Have an optional expiry date
- Include a `decay-level` counter for multi-stage degeneration (starts at 0)

### Memory Representation Objects (Semantic Content)
- Content-addressable storage of actual data
- Persist independently of their engrams
- Can be reused across multiple engrams when the stored data is identical

### Transformation & Degeneration
- After expiration, memories transform instead of disappearing
- Transformations are performed through composable transducers that process engrams in a streaming fashion
- Transducers can be combined to create complex degeneration pipelines
- The `reconsolidate!` function enables persistence of transformed engrams back to the underlying storage engine

## Walkthrough

`retrograde` is designed to work with any database backend through a pluggable storage interface. This allows you to use your preferred database technology while leveraging `retrograde`'s unique memory management capabilities.

Implementations of `retrograde.core/Reader` and `retrograde.core/Writer` must
also implement `java.io.Closeable` and `retrograde.core/Closed` so that
`retrograde` can manage their lifecycle safely.

### Database Support

`retrograde` provides a common JDBC store and comes with an implementation for
SQLite. Supporting another JDBC database requires methods for these multimethods,
dispatched by the datasource's `:dbtype`:

- `retrograde.jdbc.core/create-tables` must return the formatted SQL statements
  that create the memory representation and engram tables and their indexes.
- `retrograde.jdbc.core/insert-mem-rep-if-absent-query` must return a HoneySQL
  query that inserts a memory representation without replacing an existing row
  with the same ID.

The database-specific namespace must be loaded so that its methods are
registered. For SQLite, require `retrograde.jdbc.sqlite` as shown below.

The JDBC adapter handles the persistence layer transparently, allowing you to focus
on your application logic while `retrograde` manages the temporal aspects of your data.

### JDBC Notes

The JDBC store persists `java.time.Instant` values as epoch seconds. Sub-second
precision is truncated when `:created` and `:expires-at` values are written, so
an instant such as `2030-06-15T12:30:45.123456789Z` is read back as
`2030-06-15T12:30:45Z`.

Memory representation data is serialized with `pr-str` and deserialized with
`clojure.edn/read-string`. Values must therefore round-trip through EDN. Data
that prints with unreadable tagged literals, such as arbitrary Java objects, can
be written but may fail when read back.

`retrograde` wraps public write operations in transactions. If an operation fails,
the JDBC writer rolls the transaction back before rethrowing the original error.
If closing the underlying JDBC connection also fails afterwards, that close error
can mask the original transaction failure. Applications that need to preserve the
primary failure should handle lower-level JDBC writer lifecycles explicitly.

### Getting Started

Here's a quick example of setting up `retrograde` with a SQLite database:

```clojure
(require '[retrograde.core :as rg])
(require '[retrograde.jdbc.core :refer [->Store]])
(require '[retrograde.jdbc.sqlite])

(def honeysql-opts {:dialect :ansi
                    :quoted-snake true})

(def table-names {:engram :engrams
                  :mem-rep :objects})

(def store-opts {:sql honeysql-opts
                 :tables table-names})

(def ds {:dbtype "sqlite" :dbname "retrograde.db?foreign_keys=on"})

(def store (->Store ds store-opts))

(rg/init store)
```

### Storing memory representation objects

The `memorize!` function stores a new memory in the database and creates an associated engram. It automatically:
- Generates a unique engram ID
- Computes a content-addressable hash of the memory data for deduplication
- Sets the initial decay level to 0
- Returns the complete engram including the stored data

You can optionally specify an expiration date after which the memory becomes eligible for transformation.

```clojure
(def engram1
  (rg/memorize! store
                "alice"
                {:name "Alice"
                 :email "alice@example.com"}
                 :expires-at #inst "2280-01-01T00:00:00Z"))

;; engram1 => {:id 1
;;             :key "alice"
;;             :created #inst "2025-12-30T13:37:00Z"
;;             :decay-level 0
;;             :expires-at #inst "2280-01-01T00:00:00Z"
;;             :data {:name "Alice" :email "alice@example.com"}}
```

Each call to `memorize!` creates a new engram, even when using the same key. This allows you to maintain multiple versions of memories associated with the same key over time.


```clojure
(def engram2
  (rg/memorize! store
                "alice"
                {:name "Alice Miller"
                 :email "alice.miller@example.org"}
                 :expires-at #inst "2061-12-31T00:00:00Z"))

;; engram2 => {:id 2
;;             :key "alice"
;;             :created #inst "2025-12-30T13:38:00Z"
;;             :decay-level 0
;;             :expires-at #inst "2061-12-31T00:00:00Z"
;;             :data {:name "Alice Miller" :email "alice.miller@example.org"}}
```

### Retrieving Engrams

The `recall` function retrieves a single engram by its ID.

```clojure
(rg/recall store 1)

;; => {:id 1
;;     :key "alice"
;;     :created #inst "2025-12-30T13:37:00Z"
;;     :decay-level 0
;;     :expires-at #inst "2280-01-01T00:00:00Z"
;;     :data {:name "Alice" :email "alice@example.com"}}
```

### Streaming Engrams

`retrograde` provides transducer-like functions to efficiently process engrams:

```clojure
;; Store all active engrams associated to a given key in a vector ordered by expiry date
(rg/transduce-engrams
  store
  (map :data)
  conj
  []
  :filter {:key ["alice"]
           :expires-after (java.time.Instant/now)}
  :order [[:expires-at :asc]])

;; => [{:name "Alice Miller", :email "alice.miller@example.org"}
;;     {:name "Alice", :email "alice@example.com"}]
```

### Reconsolidating Engrams

The `reconsolidate!` function iterates over engrams and applies a transformation function to each one. It provides a powerful way to batch-process and modify engrams atomically.

**How it works:**
- Iterates through engrams based on the provided filter and order criteria
- Applies the transformation function to each engram
- The function can return:
  - **An engram map** - writes the updated engram back to the database
  - **`:retrograde.core/skip`** - skips this engram without making changes
- The entire process happens within a single transaction, ensuring atomicity
- `reconsolidate!` returns a vector of the updated engrams. Engrams skipped with
`:retrograde.core/skip` are not included.

```clojure
(defn decay-by-level
  "Transformation function that applies different decay stages"
  [engram]
  (case (:decay-level engram)
    ;; Level 0: Overwrite email
    0 (-> engram
          (update :data assoc :email "XXXXXX")
          (update :decay-level inc))
    
    ;; Level 1: Remove all details except name
    1 (-> engram
          (update :data select-keys [:name])
          (update :decay-level inc))
       
    ;; Otherwise: no changes
    :retrograde.core/skip))

;; Apply transformation
(rg/reconsolidate!
  store
  decay-by-level
  :filter {:key ["alice"]})
```

The transformation function receives the complete engram and decides what action to take based on its state. This is particularly useful for implementing multi-stage decay strategies where memories progressively degrade over time.

## Store Utilities

### Serializing Writes

`retrograde.serialized/serialize-writes` wraps an existing store and
ensures that only one write transaction can be open at a time. Read transactions
are passed through unchanged, so concurrent readers are not affected by the
write lock.

Use it when the underlying store or database connection cannot safely handle
multiple open writers:

```clojure
(require '[retrograde.serialized :refer [serialize-writes]])

(def raw-store (->Store ds store-opts))
(def store (serialize-writes raw-store))

(rg/init store)

(rg/memorize! store
              "alice"
              {:name "Alice"})
```

By default, `open-write` waits until the current writer is closed. You can pass
`:timeout-ms` to fail if the write lock is not acquired within a bounded amount
of time:

```clojure
(def store
  (serialize-writes raw-store :timeout-ms 1000))
```

When the timeout expires, `open-write` throws an exception. Explicit timeout
values must be positive integers.