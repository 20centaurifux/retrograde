(ns retrograde.jdbc.sqlite.ddl
  (:require [retrograde.jdbc.core :refer [create-tables]]
            [honey.sql :as sql]))

(defn- create-mem-rep-table
  [relation]
  {:create-table [relation :if-not-exists]
   :with-columns
   [[:id [:varchar 64] :primary-key :not-null]
    [:data :text :not-null]]})

(defn- create-engram-table
  [relation mem-rep-relation]
  {:create-table [relation :if-not-exists]
   :with-columns
   [[:id :integer :primary-key :autoincrement]
    [:key [:varchar 100] :not-null]
    [:created :integer :not-null]
    [:mem-rep-id [:varchar 64] :not-null [:references mem-rep-relation :id]]
    [:decay-level :int :not-null]
    [:expires-at :integer]]})

(defn- create-engram-table-key-index
  [relation]
  {:create-index [[(keyword (str (name relation) "-key-idx")) :if-not-exists]
                  [relation :key]]})

(defn- create-engram-table-expires-at-index
  [relation]
  {:create-index [[(keyword (str (name relation) "-expires-at-idx")) :if-not-exists]
                  [relation :expires-at]]})

(defmethod create-tables "sqlite"
  [_ds {sql :sql {:keys [engram mem-rep]} :tables}]
  (mapv #(sql/format % sql)
        [(create-mem-rep-table mem-rep)
         (create-engram-table engram mem-rep)
         (create-engram-table-key-index engram)
         (create-engram-table-expires-at-index engram)]))