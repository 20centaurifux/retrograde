(ns retrograde.jdbc.sqlite-tests
  (:require [clojure.test :refer [use-fixtures deftest]]
            [retrograde.core :refer [init Store]]
            [retrograde.jdbc.core :refer [->Store]]
            [retrograde.jdbc.sqlite]
            [retrograde.reader-tests :as rd]
            [retrograde.store-tests :as st]
            [retrograde.writer-tests :as wt]
            [next.jdbc :as jdbc]))

(def ^:private ^:dynamic store (reify
                                 Store
                                 (init [_] (throw (UnsupportedOperationException.)))
                                 (open-read [_] (throw (UnsupportedOperationException.)))
                                 (open-write [_] (throw (UnsupportedOperationException.)))))

(def ^:private opts {:sql {:dialect :ansi
                           :quoted-snake true}
                     :tables {:mem-rep :memories
                              :engram :engrams}})

(defn- sqlite-fixture
  [f]
  (let [ds {:dbtype "sqlite"
            :dbname "file::memory:?cache=shared&foreign_keys=on"}]
    (with-open [_ (jdbc/get-connection ds)]
      (binding [store (->Store ds opts)]
        (init store)
        (f)))))

(use-fixtures :each sqlite-fixture)

;;; Store

(deftest test-instants-survive-time-zone-changes
  (st/test-instants-survive-time-zone-changes store))

(deftest test-dates-survive-time-zone-changes
  (st/test-dates-survive-time-zone-changes store))

;;; Writer

(deftest test-writer_open-write
  (wt/test-open-write store))

(deftest test-writer_close
  (wt/test-close store))

(deftest test-writer_commit!
  (wt/test-commit! store))

(deftest test-writer_rollback!
  (wt/test-rollback! store))

(deftest test-writer_delete-all!
  (wt/test-delete-all! store))

(deftest test-writer-read-mem-rep
  (wt/test-read-mem-rep store))

(deftest test-writer-put-mem-rep!
  (wt/test-put-mem-rep! store))

(deftest test-writer-create-record!
  (wt/test-create-record! store))

(deftest test-writer-update-record!
  (wt/test-update-record! store))

(deftest test-writer-reduce-records
  (wt/test-reduce-records store))

;;; Reader

(deftest test-reader_open-read
  (rd/test-open-read store))

(deftest test-reader_close
  (rd/test-close store))

(deftest test-reader_read-engram
  (rd/test-read-engram store))

(deftest test-reader_stream-engrams
  (rd/test-stream-engrams store))