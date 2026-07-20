(ns retrograde.serialized-tests
  (:require [clojure.test :refer [deftest testing is]]
            [retrograde.core :as rg]
            [retrograde.serialized :refer [serialize-writes]]))

;;; Writer

(defn- writer
  [& {:keys [closed? close-fn]}]
  (reify
    java.io.Closeable
    (close [_]
      (when close-fn
        (close-fn)))

    rg/Closed
    (closed? [_]
      (boolean closed?))

    rg/Writer
    (commit! [_]
      :committed)

    (rollback! [_]
      :rolled-back)

    (delete-all! [_]
      :deleted-all)

    (put-mem-rep! [_ mem-rep]
      [:put-mem-rep mem-rep])

    (read-mem-rep [_ mem-rep-id]
      [:read-mem-rep mem-rep-id])

    (delete-orphan-mem-reps! [_]
      :deleted-orphans)

    (create-record! [_ k mem-rep-id expires-at]
      [:create-record k mem-rep-id expires-at])

    (update-record! [_ record]
      [:update-record record])

    (read-record [_ engram-id]
      [:read-record engram-id])

    (reduce-records [_ f init query]
      [:reduce-records (f init :record) query])))

;;; Tests

(deftest test-serialize-writes-store-delegation
  (testing "returns a Store"
    (is (rg/store? (serialize-writes (reify rg/Store)))))

  (testing "delegates init and open-read"
    (let [store (reify
                  rg/Store
                  (init [_] :initialized)
                  (open-write [_] (writer))
                  (open-read [_] :reader))
          locked-store (serialize-writes store)]
      (is (= :initialized (rg/init locked-store)))
      (is (= :reader (rg/open-read locked-store))))))

(deftest test-serialize-writes-locks-writer-lifetime
  (testing "blocks another writer while the first writer is open"
    (let [close-count (atom 0)
          writer-opened (java.util.concurrent.CountDownLatch. 1)
          store (reify
                  rg/Store
                  (open-write [_]
                    (writer :close-fn #(swap! close-count inc))))
          locked-store (serialize-writes store {:timeout-ms 1000})
          first-writer (rg/open-write locked-store)
          second-writer (future
                          (let [w (rg/open-write locked-store)]
                            (.countDown writer-opened)
                            w))]
      (is (not (.await writer-opened 25 java.util.concurrent.TimeUnit/MILLISECONDS)))
      (.close first-writer)
      (let [w @second-writer]
        (try
          (is (.await writer-opened 1 java.util.concurrent.TimeUnit/SECONDS))
          (finally
            (.close w))))
      (is (= 2 @close-count))))

  (testing "unlocking is idempotent when the writer is closed more than once"
    (let [close-count (atom 0)
          store (reify
                  rg/Store
                  (open-write [_]
                    (writer :close-fn #(swap! close-count inc))))
          locked-writer (-> store
                            serialize-writes
                            rg/open-write)]
      (.close locked-writer)
      (.close locked-writer)
      (is (= 1 @close-count)))))

(deftest test-serialize-writes-unlocks-when-open-write-fails
  (testing "releases the lock if the wrapped store cannot open a writer"
    (let [attempts (atom 0)
          store (reify
                  rg/Store
                  (open-write [_]
                    (when (= 1 (swap! attempts inc))
                      (throw (ex-info "open-write failed" {})))
                    (writer)))
          locked-store (serialize-writes store)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"open-write failed"
                            (rg/open-write locked-store)))
      (with-open [w (rg/open-write locked-store)]
        (is (satisfies? rg/Writer w))))))

(deftest test-locked-writer-delegates-writer-protocol
  (testing "delegates Writer and Closed protocol methods"
    (let [store (reify
                  rg/Store
                  (open-write [_]
                    (writer :closed? true)))]
      (with-open [locked-writer (-> store
                                    serialize-writes
                                    rg/open-write)]
        (try
          (is (rg/closed? locked-writer))
          (is (= :committed (rg/commit! locked-writer)))
          (is (= :rolled-back (rg/rollback! locked-writer)))
          (is (= :deleted-all (rg/delete-all! locked-writer)))
          (is (= [:put-mem-rep {:x 1}]
                 (rg/put-mem-rep! locked-writer {:x 1})))
          (is (= [:read-mem-rep "abc"]
                 (rg/read-mem-rep locked-writer "abc")))
          (is (= :deleted-orphans
                 (rg/delete-orphan-mem-reps! locked-writer)))
          (is (= [:create-record "key" "abc" nil]
                 (rg/create-record! locked-writer "key" "abc" nil)))
          (is (= [:update-record {:id 1}]
                 (rg/update-record! locked-writer {:id 1})))
          (is (= [:read-record 1]
                 (rg/read-record locked-writer 1)))
          (is (= [:reduce-records [:init :record] {:filter {:id [1]}}]
                 (rg/reduce-records locked-writer
                                    (fn [acc record] [acc record])
                                    :init
                                    {:filter {:id [1]}}))))))))

(deftest test-serialize-writes-timeout
  (testing "throws when the write lock cannot be acquired before timeout"
    (let [release (java.util.concurrent.CountDownLatch. 1)
          store (reify
                  rg/Store
                  (open-write [_]
                    (writer)))
          locked-store (serialize-writes store {:timeout-ms 10})
          first-writer (rg/open-write locked-store)
          holder (future
                   (try
                     (rg/open-write locked-store)
                     (catch Throwable e
                       e)
                     (finally
                       (.countDown release))))]
      (try
        (let [result @holder]
          (is (instance? clojure.lang.ExceptionInfo result))
          (is (re-find #"Timed out waiting for write lock" (.getMessage result))))
        (finally
          (.close first-writer)
          (is (.await release 1 java.util.concurrent.TimeUnit/SECONDS))))))

  (testing "rejects invalid timeout values"
    (let [store (reify rg/Store)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid write lock timeout"
                            (serialize-writes store {:timeout-ms 0})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid write lock timeout"
                            (serialize-writes store {:timeout-ms -1})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid write lock timeout"
                            (serialize-writes store {:timeout-ms 1.5}))))))