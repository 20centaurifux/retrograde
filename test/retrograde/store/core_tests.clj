(ns retrograde.store.core-tests
  (:require [clojure.test :refer [deftest is testing]]
            [retrograde.core :as rg]
            [retrograde.store.core :refer [defdecorated-reader
                                           defdecorated-writer]]))

;;; (Decorated) Writers

(defn- writer
  [& {:keys [close-fn]}]
  (reify
    java.io.Closeable
    (close [_]
      (when close-fn
        (close-fn))
      :closed)

    rg/Closed
    (closed? [_]
      :closed)

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

(defdecorated-writer DelegatingWriter [child])

(defdecorated-writer OverridingWriter [child tag close-count]
  (close [_]
         (swap! close-count inc))

  (rg/closed? [_]
              [:closed tag])

  (rg/commit! [_]
              [:commit tag])

  (rg/rollback! [_]
                [:rollback tag])

  (rg/delete-all! [_]
                  [:delete-all tag])

  (rg/put-mem-rep! [_ mem-rep]
                   [:put tag mem-rep])

  (rg/read-mem-rep [_ mem-rep-id]
                   [:read-mem-rep tag mem-rep-id])

  (rg/delete-orphan-mem-reps! [_]
                              [:delete-orphans tag])

  (rg/create-record! [_ k mem-rep-id expires-at]
                     [:create-record tag k mem-rep-id expires-at])

  (rg/update-record! [_ record]
                     [:update-record tag record])

  (rg/read-record [_ engram-id]
                  [:read-record tag engram-id])

  (rg/reduce-records [_ f init query]
                     [:reduce-records tag (f init :overridden-record) query]))

;;; (Decorated) Readers

(defn- reader
  [& {:keys [closed? close-fn]}]
  (reify
    java.io.Closeable
    (close [_]
      (when close-fn
        (close-fn))
      :closed)

    rg/Closed
    (closed? [_]
      (boolean closed?))

    rg/Reader
    (read-engram [_ engram-id]
      [:read-engram engram-id])

    (stream-engrams [_ xform f init query]
      [:stream-engrams ((xform f) init :engram) query])))

(defdecorated-reader DelegatingReader [child])

(defdecorated-reader OverridingReader [child tag close-count]
  (close [_]
         (swap! close-count inc))

  (rg/closed? [_]
              [:closed tag])

  (rg/read-engram [_ engram-id]
                  [:read tag engram-id])

  (rg/stream-engrams [_ xform f init query]
                     [:stream tag ((xform f) init :overridden-engram) query]))

;;; Tests

;; Writer

(deftest test-defdecorated-writer-is-resource
  (testing "implements writer resource protocols"
    (let [decorated (->DelegatingWriter (writer))]
      (is (instance? java.io.Closeable decorated))
      (is (satisfies? rg/Closed decorated))
      (is (satisfies? rg/Writer decorated))
      (is (not (satisfies? rg/Reader decorated))))))

(deftest test-defdecorated-writer-delegates-to-child
  (testing "forwards Writer methods"
    (let [decorated (->DelegatingWriter (writer))]
      (is (= :closed (rg/closed? decorated)))
      (is (= :committed (rg/commit! decorated)))
      (is (= :rolled-back (rg/rollback! decorated)))
      (is (= :deleted-all (rg/delete-all! decorated)))
      (is (= [:put-mem-rep {:x 1}]
             (rg/put-mem-rep! decorated {:x 1})))
      (is (= [:read-mem-rep "abc"]
             (rg/read-mem-rep decorated "abc")))
      (is (= :deleted-orphans
             (rg/delete-orphan-mem-reps! decorated)))
      (is (= [:create-record "key" "abc" nil]
             (rg/create-record! decorated "key" "abc" nil)))
      (is (= [:update-record {:id 1}]
             (rg/update-record! decorated {:id 1})))
      (is (= [:read-record 1]
             (rg/read-record decorated 1)))
      (is (= [:reduce-records [:init :record] {:filter {:id [1]}}]
             (rg/reduce-records decorated
                                (fn [acc record] [acc record])
                                :init
                                {:filter {:id [1]}})))))

  (testing "closes the child by default"
    (let [close-count (atom 0)
          decorated (->DelegatingWriter (writer :close-fn #(swap! close-count inc)))]
      (is (nil? (.close decorated)))
      (is (= 1 @close-count)))))

(deftest test-defdecorated-writer-uses-overrides
  (let [child-close-count (atom 0)
        override-close-count (atom 0)
        decorated (->OverridingWriter
                   (writer :close-fn #(swap! child-close-count inc))
                   :writer
                   override-close-count)]
    (is (= [:closed :writer] (rg/closed? decorated)))
    (is (= [:commit :writer] (rg/commit! decorated)))
    (is (= [:rollback :writer] (rg/rollback! decorated)))
    (is (= [:delete-all :writer] (rg/delete-all! decorated)))
    (is (= [:put :writer {:x 1}]
           (rg/put-mem-rep! decorated {:x 1})))
    (is (= [:read-mem-rep :writer "abc"]
           (rg/read-mem-rep decorated "abc")))
    (is (= [:delete-orphans :writer]
           (rg/delete-orphan-mem-reps! decorated)))
    (is (= [:create-record :writer "key" "abc" nil]
           (rg/create-record! decorated "key" "abc" nil)))
    (is (= [:update-record :writer {:id 1}]
           (rg/update-record! decorated {:id 1})))
    (is (= [:read-record :writer 1]
           (rg/read-record decorated 1)))
    (is (= [:reduce-records :writer [:init :overridden-record] {:filter {:id [1]}}]
           (rg/reduce-records decorated
                              (fn [acc record] [acc record])
                              :init
                              {:filter {:id [1]}})))
    (is (nil? (.close decorated)))
    (is (= 1 @override-close-count))
    (is (zero? @child-close-count))))

;; Reader

(deftest test-defdecorated-reader-is-resource
  (testing "implements reader resource protocols"
    (let [decorated (->DelegatingReader (reader))]
      (is (instance? java.io.Closeable decorated))
      (is (satisfies? rg/Closed decorated))
      (is (satisfies? rg/Reader decorated))
      (is (not (satisfies? rg/Writer decorated))))))

(deftest test-defdecorated-reader-delegates-to-child
  (testing "forwards Reader methods"
    (let [decorated (->DelegatingReader (reader))]
      (is (= [:read-engram 42]
             (rg/read-engram decorated 42)))
      (is (= [:stream-engrams [:init :engram] {:order [[:id :asc]]}]
             (rg/stream-engrams decorated
                                identity
                                (fn [acc engram] [acc engram])
                                :init
                                {:order [[:id :asc]]})))))

  (testing "closes the child by default"
    (let [close-count (atom 0)
          decorated (->DelegatingReader (reader :close-fn #(swap! close-count inc)))]
      (is (nil? (.close decorated)))
      (is (= 1 @close-count)))))

(deftest test-defdecorated-reader-uses-overrides
  (let [child-close-count (atom 0)
        override-close-count (atom 0)
        decorated (->OverridingReader
                   (reader :close-fn #(swap! child-close-count inc))
                   :reader
                   override-close-count)]
    (is (= [:closed :reader] (rg/closed? decorated)))
    (is (= [:read :reader 42] (rg/read-engram decorated 42)))
    (is (= [:stream :reader [:init :overridden-engram] {}]
           (rg/stream-engrams decorated
                              identity
                              (fn [acc engram] [acc engram])
                              :init
                              {})))
    (is (nil? (.close decorated)))
    (is (= 1 @override-close-count))
    (is (zero? @child-close-count))))