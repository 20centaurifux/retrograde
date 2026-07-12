(ns retrograde.core-tests
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [retrograde.core :refer :all]
            [retrograde.specs :as specs]
            [spy.assert :as assert]
            [spy.protocol :as p]))

;;; Helpers

(defn- ->mem-rep-id
  []
  (gen/generate (s/gen ::specs/mem-rep-id)))

;; Builds record fixtures with generated defaults while allowing tests to pin
;; the fields that are relevant for the behavior under test.
(defn- ->record
  ([id]
   (->record id {}))
  ([id fields]
   (let [{:keys [key mem-rep-id created decay-level expires-at]
          :or {mem-rep-id (->mem-rep-id)
               key (gen/generate (gen/not-empty gen/string))
               created (java.time.Instant/now)
               decay-level 0
               expires-at (java.time.Instant/now)}} fields]
     {:id id
      :key key
      :mem-rep-id mem-rep-id
      :created created
      :decay-level decay-level
      :expires-at expires-at})))

(defn- record->engram
  [record mem-rep]
  (-> record
      (dissoc :mem-rep-id)
      (assoc :data mem-rep)))

;;; Predicates Tests

(deftest test-store?
  (testing "Store is Store"
    (is (store? (reify Store))))
  (testing "arbitary data is no Store"
    (is (not (store? (gen/generate gen/any))))))

(defprotocol ^:private Closable
  :extend-via-metadata true
  (close [this]))

(defmacro ^:private writer
  [& body]
  `(p/mock
    Writer
    ~@body
    Closable
    (close [_])))

(deftype WriterStore [w]
  Store
  (open-write [_] w))

(defmacro ^:private reader
  [& body]
  `(p/mock
    Reader
    ~@body
    Closable
    (close [_])))

(deftype ReaderStore [r]
  Store
  (open-read [_] r))

;;; Delete Store Tests

(deftest test-clear-all!
  (testing "calls delete-all! and commit!"
    (let [writer (writer
                  (delete-all! [_])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (clear-all! store)

      (assert/called-once-with? (:delete-all! spy) writer)
      (assert/called-once-with? (:commit! spy) writer)))

  (testing "delete-all! throws Exception"
    (let [writer (writer
                  (delete-all! [_] (throw (Exception. "delete-all! failed")))
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (is (thrown? Exception (clear-all! store)))

      (assert/called-once-with? (:delete-all! spy) writer)
      (assert/not-called? (:commit! spy))))

  (testing "commit! throws Exception"
    (let [writer (writer
                  (delete-all! [_])
                  (commit! [_] (throw (Exception. "commit! failed"))))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (is (thrown? Exception (clear-all! store)))

      (assert/called-once-with? (:delete-all! spy) writer)
      (assert/called-once-with? (:commit! spy) writer))))

;;; Direct Engram Access

(deftest test-memorize!
  (testing "without expiry date"
    (let [id 1
          k "a"
          timestamp (java.time.Instant/now)
          mem-rep-id (->mem-rep-id)
          mem-rep {:x 1}
          writer (writer
                  (put-mem-rep! [_ mem-rep]
                                mem-rep-id)
                  (create-record! [_ k mem-rep-id expires-at]
                                  (->record id
                                            {:key k
                                             :mem-rep-id mem-rep-id
                                             :created timestamp
                                             :expires-at expires-at}))
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (let [engram (memorize! store k mem-rep)]
        (is (s/valid? ::specs/engram engram))
        (is (= id (:id engram)))
        (is (= k (:key engram)))
        (is (= mem-rep (:data engram)))
        (is (= timestamp (:created engram)))
        (is (zero? (:decay-level engram)))
        (is (nil? (:expires-at engram))))

      (assert/called-once-with? (:put-mem-rep! spy) writer mem-rep)
      (assert/called-once-with? (:create-record! spy) writer k mem-rep-id nil)
      (assert/called-once-with? (:commit! spy) writer)))

  (testing "with expiry date"
    (let [id 1
          k "a"
          timestamp (java.time.Instant/now)
          mem-rep-id (->mem-rep-id)
          mem-rep {:x 1}
          expiry-date (.plus (java.time.Instant/now) (java.time.Duration/ofDays 1))
          writer (writer
                  (put-mem-rep! [_ mem-rep]
                                mem-rep-id)
                  (create-record! [_ k mem-rep-id expires-at]
                                  (->record id
                                            {:key k
                                             :mem-rep-id mem-rep-id
                                             :created timestamp
                                             :expires-at expires-at}))
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (let [engram (memorize! store k mem-rep :expires-at expiry-date)]
        (is (s/valid? ::specs/engram engram))
        (is (= id (:id engram)))
        (is (= k (:key engram)))
        (is (= mem-rep (:data engram)))
        (is (= timestamp (:created engram)))
        (is (zero? (:decay-level engram)))
        (is (= expiry-date (:expires-at engram))))

      (assert/called-once-with? (:put-mem-rep! spy) writer mem-rep)
      (assert/called-once-with? (:create-record! spy) writer k mem-rep-id expiry-date)
      (assert/called-once-with? (:commit! spy) writer)))

  (testing "put-mem-rep! throws Exception"
    (let [k "a"
          mem-rep {:x 1}
          writer (writer
                  (put-mem-rep! [_ _] (throw (Exception. "put-mem-rep! failed")))
                  (create-record! [_ _ _ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (is (thrown? Exception (memorize! store k mem-rep)))

      (assert/called-once-with? (:put-mem-rep! spy) writer mem-rep)
      (assert/not-called? (:create-record! spy))
      (assert/not-called? (:commit! spy))))

  (testing "create-record! throws Exceptions"
    (let [k "a"
          mem-rep-id (->mem-rep-id)
          mem-rep {:x 1}
          writer (writer
                  (put-mem-rep! [_ _] mem-rep-id)
                  (create-record! [_ _ _ _] (throw (Exception. "create-record! failed")))
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (is (thrown? Exception (memorize! store k mem-rep)))

      (assert/called-once-with? (:put-mem-rep! spy) writer mem-rep)
      (assert/called-once-with? (:create-record! spy) writer k mem-rep-id nil)
      (assert/not-called? (:commit! spy)))))

(deftest test-recall
  (testing "engram found"
    (let [engram (-> (->record 1)
                     (record->engram  {:x 1}))
          reader (reader
                  (read-engram [_ _] engram))
          spy (p/spies reader)
          store (->ReaderStore reader)
          match (recall store 1)]
      (is (= engram match))

      (assert/called-once-with? (:read-engram spy) reader 1)))

  (testing "engram not found"
    (let [reader (reader
                  (read-engram [_ _]))
          spy (p/spies reader)
          store (->ReaderStore reader)
          match (recall store 1)]
      (is (nil? match))

      (assert/called-once-with? (:read-engram spy) reader 1))))

;;; Streaming Tests

(deftest test-transduce-engrams
  (testing "transduce engrams"
    (let [engrams [(-> (->record 1 {:key "a"})
                       (record->engram {:x 1}))
                   (-> (->record 2 {:key "b"})
                       (record->engram {:x 2}))
                   (-> (->record 3 {:key "c"})
                       (record->engram {:x 3}))]
          reader (reader
                  (stream-engrams [_ xf f init query]
                                  (transduce xf f init engrams)))
          spy (p/spies reader)
          store (->ReaderStore reader)
          xform (map :data)
          result (transduce-engrams store
                                    xform
                                    conj
                                    []
                                    :order [[:key :asc]]
                                    :filter {:key ["a" "b" "c"]})]
      (is (= [{:x 1} {:x 2} {:x 3}] result))

      (assert/called-once-with? (:stream-engrams spy)
                                reader
                                xform
                                conj
                                []
                                {:order [[:key :asc]]
                                 :filter {:key ["a" "b" "c"]}}))))

(deftest test-reconsolidate!
  (testing "reconsolidate multiple engrams"
    (let [record1 (->record 1)
          mem-rep1 {:x 1}
          record2 (->record 2)
          mem-rep2 {:x 2}
          new-mem-rep-id1 (->mem-rep-id)
          new-mem-rep-id2 (->mem-rep-id)
          f (fn [engram]
              (assoc-in engram
                        [:data :x]
                        (* 2 (get-in engram [:data :x]))))
          writer (writer
                  (reduce-records [_ f init _]
                                  (let [result (f init record1)
                                        result' (f result record2)]
                                    result'))
                  (read-mem-rep [_ mem-rep-id]
                                (cond
                                  (= mem-rep-id (:mem-rep-id record1)) mem-rep1
                                  (= mem-rep-id (:mem-rep-id record2)) mem-rep2))
                  (put-mem-rep! [_ data]
                                (cond
                                  (= data {:x 2}) new-mem-rep-id1
                                  (= data {:x 4}) new-mem-rep-id2))
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)
          result (reconsolidate! store f)]
      (is (= 2 result))

      (assert/called-once? (:reduce-records spy))
      (assert/called-n-times? (:read-mem-rep spy) 2)
      (assert/called-with? (:read-mem-rep spy) writer (:mem-rep-id record1))
      (assert/called-with? (:read-mem-rep spy) writer (:mem-rep-id record2))
      (assert/called-n-times? (:put-mem-rep! spy) 2)
      (assert/called-with? (:put-mem-rep! spy) writer {:x 2})
      (assert/called-with? (:put-mem-rep! spy) writer {:x 4})
      (assert/called-n-times? (:update-record! spy) 2)
      (assert/called-with? (:update-record! spy) writer
                           (assoc record1 :mem-rep-id new-mem-rep-id1))
      (assert/called-with? (:update-record! spy) writer
                           (assoc record2 :mem-rep-id new-mem-rep-id2))
      (assert/called-once? (:commit! spy))))

  (testing "f returns :retrograde.core/skip for some engrams"
    (let [record1 (->record 1)
          mem-rep1 {:x :first}
          record2 (->record 2)
          mem-rep2 {:x :second}
          new-mem-rep-id (->mem-rep-id)
          f (fn [engram]
              (if (= (get-in engram [:data :x]) :first)
                (assoc-in engram [:data :y] 1)
                :retrograde.core/skip))
          writer (writer
                  (reduce-records [_ f init _]
                                  (let [result (f init record1)
                                        result' (f result record2)]
                                    result'))
                  (read-mem-rep [_ mem-rep-id]
                                (cond
                                  (= mem-rep-id (:mem-rep-id record1)) mem-rep1
                                  (= mem-rep-id (:mem-rep-id record2)) mem-rep2))
                  (put-mem-rep! [_ _] new-mem-rep-id)
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)
          result (reconsolidate! store f)]
      (is (= 1 result))

      (assert/called-once? (:reduce-records spy))
      (assert/called-n-times? (:read-mem-rep spy) 2)
      (assert/called-with? (:read-mem-rep spy) writer (:mem-rep-id record1))
      (assert/called-with? (:read-mem-rep spy) writer (:mem-rep-id record2))
      (assert/called-once? (:put-mem-rep! spy))
      (assert/called-once-with? (:update-record! spy)
                                writer
                                (assoc record1 :mem-rep-id new-mem-rep-id))
      (assert/called-once? (:commit! spy))))

  (testing "memory representations are cached"
    (let [mem-rep-id (->mem-rep-id)
          record1 (->record 1 {:mem-rep-id mem-rep-id})
          record2 (->record 2 {:mem-rep-id mem-rep-id})
          mem-rep {:x 1}
          writer (writer
                  (reduce-records [_ f init _]
                                  (let [result (f init record1)
                                        result' (f result record2)]
                                    result'))
                  (read-mem-rep [_ _]
                                mem-rep)
                  (put-mem-rep! [_ _])
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)
          result (reconsolidate! store
                                 (constantly :retrograde.core/skip))]
      (is (zero? result))

      (assert/called-once? (:reduce-records spy))
      (assert/called-once-with? (:read-mem-rep spy) writer mem-rep-id)
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/called-once? (:commit! spy))))

  (testing "memory representation cache is local only"
    (let [mem-rep-id (->mem-rep-id)
          record1 (->record 1 {:mem-rep-id mem-rep-id})
          record2 (->record 2 {:mem-rep-id mem-rep-id})
          mem-rep {:x 1}
          writer (writer
                  (reduce-records [_ f init _]
                                  (let [result (f init record1)
                                        result' (f result record2)]
                                    result'))
                  (read-mem-rep [_ _]
                                mem-rep)
                  (put-mem-rep! [_ _])
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)
          result (repeatedly 5
                             #(reconsolidate! store
                                              (constantly :retrograde.core/skip)))]
      (is (every? zero? result))

      (assert/called-n-times? (:reduce-records spy) 5)
      (assert/called-n-times? (:read-mem-rep spy) 5)
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/called-n-times? (:commit! spy) 5)))

  (testing "throws ex-info when f returns invalid engram"
    (let [record (->record 1)
          mem-rep {:x 1}
          writer (writer
                  (reduce-records [_ f init query]
                                  (f init record))
                  (read-mem-rep [_ _]
                                mem-rep)
                  (put-mem-rep! [_ _])
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)]
      (try
        (reconsolidate! store (constantly 1))
        (is false "Expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo ex
          (is (= "Invalid engram" (.getMessage ex)))
          (let [data (ex-data ex)]
            (is (= (-> data :explain ::s/problems first :val) 1))
            (is (some? (:explain data))))))

      (assert/called-once? (:reduce-records spy))
      (assert/called-once? (:read-mem-rep spy))
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/not-called? (:commit! spy))))

  (testing "throws ex-info when f changes engram ID"
    (let [record (->record 1)
          mem-rep {:x 1}
          writer (writer
                  (reduce-records [_ f init query]
                                  (f init record))
                  (read-mem-rep [_ _]
                                mem-rep)
                  (put-mem-rep! [_ _])
                  (update-record! [_ _])
                  (commit! [_]))
          spy (p/spies writer)
          store (->WriterStore writer)
          f (fn [engram]
              (assoc engram :id (inc (:id engram))))]
      (try
        (reconsolidate! store f)
        (is false "Expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo ex
          (is (= "Engram ID has changed" (.getMessage ex)))
          (let [data (ex-data ex)]
            (is (s/valid? ::specs/engram (:old data)))
            (is (s/valid? ::specs/engram (:new data))))))

      (assert/called-once? (:reduce-records spy))
      (assert/called-once? (:read-mem-rep spy))
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/not-called? (:commit! spy)))))