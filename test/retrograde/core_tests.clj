(ns retrograde.core-tests
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [retrograde.core :refer :all]
            [retrograde.specs :as specs]
            [spy.assert :as assert]
            [spy.protocol :as p]))

;;; Generators

(def ^:private hex-char-gen
  (gen/elements [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]))

(def ^:private  hex-string-gen
  (gen/fmap #(apply str %) (gen/vector hex-char-gen 32)))

;;; Helpers

(defn- ->mem-rep-id
  []
  (gen/generate hex-string-gen))

(defn- ->record
  ([id]
   (->record id (->mem-rep-id)))
  ([id mem-rep-id]
   {:id id
    :key (gen/generate (gen/not-empty gen/string))
    :mem-rep-id mem-rep-id
    :created (java.time.Instant/now)
    :decay-level 0
    :expires-at (java.time.Instant/now)}))

;;; Predicates

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

;;; Reset store

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
  (let [k (gen/generate (gen/not-empty gen/string))
        data (gen/generate (gen/map gen/keyword gen/any))
        hash (gen/generate hex-string-gen)]
    (testing "without expiry date"
      (let [record {:id 1
                    :key k
                    :created (java.time.Instant/now)
                    :mem-rep-id hash
                    :decay-level 0
                    :expires-at 0}
            writer (writer
                    (put-mem-rep! [_ _] hash)
                    (create-record! [_ _ _ _] record)
                    (commit! [_]))
            spy (p/spies writer)
            store (->WriterStore writer)
            engram (-> record
                       (dissoc :mem-rep-id)
                       (assoc :data data))]
        (let [result (memorize! store k data)]
          (is (= result engram))
          (assert/called-once-with? (:put-mem-rep! spy) writer data)
          (assert/called-once-with? (:create-record! spy) writer k hash nil)
          (assert/called-once-with? (:commit! spy) writer))))

    (testing "with expiry date"
      (let [expiry-date (.plus (java.time.Instant/now) (java.time.Duration/ofDays 1))
            record {:id 1
                    :key k
                    :created (java.time.Instant/now)
                    :mem-rep-id hash
                    :decay-level 0
                    :expires-at expiry-date}
            writer (writer
                    (put-mem-rep! [_ _] hash)
                    (create-record! [_ _ _ _] record)
                    (commit! [_]))
            spy (p/spies writer)
            store (->WriterStore writer)
            engram (-> record
                       (dissoc :mem-rep-id)
                       (assoc :data data))]
        (let [result (memorize! store k data :expires-at expiry-date)]
          (is (= result engram))
          (assert/called-once-with? (:put-mem-rep! spy) writer data)
          (assert/called-once-with? (:create-record! spy) writer k hash expiry-date)
          (assert/called-once-with? (:commit! spy) writer))))

    (testing "put-mem-rep! throws Exception"
      (let [writer (writer
                    (put-mem-rep! [_ _] (throw (Exception. "put-mem-rep! failed")))
                    (create-record! [_ _ _ _])
                    (commit! [_]))
            spy (p/spies writer)
            store (->WriterStore writer)]
        (is (thrown? Exception (memorize! store k data)))
        (assert/called-once-with? (:put-mem-rep! spy) writer data)
        (assert/not-called? (:create-record! spy))
        (assert/not-called? (:commit! spy))))

    (testing "create-record! throws Exceptions"
      (let [writer (writer
                    (put-mem-rep! [_ _] hash)
                    (create-record! [_ _ _ _] (throw (Exception. "create-record! failed")))
                    (commit! [_]))
            spy (p/spies writer)
            store (->WriterStore writer)]
        (is (thrown? Exception (memorize! store k data)))
        (assert/called-once-with? (:put-mem-rep! spy) writer data)
        (assert/called-once-with? (:create-record! spy) writer k hash nil)
        (assert/not-called? (:commit! spy))))))

(deftest test-recall
  (testing "engram found"
    (let [id (gen/generate (gen/large-integer* {:min 1}))
          engram {:id id
                  :key (gen/generate (gen/not-empty gen/string))
                  :created (java.time.Instant/now)
                  :decay-level 0
                  :expires-at nil}
          reader (reader
                  (read-engram [_ _] engram))
          spy (p/spies reader)
          store (->ReaderStore reader)
          match (recall store id)]
      (is (= engram match))
      (assert/called-once-with? (:read-engram spy) reader id)))

  (testing "engram not found"
    (let [id (gen/generate (gen/large-integer* {:min 1}))
          reader (reader
                  (read-engram [_ _]))
          spy (p/spies reader)
          store (->ReaderStore reader)
          match (recall store id)]
      (is (nil? match))
      (assert/called-once-with? (:read-engram spy) reader id))))

(deftest test-transduce-engrams
  (testing "transduce engrams"
    (let [engrams [{:id 1 :key "a" :data {:x 1}}
                   {:id 2 :key "b" :data {:x 2}}
                   {:id 3 :key "c" :data {:x 3}}]
          xform (map :data)
          reader (reader
                  (stream-engrams [_ xf f init query]
                                  (transduce xf f init engrams)))
          spy (p/spies reader)
          store (->ReaderStore reader)
          result (transduce-engrams store xform conj [] :order [[:key :asc]] :filter {:key ["a" "b" "c"]})]
      (is (= [{:x 1} {:x 2} {:x 3}] result))
      (assert/called-once-with?
       (:stream-engrams spy)
       reader
       xform
       conj
       []
       {:order [[:key :asc]] :filter {:key ["a" "b" "c"]}}))))

(deftest test-reconsolidate!
  (testing "reconsolidate multiple engrams"
    (let [record1 (->record 1)
          mem-rep1 {:x 1}
          record2 (->record 2)
          mem-rep2 {:x 2}
          new-hash1 (gen/generate hex-string-gen)
          new-hash2 (gen/generate hex-string-gen)
          f (fn [engram]
              (assoc-in engram [:data :x] (* 2 (get-in engram [:data :x]))))
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
                                  (= data {:x 2}) new-hash1
                                  (= data {:x 4}) new-hash2))
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
      (assert/called-with? (:update-record! spy) writer (assoc record1 :mem-rep-id new-hash1))
      (assert/called-with? (:update-record! spy) writer (assoc record2 :mem-rep-id new-hash2))
      (assert/called-once? (:commit! spy))))

  (testing "f returns :retrograde.core/skip for some engrams"
    (let [record1 (->record 1)
          mem-rep1 {:foo 1}
          record2 (->record 2)
          mem-rep2 {:bar 2}
          new-hash (gen/generate hex-string-gen)
          f (fn [engram]
              (if (= (get-in engram [:data :foo]) 1)
                (assoc-in engram [:data :bar] 2)
                :retrograde.core/skip))
          writer (writer
                  (reduce-records [_ f init _]
                                  (let [result (f init record1)
                                        result' (f result record2)]
                                    result'))
                  (read-mem-rep [_ mem-rep-id]
                                (cond
                                  (= mem-rep-id (:mem-rep-id record1)) mem-rep1
                                  (= mem-rep-id (:mem-rep-id record1)) mem-rep2))
                  (put-mem-rep! [_ _] new-hash)
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
      (assert/called-once-with? (:update-record! spy) writer (assoc record1 :mem-rep-id new-hash))
      (assert/called-once? (:commit! spy))))

  (testing "memory representations are cached"
    (let [mem-rep-id (->mem-rep-id)
          record1 (->record 1 mem-rep-id)
          record2 (->record 2 mem-rep-id)
          mem-rep {:x 1}
          f (fn [_]
              :retrograde.core/skip)
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
          result (reconsolidate! store f)]
      (is (zero? result))
      (assert/called-once? (:reduce-records spy))
      (assert/called-once-with? (:read-mem-rep spy) writer mem-rep-id)
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/called-once? (:commit! spy))))

  (testing "memory representation cache is local only"
    (let [mem-rep-id (->mem-rep-id)
          record1 (->record 1 mem-rep-id)
          record2 (->record 2 mem-rep-id)
          mem-rep {:x 1}
          f (fn [_]
              :retrograde.core/skip)
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
          result (repeatedly 5 #(reconsolidate! store f))]
      (is (every? zero? result))
      (assert/called-n-times? (:reduce-records spy) 5)
      (assert/called-n-times? (:read-mem-rep spy) 5)
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/called-n-times? (:commit! spy) 5)))

  (testing "throws ex-info when f returns invalid engram"
    (let [record (->record 1)
          mem-rep {:x 1}
          f (fn [_]
              23)
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
        (reconsolidate! store f)
        (is false "Expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= "Invalid engram" (.getMessage e)))
          (let [data (ex-data e)]
            (is (= (-> data :explain ::s/problems first :val) 23))
            (is (contains? data :explain))
            (is (some? (:explain data))))))
      (assert/called-once? (:reduce-records spy))
      (assert/called-once? (:read-mem-rep spy))
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/not-called? (:commit! spy))))

  (testing "throws ex-info when f changes engram ID"
    (let [record (->record 1)
          mem-rep {:x 1}
          f (fn [engram]
              (assoc engram :id (inc (:id engram))))
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
        (reconsolidate! store f)
        (is false "Expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= "Engram ID has changed" (.getMessage e)))
          (let [data (ex-data e)]
            (is (s/valid? ::specs/engram (:old data)))
            (is (s/valid? ::specs/engram (:new data))))))
      (assert/called-once? (:reduce-records spy))
      (assert/called-once? (:read-mem-rep spy))
      (assert/not-called? (:put-mem-rep! spy))
      (assert/not-called? (:update-record! spy))
      (assert/not-called? (:commit! spy)))))