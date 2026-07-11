(ns retrograde.writer-tests
  (:require [clojure.test :refer [testing is]]
            [retrograde.core :refer :all]
            [retrograde.time :refer [->epoch]]))

(defn test-open-write
  [store]
  (testing "returns Writer"
    (with-open [writer (open-write store)]
      (is (satisfies? Writer writer))))

  (testing "Writer is not closed"
    (with-open [writer (open-write store)]
      (is (not (closed? writer))))))

(defn test-close
  [store]
  (testing "returns nil"
    (let [writer (open-write store)]
      (is (nil? (.close writer)))))

  (testing "Writer is closed"
    (let [writer (open-write store)]
      (.close writer)
      (is (closed? writer)))))

(defn test-commit!
  [store]
  (testing "returns nil"
    (with-open [writer (open-write store)]
      (is (nil? (commit! writer)))))

  (testing "Writer is not closed"
    (with-open [writer (open-write store)]
      (commit! writer)
      (is (not (closed? writer)))))

  (testing "closed Writer throws Exception"
    (let [writer (open-write store)]
      (.close writer)
      (is (thrown? Exception (commit! writer))))))

(defn test-delete-all!
  [store]
  (testing "returns nil"
    (with-open [writer (open-write store)]
      (is (nil? (delete-all! writer)))))

  (testing "deletes all engrams"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)]
        (create-record! writer "key1" mem-rep-id nil)
        (create-record! writer "key2" mem-rep-id nil)
        (create-record! writer "key3" mem-rep-id nil)
        (commit! writer)
        (delete-all! writer)
        (commit! writer)
        (let [result (reduce-records writer
                                     (fn [acc _] (inc acc))
                                     0
                                     {})]
          (is (zero? result))))))

  (testing "deletes all mem-reps"
    (with-open [writer (open-write store)]
      (let [data1 {:foo "bar"}
            data2 {:baz "qux"}
            id1 (put-mem-rep! writer data1)
            id2 (put-mem-rep! writer data2)]
        (commit! writer)
        (is (some? (read-mem-rep writer id1)))
        (is (some? (read-mem-rep writer id2)))
        (delete-all! writer)
        (commit! writer)
        (is (nil? (read-mem-rep writer id1)))
        (is (nil? (read-mem-rep writer id2)))))))

(defn test-put-mem-rep!
  [store]
  (testing "returns hash id"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar" :baz 42}
            id (put-mem-rep! writer data)]
        (is (string? id))
        (is (not (empty? id))))))

  (testing "same data returns same hash"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar" :baz 42}
            id1 (put-mem-rep! writer data)
            id2 (put-mem-rep! writer data)]
        (is (= id1 id2)))))

  (testing "different data returns different hash"
    (with-open [writer (open-write store)]
      (let [data1 {:foo "bar"}
            data2 {:foo "baz"}
            id1 (put-mem-rep! writer data1)
            id2 (put-mem-rep! writer data2)]
        (is (not= id1 id2)))))

  (testing "stored data can be read back"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar" :baz 42}
            id (put-mem-rep! writer data)
            result (read-mem-rep writer id)]
        (is (= data result)))))

  (testing "duplicate insert is idempotent"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            id1 (put-mem-rep! writer data)
            id2 (put-mem-rep! writer data)
            result (read-mem-rep writer id1)]
        (is (= id1 id2))
        (is (= data result))))))

(defn test-read-mem-rep
  [store]
  (testing "returns nil for non-existent id"
    (with-open [writer (open-write store)]
      (is (nil? (read-mem-rep writer "nonexistent")))))

  (testing "returns data for existing id"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar" :baz 42}
            id (put-mem-rep! writer data)
            result (read-mem-rep writer id)]
        (is (some? result))
        (is (= data result))))))

(defn test-create-record!
  [store]
  (testing "returns record with generated id"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            expires-at (java.time.Instant/now)
            record (create-record! writer "test-key" mem-rep-id expires-at)]
        (is (some? record))
        (is (some? (:id record)))
        (is (= "test-key" (:key record)))
        (is (some? (:created record)))
        (is (= mem-rep-id (:mem-rep-id record)))
        (is (= (.getEpochSecond expires-at) (.getEpochSecond (:expires-at record))))
        (is (zero? (:decay-level record))))))

  (testing "created record can be read back"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            expires-at (java.time.Instant/now)
            created (create-record! writer "test-key" mem-rep-id expires-at)
            read-back (read-record writer (:id created))]
        (is (= (:id created) (:id read-back)))
        (is (= (:key created) (:key read-back)))
        (is (= (:mem-rep-id created) (:mem-rep-id read-back)))
        (is (= (:decay-level created) (:decay-level read-back)))
        (is (= (->epoch (:created created)) (->epoch (:created read-back))))
        (is (= (->epoch (:expires-at created)) (->epoch (:expires-at read-back)))))))

  (testing "expires-at can be nil"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            record (create-record! writer "test-key" mem-rep-id nil)]
        (is (some? record))
        (is (nil? (:expires-at record)))))))

(defn test-update-record!
  [store]
  (testing "updates existing record"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            created (create-record! writer "test-key" mem-rep-id nil)
            new-data {:baz "qux"}
            new-mem-rep-id (put-mem-rep! writer new-data)
            updated (update-record! writer (assoc created
                                                  :key "updated-key"
                                                  :mem-rep-id new-mem-rep-id
                                                  :decay-level 1
                                                  :expires-at (java.time.Instant/now)))]
        (is (= (:id created) (:id updated)))
        (is (= "updated-key" (:key updated)))
        (is (= new-mem-rep-id (:mem-rep-id updated)))
        (is (= 1 (:decay-level updated)))
        (is (some? (:expires-at updated)))
        (is (= (->epoch (:created created)) (->epoch (:created updated)))))))

  (testing "updated record can be read back"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            created (create-record! writer "test-key" mem-rep-id nil)
            new-data {:baz "qux"}
            new-mem-rep-id (put-mem-rep! writer new-data)
            updated (update-record! writer (assoc created
                                                  :key "updated-key"
                                                  :mem-rep-id new-mem-rep-id
                                                  :decay-level 1
                                                  :expires-at (java.time.Instant/now)))
            read-back (read-record writer (:id updated))]
        (is (= read-back updated)))))

  (testing "return nil if id does not exist"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            new-record {:id 999999
                        :key "new-key"
                        :created (java.time.Instant/now)
                        :mem-rep-id mem-rep-id
                        :expires-at nil
                        :decay-level 0}
            result (update-record! writer new-record)]
        (is (nil? result))))))

(defn test-reduce-records
  [store]
  (testing "reduces over empty result set"
    (with-open [writer (open-write store)]
      (let [result (reduce-records writer
                                   (fn [acc _] (conj acc :item))
                                   []
                                   {})]
        (is (= [] result)))))

  (testing "reduces over all records without filter"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "key1" mem-rep-id nil)
            _ (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {})]
        (is (= 3 (count result)))
        (is (= #{"key1" "key2" "key3"} (set result))))))

  (testing "filter by id - single id"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            rec1 (create-record! writer "key1" mem-rep-id nil)
            _ (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:id [(:id rec1)]}})]
        (is (= 1 (count result)))
        (is (= ["key1"] result)))))

  (testing "filter by id - multiple ids"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            rec1 (create-record! writer "key1" mem-rep-id nil)
            rec2 (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:id [(:id rec1) (:id rec2)]}})]
        (is (= 2 (count result)))
        (is (= #{"key1" "key2"} (set result))))))

  (testing "filter by key - single key"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "key1" mem-rep-id nil)
            _ (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:key ["key2"]}})]
        (is (= 1 (count result)))
        (is (= ["key2"] result)))))

  (testing "filter by key - multiple keys"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "key1" mem-rep-id nil)
            _ (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:key ["key1" "key3"]}})]
        (is (= 2 (count result)))
        (is (= #{"key1" "key3"} (set result))))))

  (testing "filter by expires-until"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            now (java.time.Instant/now)
            future1 (.plusSeconds now 3600)
            future2 (.plusSeconds now 7200)
            future3 (.plusSeconds now 10800)
            _ (create-record! writer "key1" mem-rep-id future1)
            _ (create-record! writer "key2" mem-rep-id future2)
            _ (create-record! writer "key3" mem-rep-id future3)
            cutoff (.plusSeconds now 7200)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:expires-until cutoff}})]
        (is (= 2 (count result)))
        (is (= #{"key1" "key2"} (set result))))))

  (testing "filter by expires-after"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            now (java.time.Instant/now)
            future1 (.plusSeconds now 3600)
            future2 (.plusSeconds now 7200)
            future3 (.plusSeconds now 10800)
            _ (create-record! writer "key1" mem-rep-id future1)
            _ (create-record! writer "key2" mem-rep-id future2)
            _ (create-record! writer "key3" mem-rep-id future3)
            cutoff (.plusSeconds now 7200)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:expires-after cutoff}})]
        (is (= 2 (count result)))
        (is (= #{"key2" "key3"} (set result))))))

  (testing "filter by expires-until and expires-after (range)"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            now (java.time.Instant/now)
            future1 (.plusSeconds now 3600)
            future2 (.plusSeconds now 7200)
            future3 (.plusSeconds now 10800)
            future4 (.plusSeconds now 14400)
            _ (create-record! writer "key1" mem-rep-id future1)
            _ (create-record! writer "key2" mem-rep-id future2)
            _ (create-record! writer "key3" mem-rep-id future3)
            _ (create-record! writer "key4" mem-rep-id future4)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:expires-after (.plusSeconds now 6000)
                                             :expires-until (.plusSeconds now 12000)}})]
        (is (= 2 (count result)))
        (is (= #{"key2" "key3"} (set result))))))

  (testing "combined filters - id and key"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            rec1 (create-record! writer "key1" mem-rep-id nil)
            rec2 (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key1" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:id record)))
                                   []
                                   {:filter {:id [(:id rec1) (:id rec2)]
                                             :key ["key1"]}})]
        (is (= 1 (count result)))
        (is (= [(:id rec1)] result)))))

  (testing "order by id ascending"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            rec1 (create-record! writer "key1" mem-rep-id nil)
            rec2 (create-record! writer "key2" mem-rep-id nil)
            rec3 (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:id record)))
                                   []
                                   {:order [[:id :asc]]})]
        (is (= [(:id rec1) (:id rec2) (:id rec3)] result)))))

  (testing "order by id descending"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            rec1 (create-record! writer "key1" mem-rep-id nil)
            rec2 (create-record! writer "key2" mem-rep-id nil)
            rec3 (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:id record)))
                                   []
                                   {:order [[:id :desc]]})]
        (is (= [(:id rec3) (:id rec2) (:id rec1)] result)))))

  (testing "order by key"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "zebra" mem-rep-id nil)
            _ (create-record! writer "alpha" mem-rep-id nil)
            _ (create-record! writer "delta" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:order [[:key :asc]]})]
        (is (= ["alpha" "delta" "zebra"] result)))))

  (testing "order by created"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "key1" mem-rep-id nil)
            _ (create-record! writer "key2" mem-rep-id nil)
            _ (create-record! writer "key3" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:order [[:created :asc]]})]
        (is (= ["key1" "key2" "key3"] result)))))

  (testing "order by expires-at"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            now (java.time.Instant/now)
            _ (create-record! writer "key3" mem-rep-id (.plusSeconds now 10800))
            _ (create-record! writer "key1" mem-rep-id (.plusSeconds now 3600))
            _ (create-record! writer "key2" mem-rep-id (.plusSeconds now 7200))
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:order [[:expires-at :asc]]})]
        (is (= ["key1" "key2" "key3"] result)))))

  (testing "order by multiple columns"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "beta" mem-rep-id nil)
            _ (create-record! writer "alpha" mem-rep-id nil)
            _ (create-record! writer "beta" mem-rep-id nil)
            _ (create-record! writer "alpha" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc [(:key record) (:id record)]))
                                   []
                                   {:order [[:key :asc] [:id :asc]]})]
        (is (= 4 (count result)))
        (is (= "alpha" (first (first result))))
        (is (= "alpha" (first (second result))))
        (is (= "beta" (first (nth result 2))))
        (is (= "beta" (first (nth result 3))))
        ;; IDs should be in ascending order within same key
        (is (< (second (first result)) (second (second result))))
        (is (< (second (nth result 2)) (second (nth result 3)))))))

  (testing "filter and order combined"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "zebra" mem-rep-id nil)
            _ (create-record! writer "alpha" mem-rep-id nil)
            _ (create-record! writer "delta" mem-rep-id nil)
            _ (create-record! writer "beta" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc record] (conj acc (:key record)))
                                   []
                                   {:filter {:key ["alpha" "delta" "beta"]}
                                    :order [[:key :desc]]})]
        (is (= ["delta" "beta" "alpha"] result)))))

  (testing "uses init value correctly"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            _ (create-record! writer "key1" mem-rep-id nil)
            result (reduce-records writer
                                   (fn [acc _] (+ acc 1))
                                   100
                                   {})]
        (is (= 101 result)))))

  (testing "reducing function receives proper record structure"
    (with-open [writer (open-write store)]
      (let [data {:foo "bar"}
            mem-rep-id (put-mem-rep! writer data)
            expires-at (java.time.Instant/now)
            created (create-record! writer "test-key" mem-rep-id expires-at)
            result (reduce-records writer
                                   (fn [_ record] record)
                                   nil
                                   {:filter {:id [(:id created)]}})]
        (is (some? result))
        (is (= (:id created) (:id result)))
        (is (= "test-key" (:key result)))
        (is (= mem-rep-id (:mem-rep-id result)))
        (is (zero? (:decay-level result)))
        (is (instance? java.time.Instant (:created result)))
        (is (instance? java.time.Instant (:expires-at result)))))))