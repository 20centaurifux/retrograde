(ns retrograde.reader-tests
  (:require [clojure.test :refer [testing is]]
            [retrograde.core :as rg]))

(defn test-open-read
  [store]
  (testing "returns Reader"
    (with-open [reader (rg/open-read store)]
      (is (satisfies? rg/Reader reader))))

  (testing "Reader is not closed"
    (with-open [reader (rg/open-read store)]
      (is (not (rg/closed? reader))))))

(defn test-close
  [store]
  (testing "returns nil"
    (let [reader (rg/open-read store)]
      (is (nil? (.close reader)))))

  (testing "Reader is closed"
    (let [reader (rg/open-read store)]
      (.close reader)
      (is (rg/closed? reader)))))

(defn test-read-engram
  [store]
  (testing "returns nil for non-existent id"
    (with-open [reader (rg/open-read store)]
      (is (nil? (rg/read-engram reader 999999)))))

  (testing "returns engram for existing id"
    (let [data {:foo "bar" :baz 42}
          key "test-key"
          expires-at (-> (java.time.Instant/now)
                         (.plusSeconds 3600))
          engram (rg/memorize! store key data :expires-at expires-at)]
      (with-open [reader (rg/open-read store)]
        (let [result (rg/read-engram reader (:id engram))]
          (is (some? result))
          (is (= (:id engram) (:id result)))
          (is (= key (:key result)))
          (is (= (.getEpochSecond (:created engram)) (.getEpochSecond (:created result))))
          (is (= data (:data result)))
          (is (= (.getEpochSecond (:expires-at engram)) (.getEpochSecond (:expires-at result))))
          (is (= 0 (:decay-level result))))))))


(defn test-stream-engrams
  [store]
  (testing "streams empty result set"
    (rg/clear-all! store)
    (with-open [reader (rg/open-read store)]
      (let [result (rg/stream-engrams reader
                                      (map :key)
                                      conj
                                      []
                                      {})]
        (is (= [] result)))))

  (testing "streams all engrams without filter"
    (let [data1 {:content "first"}
          data2 {:content "second"}
          data3 {:content "third"}]
      ;; Create test engrams
      (rg/clear-all! store)
      (rg/memorize! store "key1" data1)
      (rg/memorize! store "key2" data2)
      (rg/memorize! store "key3" data3)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {})]
          (is (= 3 (count result)))
          (is (= #{"key1" "key2" "key3"} (set result)))))))

  (testing "streams with key filter"
    (let [data {:test "data"}]
      (rg/clear-all! store)
      (rg/memorize! store "filter-key1" data)
      (rg/memorize! store "filter-key2" data)
      (rg/memorize! store "other-key" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:filter {:key ["filter-key1" "filter-key2"]}})]
          (is (= 2 (count result)))
          (is (= #{"filter-key1" "filter-key2"} (set result)))))))

  (testing "streams with expires-until filter"
    (let [data {:test "expiry"}
          past (-> (java.time.Instant/now) (.minusSeconds 3600))
          future (-> (java.time.Instant/now) (.plusSeconds 3600))]
      (rg/clear-all! store)
      (rg/memorize! store "expired-key" data :expires-at past)
      (rg/memorize! store "future-key" data :expires-at future)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:filter {:expires-until (java.time.Instant/now)}})]
          (is (= 1 (count result)))
          (is (= "expired-key" (first result)))))))

  (testing "streams with expires-after filter"
    (let [data {:test "expiry"}
          past (-> (java.time.Instant/now) (.minusSeconds 3600))
          future (-> (java.time.Instant/now) (.plusSeconds 3600))
          now (java.time.Instant/now)]
      (rg/clear-all! store)
      (rg/memorize! store "expired-key" data :expires-at past)
      (rg/memorize! store "future-key" data :expires-at future)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:filter {:expires-after now}})]
          (is (= 1 (count result)))
          (is (= "future-key" (first result)))))))

  (testing "streams with id filter"
    (let [data {:test "id-filter"}]
      (rg/clear-all! store)
      (let [engram1 (rg/memorize! store "id-key1" data)
            engram2 (rg/memorize! store "id-key2" data)
            _ (rg/memorize! store "id-key3" data)]
        (with-open [reader (rg/open-read store)]
          (let [result (rg/stream-engrams reader
                                          (map :key)
                                          conj
                                          []
                                          {:filter {:id [(:id engram1) (:id engram2)]}})]
            (is (= 2 (count result)))
            (is (= #{"id-key1" "id-key2"} (set result))))))))

  (testing "streams with order by key ascending"
    (let [data {:order "test"}]
      (rg/clear-all! store)
      (rg/memorize! store "zebra" data)
      (rg/memorize! store "alpha" data)
      (rg/memorize! store "beta" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:key :asc]]})]
          (is (= ["alpha" "beta" "zebra"] result))))))

  (testing "streams with order by key descending"
    (let [data {:order "test"}]
      (rg/clear-all! store)
      (rg/memorize! store "zebra" data)
      (rg/memorize! store "alpha" data)
      (rg/memorize! store "beta" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:key :desc]]})]
          (is (= ["zebra" "beta" "alpha"] result))))))

  (testing "streams with order by created ascending"
    (let [data {:order "test"}]
      (rg/clear-all! store)
      (Thread/sleep 1001)
      (rg/memorize! store "first" data)
      (Thread/sleep 1001)
      (rg/memorize! store "second" data)
      (Thread/sleep 1001)
      (rg/memorize! store "third" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:created :asc]]})]
          (is (= ["first" "second" "third"] result))))))

  (testing "streams with order by created descending"
    (let [data {:order "test"}]
      (rg/clear-all! store)
      (Thread/sleep 1001)
      (rg/memorize! store "first" data)
      (Thread/sleep 1001)
      (rg/memorize! store "second" data)
      (Thread/sleep 1001)
      (rg/memorize! store "third" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:created :desc]]})]
          (is (= ["third" "second" "first"] result))))))

  (testing "streams with order by expires-at ascending"
    (let [data {:order "test"}
          early-expiry (-> (java.time.Instant/now) (.plusSeconds 1000))
          late-expiry (-> (java.time.Instant/now) (.plusSeconds 5000))]
      (rg/clear-all! store)
      (rg/memorize! store "late" data :expires-at late-expiry)
      (rg/memorize! store "early" data :expires-at early-expiry)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:expires-at :asc]]})]
          (is (= ["early" "late"] result))))))

  (testing "streams with order by expires-at descending"
    (let [data {:order "test"}
          early-expiry (-> (java.time.Instant/now) (.plusSeconds 1000))
          late-expiry (-> (java.time.Instant/now) (.plusSeconds 5000))]
      (rg/clear-all! store)
      (rg/memorize! store "late" data :expires-at late-expiry)
      (rg/memorize! store "early" data :expires-at early-expiry)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :key)
                                        conj
                                        []
                                        {:order [[:expires-at :desc]]})]
          (is (= ["late" "early"] result))))))

  (testing "streams with transducer chain"
    (let [data {:value 42}]
      (rg/clear-all! store)
      (rg/memorize! store "chain1" data)
      (rg/memorize! store "chain2" data)
      (rg/memorize! store "chain3" data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (comp (map :data)
                                              (map :value)
                                              (filter #(= % 42)))
                                        conj
                                        []
                                        {})]
          (is (= 3 (count result)))
          (is (every? #(= % 42) result))))))

  (testing "streams engram data correctly"
    (let [test-data {:complex {:nested "value"}
                     :list [1 2 3]
                     :string "test"}]
      (rg/clear-all! store)
      (rg/memorize! store "data-test" test-data)
      (with-open [reader (rg/open-read store)]
        (let [result (rg/stream-engrams reader
                                        (map :data)
                                        conj
                                        []
                                        {:filter {:key ["data-test"]}})]
          (is (= 1 (count result)))
          (is (= test-data (first result))))))))