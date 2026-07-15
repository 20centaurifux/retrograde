(ns retrograde.store-tests
  (:require [clojure.test :refer [testing is]]
            [retrograde.core :as rg]))

(defn test-instants-survive-time-zone-changes
  [store]
  (testing "stores and reads instants independently of the default time zone"
    (let [original-time-zone (java.util.TimeZone/getDefault)
          expires-at (java.time.Instant/parse "2030-06-15T12:30:45Z")]
      (try
        (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone "UTC"))
        (let [engram (rg/memorize! store
                                   "time-zone-test"
                                   {:time-zone "stable"}
                                   :expires-at expires-at)]
          (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone "Europe/Berlin"))
          (let [read-back (rg/recall store (:id engram))]
            (is (= (:created engram)
                   (:created read-back)))
            (is (= expires-at
                   (:expires-at read-back)))))
        (finally
          (java.util.TimeZone/setDefault original-time-zone))))))

(defn test-dates-survive-time-zone-changes
  [store]
  (testing "stores java.util.Date values independently of the default time zone"
    (let [original-time-zone (java.util.TimeZone/getDefault)
          expires-at (java.util.Date/from
                      (java.time.Instant/parse "2030-06-15T12:30:45Z"))]
      (try
        (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone "UTC"))
        (let [engram (rg/memorize! store
                                   "date-time-zone-test"
                                   {:time-zone "stable"}
                                   :expires-at expires-at)]
          (java.util.TimeZone/setDefault (java.util.TimeZone/getTimeZone "Europe/Berlin"))
          (let [read-back (rg/recall store (:id engram))]
            (is (= (:created engram)
                   (:created read-back)))
            (is (= (.toInstant expires-at)
                   (:expires-at read-back)))))
        (finally
          (java.util.TimeZone/setDefault original-time-zone))))))