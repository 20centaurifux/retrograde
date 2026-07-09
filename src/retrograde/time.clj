(ns retrograde.time)

(defn ->epoch
  "Converts a java.time.Instant or java.util.Date to epoch seconds.

  Returns nil when instant is nil."
  [instant]
  (when instant
    (if (instance? java.time.Instant instant)
        (.getEpochSecond instant)
        (quot (.getTime instant) 1000))))

(defn ->instant
  "Converts epoch seconds to a java.time.Instant.

  Returns nil when epoch is nil."
  [epoch]
  (when epoch
    (java.time.Instant/ofEpochSecond epoch)))