(ns retrograde.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]))

;;; engrams

(s/def ::id (s/and integer? pos?))

(s/def ::key (s/and string? seq #(<= (count %) 100)))

(s/def ::data some?)

(s/def ::created inst?)

(s/def ::expires-at (s/nilable inst?))

(s/def ::decay-level nat-int?)

(s/def ::engram (s/keys :req-un [::id
                                 ::key
                                 ::data
                                 ::created
                                 ::decay-level]
                        :opt-un [::expires-at]))

;;; records

(defn- mem-rep-id-gen
  []
  (gen/fmap #(apply str %)
            (gen/vector (gen/elements [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f])
                        64)))

(s/def ::mem-rep-id
  (s/with-gen
    (s/and string?
           #(boolean (re-matches #"[0-9a-fA-F]+" %)))
    mem-rep-id-gen))

(s/def ::record (s/keys :req-un [::id
                                 ::key
                                 ::mem-rep-id
                                 ::created
                                 ::decay-level
                                 ::expires-at]))

;;; filter

(s/def :retrograde.specs.filter/key (s/coll-of ::key :min-count 1 :distinct true))

(s/def :retrograde.specs.filter/id (s/coll-of ::id :min-count 1 :distinct true))

(s/def :retrograde.specs.filter/expires-until inst?)

(s/def :retrograde.specs.filter/expires-after inst?)

(s/def ::filter (s/keys :opt-un [:retrograde.specs.filter/key
                                 :retrograde.specs.filter/id
                                 :retrograde.specs.filter/expires-until
                                 :retrograde.specs.filter/expires-after]))

;;; order

(s/def ::order-field #{:id :key :created :expires-at})

(s/def ::order-direction #{:asc :desc})

(s/def ::order-clause (s/tuple ::order-field ::order-direction))

(s/def ::order (s/coll-of ::order-clause :kind vector? :min-count 1))