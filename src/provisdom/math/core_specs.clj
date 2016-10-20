(ns provisdom.math.core-specs
  (:require [clojure.spec :as s]
            [provisdom.math.core :as m]))

(s/def ::non- (s/and number? m/non-?))
(s/def ::pos (s/and (s/double-in :infinite? false) pos?))

(s/def ::long-non- (s/int-in 0 m/max-long))
(s/def ::long-pos (s/int-in 1 m/max-long))
(s/def ::nilable-pos (s/nilable ::pos))
(s/def ::prob (s/double-in :min 0.0 :max 1.0))
(s/def ::open-prob (s/double-in :min m/tiny-dbl :max 0.9999999999999999))
(s/def ::nilable-prob (s/nilable ::prob))
(s/def ::rnd ::open-prob)
(s/def ::rnd-lazy (s/coll-of ::rnd))