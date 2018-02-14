(ns provisdom.math.format
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.strings :as strings]
    [provisdom.math.core :as m]
    [clojure.string :as str]))

(s/def ::digits (s/with-gen ::m/long+ #(gen/large-integer* {:min 1 :max 35})))
(s/def ::max-digits ::digits)
(s/def ::max-length ::m/long+)

(s/def ::decimal-places
  (s/with-gen ::m/long-non- #(gen/large-integer* {:min 0 :max 35})))

(s/def ::max-decimal-places ::decimal-places)
(s/def ::money? boolean?)

(def by-letter
  {"T" 1000000000000
   "B" 1000000000
   "M" 1000000
   "K" 1000})

(defn trim-number-as-string
  "Trims number of any unnecessary characters e.g. -0.3 and 0.30."
  [s]
  (let [s (cond (str/starts-with? s "-0") (trim-number-as-string (str "-" (strings/trim-start s "-0")))
                (str/includes? s ".") (strings/trim-end s "0")
                :else s)]
    (strings/trim-start s "0")))

(s/fdef trim-number-as-string
        :args (s/cat :s string?)
        :ret string?)

(defn format-as-float
  "Formats `finite` into float form with a number of decimal places."
  [finite decimal-places]
  (format (str "%." decimal-places "f") (double finite)))

(s/fdef format-as-float
        :args (s/cat :finite ::m/finite :decimal-places ::decimal-places)
        :ret string?)

(defn format-as-exponential
  "Formats `finite` into exponential form with a number of digits."
  ([finite] (format-as-exponential finite {}))
  ([finite {::keys [digits]}]
   (if digits
     (str/replace
       (str/replace
         (format (str "%." (long (dec digits)) "E")
                 (double finite))
         "E+0"
         "E+")
       "E-0"
       "E-")
     (let [s (str/replace
               (str/replace
                 (format (str "%." 15 "E") (double finite))
                 "E+0"
                 "E+")
               "E-0"
               "E-")]
       (loop [s1 s]
         (if (str/includes? s1 "0E")
           (recur (str/replace s1 "0E" "E"))
           s1))))))

(s/fdef format-as-exponential
        :args (s/cat :finite ::m/finite
                     :opts (s/? (s/keys :opt [::digits])))
        :ret string?)

(defn format-number
  "Formats `number` into its best form."
  ([number max-length] (format-number number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits]}]
   (cond (m/nan? number) "NaN"
         (m/inf+? number) "Inf"
         (m/inf-? number) "-Inf"
         :else (let [rounded-number (double (if max-decimal-places
                                              (read-string (format-as-float number max-decimal-places))
                                              number))
                     rounded-number-as-string (if max-digits
                                                (format-as-exponential rounded-number {::digits max-digits})
                                                (str rounded-number))
                     shortened-number (if max-digits
                                        (read-string rounded-number-as-string)
                                        rounded-number)
                     new-max-length (min max-length (count rounded-number-as-string))
                     standard-digits (count (str (m/round shortened-number :toward-zero)))
                     want-exponential? (or (and (> standard-digits new-max-length)
                                                (> standard-digits (+ 5.5 (* -0.5 (m/sgn shortened-number)))))
                                           (< (m/abs shortened-number) 0.0001))]
                 (loop [i (max new-max-length 1)]
                   (let [s (if want-exponential?
                             (format-as-exponential shortened-number {::digits i})
                             (format-as-float shortened-number (dec i)))]
                     (if (or (m/one? i) (<= (count s) new-max-length))
                       s
                       (recur (dec i)))))))))

(s/fdef format-number
        :args (s/cat :number ::m/number
                     :max-length ::max-length
                     :opts (s/? (s/keys :opt [::max-decimal-places ::max-digits])))
        :ret string?)

;;;SHORTHAND
(defn unparse-shorthand
  "Converts `number` into shorthand."
  ([number max-length] (unparse-shorthand number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits money?]}]
   (cond (m/nan? number) "NaN"
         (m/inf+? number) "Inf"
         (m/inf-? number) "-Inf"
         :else (let [rounded-number (double (if max-decimal-places
                                              (read-string
                                                (format-as-float number max-decimal-places))
                                              number))
                     shortened-number (if max-digits
                                        (read-string
                                          (format-as-exponential rounded-number {::digits max-digits}))
                                        rounded-number)
                     absolute-value (m/abs shortened-number)
                     f (fn [x letter]
                         (let [adjusted-number (str (format-number (/ x (by-letter letter))
                                                                   (max 1 (dec max-length))))
                               without-ending (if (str/ends-with? adjusted-number ".0")
                                                (strings/butlast-string
                                                  (strings/butlast-string adjusted-number))
                                                adjusted-number)]
                           (str without-ending letter)))
                     s (cond (>= absolute-value 1e15) (format-number shortened-number max-length)
                             (>= absolute-value (by-letter "T")) (f shortened-number "T")
                             (>= absolute-value (by-letter "B")) (f shortened-number "B")
                             (>= absolute-value (by-letter "M")) (f shortened-number "M")
                             (>= absolute-value (by-letter "K")) (f shortened-number "K")
                             :else (format-number shortened-number max-length))]
                 (if money?
                   (if (neg? shortened-number)
                     (str "-$" (strings/rest-string s))
                     (str "$" s))
                   s)))))

(s/fdef unparse-shorthand
        :args (s/cat :number ::m/number
                     :max-length ::max-length
                     :opts (s/? (s/keys :opt [::max-decimal-places ::max-digits ::money?])))
        :ret string?)

(defn parse-shorthand
  "Converts a shorthand string, `s`, into a number if possible. Otherwise
  returns nil."
  [s]
  (let [removed-money (cond (str/starts-with? s "$") (strings/rest-string s)
                            (str/starts-with? s "-$") (str "-" (strings/trim-start s "-$"))
                            :else s)]
    (case removed-money
      "NaN" m/nan
      "Inf" m/inf+
      "-Inf" m/inf-
      (let [f (fn [x letter]
                (let [n (read-string (strings/butlast-string x))]
                  (when (number? n)
                    (* (by-letter letter) n))))]
        (try (cond (str/ends-with? removed-money "T") (f removed-money "T")
                   (str/ends-with? removed-money "B") (f removed-money "B")
                   (str/ends-with? removed-money "M") (f removed-money "M")
                   (str/ends-with? removed-money "K") (f removed-money "K")
                   :else (let [n (read-string removed-money)]
                           (when (number? n)
                             n)))
             (catch Exception _ nil))))))

(s/fdef parse-shorthand
        :args (s/cat :s string?)
        :ret (s/nilable ::m/number))

