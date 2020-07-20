(ns provisdom.math.format
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.utility-belt.strings :as strings]
    [provisdom.math.core :as m]
    [clojure.string :as str]))

(s/def ::digits
  (s/with-gen ::m/int+
              #(gen/large-integer* {:min 1 :max 35})))

(s/def ::max-digits ::digits)

(s/def ::max-length ::m/int+)

(s/def ::decimal-places
  (s/with-gen ::m/int-non-
              #(gen/large-integer* {:min 0 :max 35})))

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
  (let [s (cond (str/starts-with? s "-0") (trim-number-as-string
                                            (str "-"
                                                 (strings/trim-start s "-0")))
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
  :args (s/cat :finite ::m/finite
               :decimal-places ::decimal-places)
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
  ([number max-length
    {::keys [max-decimal-places max-digits]}]
   (let [number (double number)]
     (cond (m/nan? number) "NaN"
           (m/inf+? number) "Inf"
           (m/inf-? number) "-Inf"
           :else (let [rounded (double
                                 (if max-decimal-places
                                   (read-string
                                     (format-as-float
                                       number max-decimal-places))
                                   number))
                       rounded-str (if max-digits
                                     (format-as-exponential
                                       rounded {::digits max-digits})
                                     (str rounded))
                       shortened (double (if max-digits
                                           (read-string rounded-str)
                                           rounded))
                       new-max-length (min max-length (count rounded-str))
                       std-digits (count (str (m/round shortened :toward-zero)))
                       want-exp? (or (and (> std-digits new-max-length)
                                          (> std-digits
                                             (+ 5.5
                                                (* -0.5 (m/sgn shortened)))))
                                     (< (m/abs shortened) 0.0001))]
                   (loop [i (max new-max-length 1)]
                     (let [s (if want-exp?
                               (format-as-exponential shortened {::digits i})
                               (format-as-float shortened (dec i)))]
                       (if (or (<= i 1) (<= (count s) new-max-length))
                         s
                         (recur (dec i))))))))))

(s/fdef format-number
  :args (s/cat :number ::m/number
               :max-length ::max-length
               :opts (s/? (s/keys :opt [::max-decimal-places
                                        ::max-digits])))
  :ret string?)

;;;SHORTHAND
(defn unparse-shorthand
  "Converts `number` into shorthand."
  ([number max-length] (unparse-shorthand number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits money?]}]
   (cond (m/nan? number) "NaN"
         (m/inf+? number) "Inf"
         (m/inf-? number) "-Inf"
         :else (let [rounded (double
                               (if max-decimal-places
                                 (read-string
                                   (format-as-float number max-decimal-places))
                                 number))
                     shortened (if max-digits
                                 (read-string
                                   (format-as-exponential
                                     rounded {::digits max-digits}))
                                 rounded)
                     ab-v (m/abs shortened)
                     f (fn [x letter]
                         (let [adj-n (str (format-number
                                            (/ x (by-letter letter))
                                            (max 1 (dec max-length))))
                               wo-ending (if (str/ends-with? adj-n ".0")
                                           (strings/butlast-string
                                             (strings/butlast-string adj-n))
                                           adj-n)]
                           (str wo-ending letter)))
                     s (cond (>= ab-v 1e15) (format-number shortened max-length)
                             (>= ab-v (by-letter "T")) (f shortened "T")
                             (>= ab-v (by-letter "B")) (f shortened "B")
                             (>= ab-v (by-letter "M")) (f shortened "M")
                             (>= ab-v (by-letter "K")) (f shortened "K")
                             :else (format-number shortened max-length))]
                 (if money?
                   (if (neg? shortened)
                     (str "-$" (strings/rest-string s))
                     (str "$" s))
                   s)))))

(s/fdef unparse-shorthand
  :args (s/cat :number ::m/number
               :max-length ::max-length
               :opts (s/? (s/keys :opt [::max-decimal-places
                                        ::max-digits
                                        ::money?])))
  :ret string?)

(defn parse-shorthand
  "Converts a shorthand string, `s`, into a number if possible. Otherwise
  returns nil."
  [s]
  (let [removed-money (cond (str/starts-with? s "$")
                            (strings/rest-string s)

                            (str/starts-with? s "-$")
                            (str "-" (strings/trim-start s "-$"))

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
