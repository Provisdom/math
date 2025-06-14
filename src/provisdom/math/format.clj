(ns provisdom.math.format
  "Number formatting and parsing utilities.
  
  Provides intelligent number formatting that automatically chooses between
  fixed-point and scientific notation based on magnitude and display constraints.
  Includes shorthand notation (K, M, B, T) for large numbers and money formatting.
  
  Key features:
  - Adaptive formatting (fixed vs scientific notation)
  - Shorthand notation parsing/unparsing (1.5K → 1500)  
  - Money formatting with $ prefix
  - Configurable precision and length limits
  - Robust parsing that handles edge cases"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.strings :as strings]))

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
  "Removes unnecessary characters from a number string.
  
  Eliminates leading/trailing zeros and normalizes negative zero representations.
  Useful for cleaning up formatted number output.
  
  Examples:
    (trim-number-as-string \"0.30\")    ;=> \"0.3\"
    (trim-number-as-string \"-0.0\")    ;=> \"0\"
    (trim-number-as-string \"000123\")  ;=> \"123\"
    (trim-number-as-string \"5.000\")   ;=> \"5\""
  [s]
  (let [s (cond (str/starts-with? s "-0")
            (trim-number-as-string
              (str "-" (strings/trim-start s "-0")))

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
  "Formats a number intelligently within length constraints.
  
  Automatically chooses between fixed-point and scientific notation to
  produce the most readable result within `max-length` characters.
  Handles special values (NaN, Infinity) appropriately.
  
  Options:
    ::max-decimal-places - Limit decimal precision
    ::max-digits - Limit significant digits in scientific notation
  
  Examples:
    (format-number 1234.5678 8)           ;=> \"1234.568\"
    (format-number 0.000123 8)           ;=> \"1.23E-4\"  
    (format-number 1234567890 8)         ;=> \"1.235E+9\"
    (format-number ##NaN 5)              ;=> \"NaN\""
  ([number max-length] (format-number number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits]}]
   (let [number (double number)]
     (cond
       (m/nan? number) "NaN"
       (m/inf+? number) "Inf"
       (m/inf-? number) "-Inf"

       :else
       (let [rounded-number (double
                              (if max-decimal-places
                                (read-string
                                  (format-as-float
                                    number max-decimal-places))
                                number))
             rounded-number-as-string (if max-digits
                                        (format-as-exponential
                                          rounded-number {::digits max-digits})
                                        (str rounded-number))
             shortened-number (double (if max-digits
                                        (read-string rounded-number-as-string)
                                        rounded-number))
             new-max-length (min max-length (count rounded-number-as-string))
             standard-digits (count
                               (str (m/round shortened-number :toward-zero)))
             want-exponential? (or (and (> standard-digits new-max-length)
                                     (> standard-digits
                                       (+ 5.5
                                         (* -0.5 (m/sgn shortened-number)))))
                                 (< (m/abs shortened-number) 0.0001))]
         (loop [i (max new-max-length 1)]
           (let [s (if want-exponential?
                     (format-as-exponential shortened-number {::digits i})
                     (format-as-float shortened-number (dec i)))]
             (if (or (<= i 1) (<= (count s) new-max-length))
               s
               (recur (dec i))))))))))

(s/fdef format-number
  :args (s/cat :number ::m/number
          :max-length ::max-length
          :opts (s/? (s/keys :opt [::max-decimal-places ::max-digits])))
  :ret string?)

;;;SHORTHAND
(defn unparse-shorthand
  "Converts numbers to shorthand notation with K, M, B, T suffixes.
  
  Formats large numbers using shorthand suffixes (K=thousands, M=millions, 
  B=billions, T=trillions) within the specified `max-length` character limit.
  Automatically falls back to standard formatting for very large numbers or 
  when shorthand doesn't provide space savings.
  
  Parameters:
    max-length - Maximum number of characters in the output string
  
  Options:
    ::max-decimal-places - Limit decimal precision
    ::max-digits - Limit significant digits in scientific notation  
    ::money? - Add $ prefix for currency formatting
  
  Examples:
    (unparse-shorthand 1500 5)                    ;=> \"1.5K\"
    (unparse-shorthand 2300000 6)                 ;=> \"2.3M\"
    (unparse-shorthand 1500 5 {::money? true})    ;=> \"$1.5K\"
    (unparse-shorthand -500000 7 {::money? true}) ;=> \"-$500K\"
    (unparse-shorthand 42 5)                      ;=> \"42\"
    (unparse-shorthand ##NaN 5)                   ;=> \"NaN\""
  ([number max-length] (unparse-shorthand number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits money?]}]
   (cond
     (m/nan? number) "NaN"
     (m/inf+? number) "Inf"
     (m/inf-? number) "-Inf"

     :else
     (let [rounded-number (double
                            (if max-decimal-places
                              (read-string
                                (format-as-float number max-decimal-places))
                              number))
           shortened-number (if max-digits
                              (read-string
                                (format-as-exponential
                                  rounded-number {::digits max-digits}))
                              rounded-number)
           absolute-value (m/abs shortened-number)
           f (fn [x letter]
               (let [adjusted-number (str (format-number
                                            (/ x (by-letter letter))
                                            (max 1 (dec max-length))))
                     without-ending (if (str/ends-with? adjusted-number ".0")
                                      (strings/butlast-string
                                        (strings/butlast-string
                                          adjusted-number))
                                      adjusted-number)]
                 (str without-ending letter)))
           s (cond (>= absolute-value 1e15)
               (format-number shortened-number max-length)

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
          :opts (s/? (s/keys :opt [::max-decimal-places
                                   ::max-digits
                                   ::money?])))
  :ret string?)

(defn parse-shorthand
  "Parses shorthand number strings back to numeric values.
  
  Converts formatted strings with suffixes (K, M, B, T) and money symbols ($)
  back to their numeric equivalents. Returns nil if parsing fails.
  
  Handles special values and all formats produced by [[unparse-shorthand]].
  
  Examples:
    (parse-shorthand \"1.5K\")    ;=> 1500.0
    (parse-shorthand \"$2.3M\")   ;=> 2300000.0  
    (parse-shorthand \"-$500K\")  ;=> -500000.0
    (parse-shorthand \"NaN\")     ;=> ##NaN
    (parse-shorthand \"42\")      ;=> 42
    (parse-shorthand \"invalid\") ;=> nil"
  [s]
  (let [removed-money (cond (str/starts-with? s "$") (strings/rest-string s)

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

