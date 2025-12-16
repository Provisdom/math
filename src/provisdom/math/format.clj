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
    [provisdom.utility-belt.anomalies :as anomalies]
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
(s/def ::precision ::decimal-places)
(s/def ::thousands-sep? boolean?)
(s/def ::engineering? boolean?)

(s/def ::rounding-mode
  #{:ceiling :floor :half-down :half-even :half-up})

(s/def ::decimal-symbol char?)
(s/def ::grouping-symbol char?)

(s/def ::shorthand-letters
  (s/map-of string? ::m/pos))

;;;CONSTANTS
(def ^:private ^:const exponential-threshold-base
  "Base threshold for standard digit count before switching to exponential notation.
  Numbers with more than this many digits (adjusted for sign) use exponential format."
  5.5)

(def ^:private ^:const exponential-sign-adjustment
  "Adjustment factor for the sign of the number.
  Positive numbers: threshold = 5.5 - 0.5 = 5
  Negative numbers: threshold = 5.5 + 0.5 = 6 (accounts for minus sign)"
  -0.5)

(def ^:private ^:const small-number-exponential-threshold
  "Numbers with absolute value below this threshold use exponential notation.
  This corresponds to 10^-4."
  0.0001)

(def by-letter
  "Default shorthand letter multipliers for K, M, B, T suffixes."
  {"B" 1000000000
   "K" 1000
   "M" 1000000
   "T" 1000000000000})

(def ^:private by-letter-sorted
  "Shorthand letters sorted by value descending for iteration."
  [["T" 1000000000000]
   ["B" 1000000000]
   ["M" 1000000]
   ["K" 1000]])

;;;PRIVATE HELPERS
(defn- safe-parse-number
  "Safely parses a string to a number without code execution risk.
  Returns nil if parsing fails. Uses Double/parseDouble instead of read-string."
  [s]
  (when (and (string? s) (not (str/blank? s)))
    (try
      (Double/parseDouble s)
      (catch NumberFormatException _ nil))))

(defn- scientific-notation?
  "Returns true if string appears to be scientific notation (e.g., '1.23E+4')."
  [s]
  (boolean (re-matches #"-?[\d.]+[Ee][+-]?\d+" s)))

(defn- normalize-exponent
  "Normalizes exponent notation by removing leading zeros from multi-digit exponents.
  E+01 -> E+1, E-01 -> E-1, but E+0 and E-0 are preserved."
  [s]
  (-> s
      (str/replace #"E\+0+(\d)" "E+$1")
      (str/replace #"E-0+(\d)" "E-$1")))

(defn- use-exponential?
  "Determines whether exponential notation should be used for the given number."
  [standard-digits new-max-length shortened-number]
  (let [digit-threshold (+ exponential-threshold-base
                          (* exponential-sign-adjustment (m/sgn shortened-number)))]
    (or (and (> standard-digits new-max-length)
             (> standard-digits digit-threshold))
        (< (m/abs shortened-number) small-number-exponential-threshold))))

(defn- java-rounding-mode
  "Converts keyword rounding mode to Java RoundingMode."
  ^java.math.RoundingMode [mode]
  (case mode
    :ceiling java.math.RoundingMode/CEILING
    :floor java.math.RoundingMode/FLOOR
    :half-down java.math.RoundingMode/HALF_DOWN
    :half-even java.math.RoundingMode/HALF_EVEN
    :half-up java.math.RoundingMode/HALF_UP
    java.math.RoundingMode/HALF_UP))

(defn- add-thousands-separators
  "Adds thousand separators (commas) to the integer portion of a number string."
  [s]
  (let [[sign rest-s] (if (str/starts-with? s "-")
                        ["-" (subs s 1)]
                        ["" s])
        [int-part dec-part] (str/split rest-s #"\." 2)
        int-with-sep (loop [chars (reverse int-part)
                            result []
                            cnt 0]
                       (if (empty? chars)
                         (apply str (reverse result))
                         (let [c (first chars)
                               ;; Add comma before this digit if we're at position 3, 6, 9, etc.
                               new-result (if (and (pos? cnt) (zero? (mod cnt 3)))
                                            (conj result \, c)
                                            (conj result c))]
                           (recur (rest chars) new-result (inc cnt)))))]
    (str sign int-with-sep (when dec-part (str "." dec-part)))))

(defn- localize-number-string
  "Replaces decimal and grouping symbols based on locale options.
  Uses a placeholder to avoid conflicts when swapping symbols."
  [s {::keys [decimal-symbol grouping-symbol]}]
  (if (or decimal-symbol grouping-symbol)
    (let [;; Use a placeholder to avoid conflicts when both symbols are being swapped
          placeholder "\u0000"
          s1 (if decimal-symbol
               (str/replace s "." placeholder)
               s)
          s2 (if grouping-symbol
               (str/replace s1 "," (str grouping-symbol))
               s1)
          s3 (if decimal-symbol
               (str/replace s2 placeholder (str decimal-symbol))
               s2)]
      s3)
    s))

(defn trim-number-as-string
  "Removes unnecessary characters from a number string.

  Eliminates leading/trailing zeros and normalizes negative zero representations.
  Useful for cleaning up formatted number output.

  Examples:
    (trim-number-as-string \"0.30\")    ;=> \"0.3\"
    (trim-number-as-string \"-0.0\")    ;=> \"0\"
    (trim-number-as-string \"000123\")  ;=> \"123\"
    (trim-number-as-string \"5.000\")   ;=> \"5\"
    (trim-number-as-string \"-00.003\") ;=> \"-.003\""
  [s]
  (let [negative? (str/starts-with? s "-")
        s-abs (if negative? (subs s 1) s)
        ;; Handle decimal trailing zeros
        s-trimmed (if (str/includes? s-abs ".")
                    (-> s-abs
                        (strings/trim-end "0")
                        (strings/trim-end "."))
                    s-abs)
        ;; Trim leading zeros (this removes all leading zeros)
        s-no-leading (strings/trim-start s-trimmed "0")
        ;; Handle edge case where result is empty
        s-final (if (empty? s-no-leading) "0" s-no-leading)]
    ;; Only add negative sign if result is non-zero
    (if (and negative? (not= s-final "0"))
      (str "-" s-final)
      s-final)))

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
     (normalize-exponent
       (format (str "%." (long (dec digits)) "E") (double finite)))
     (let [s (normalize-exponent
               (format (str "%." 15 "E") (double finite)))]
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
                                (safe-parse-number
                                  (format-as-float
                                    number max-decimal-places))
                                number))
             rounded-number-as-string (if max-digits
                                        (format-as-exponential
                                          rounded-number {::digits max-digits})
                                        (str rounded-number))
             shortened-number (double (if max-digits
                                        (safe-parse-number rounded-number-as-string)
                                        rounded-number))
             new-max-length (min max-length (count rounded-number-as-string))
             standard-digits (count
                               (str (m/round shortened-number :toward-zero)))
             want-exponential? (use-exponential? standard-digits new-max-length shortened-number)]
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
                              (safe-parse-number
                                (format-as-float number max-decimal-places))
                              number))
           shortened-number (if max-digits
                              (safe-parse-number
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
  back to their numeric equivalents. Also handles scientific notation (e.g., '1.23E+4').
  Returns an anomaly if parsing fails.

  Handles special values and all formats produced by [[unparse-shorthand]].

  Examples:
    (parse-shorthand \"1.5K\")    ;=> 1500.0
    (parse-shorthand \"$2.3M\")   ;=> 2300000.0
    (parse-shorthand \"-$500K\")  ;=> -500000.0
    (parse-shorthand \"1.23E+4\") ;=> 12300.0
    (parse-shorthand \"NaN\")     ;=> ##NaN
    (parse-shorthand \"42\")      ;=> 42
    (parse-shorthand \"invalid\") ;=> {::anomalies/category ::anomalies/incorrect ...}"
  [s]
  (if (or (nil? s) (str/blank? s))
    {::anomalies/category ::anomalies/incorrect
     ::anomalies/message "Cannot parse empty or blank string"
     ::anomalies/data {:input s}}
    (let [removed-money (cond (str/starts-with? s "$") (strings/rest-string s)

                          (str/starts-with? s "-$")
                          (str "-" (strings/trim-start s "-$"))

                          :else s)]
      (case removed-money
        "NaN" m/nan
        "Inf" m/inf+
        "-Inf" m/inf-
        (let [f (fn [x letter]
                  (when-let [n (safe-parse-number (strings/butlast-string x))]
                    (* (by-letter letter) n)))
              result (cond
                       ;; Handle scientific notation first
                       (scientific-notation? removed-money)
                       (safe-parse-number removed-money)

                       (str/ends-with? removed-money "T") (f removed-money "T")
                       (str/ends-with? removed-money "B") (f removed-money "B")
                       (str/ends-with? removed-money "M") (f removed-money "M")
                       (str/ends-with? removed-money "K") (f removed-money "K")
                       :else (safe-parse-number removed-money))]
          (if (some? result)
            result
            {::anomalies/category ::anomalies/incorrect
             ::anomalies/message "Failed to parse number string"
             ::anomalies/data {:input s}}))))))

(s/fdef parse-shorthand
  :args (s/cat :s string?)
  :ret (s/or :anomaly ::anomalies/anomaly
             :number ::m/number))

;;;PERCENT
(defn format-percent
  "Converts decimal to percentage string.

  Options:
    ::precision - Decimal places (default 0)

  Examples:
    (format-percent 0.15)                    ;=> \"15%\"
    (format-percent 0.156 {::precision 1})   ;=> \"15.6%\"
    (format-percent 1.5)                     ;=> \"150%\"
    (format-percent ##NaN)                   ;=> \"NaN\""
  ([decimal] (format-percent decimal {}))
  ([decimal {::keys [precision] :or {precision 0}}]
   (cond
     (m/nan? decimal) "NaN"
     (m/inf+? decimal) "Inf"
     (m/inf-? decimal) "-Inf"
     :else (str (format-as-float (* 100 decimal) precision) "%"))))

(s/fdef format-percent
  :args (s/cat :decimal ::m/number
               :opts (s/? (s/keys :opt [::precision])))
  :ret string?)

;;;ENGINEERING NOTATION
(defn format-as-engineering
  "Formats `finite` into engineering notation (exponent divisible by 3).

  Engineering notation uses exponents that are multiples of 3, making it
  easier to read with SI prefixes (kilo, mega, giga, etc.).

  Options:
    ::digits - Number of significant digits (default 3)

  Examples:
    (format-as-engineering 12345)              ;=> \"12.345E+3\"
    (format-as-engineering 0.00123)            ;=> \"1.23E-3\"
    (format-as-engineering 1234567 {::digits 4}) ;=> \"1.235E+6\""
  ([finite] (format-as-engineering finite {}))
  ([finite {::keys [digits] :or {digits 3}}]
   (let [value (double finite)]
     (if (zero? value)
       "0E+0"
       (let [exp (m/floor (m/log10 (m/abs value)))
             eng-exp (* 3 (long (m/floor (/ exp 3))))
             mantissa (/ value (m/pow 10.0 eng-exp))
             decimal-places (max 0 (dec digits))]
         (str (format (str "%." decimal-places "f") mantissa)
              "E" (if (>= eng-exp 0) "+" "") eng-exp))))))

(s/fdef format-as-engineering
  :args (s/cat :finite ::m/finite
               :opts (s/? (s/keys :opt [::digits])))
  :ret string?)

;;;EXTENDED FORMATTING
(defn format-number-extended
  "Extended number formatting with additional options.

  Builds on [[format-number]] with support for thousand separators,
  engineering notation, rounding mode control, and localization.

  Options:
    ::max-decimal-places - Limit decimal precision
    ::max-digits - Limit significant digits in scientific notation
    ::engineering? - Use engineering notation (exponents divisible by 3)
    ::thousands-sep? - Add thousand separators (commas)
    ::rounding-mode - One of :ceiling, :floor, :half-down, :half-even, :half-up
    ::decimal-symbol - Custom decimal separator character
    ::grouping-symbol - Custom grouping separator character

  Examples:
    (format-number-extended 1234567 10 {::thousands-sep? true})
    ;=> \"1,234,567\"
    (format-number-extended 12345 8 {::engineering? true})
    ;=> \"12.345E+3\""
  ([number max-length] (format-number-extended number max-length {}))
  ([number max-length {::keys [decimal-symbol
                               engineering?
                               grouping-symbol
                               max-decimal-places
                               max-digits
                               rounding-mode
                               thousands-sep?]
                       :or {rounding-mode :half-up}}]
   (let [number (double number)]
     (cond
       (m/nan? number) "NaN"
       (m/inf+? number) "Inf"
       (m/inf-? number) "-Inf"

       :else
       (let [;; Apply rounding mode if specified
             rounded (if (and max-decimal-places (not= rounding-mode :half-up))
                       (let [bd (BigDecimal/valueOf number)
                             scale max-decimal-places
                             rm (java-rounding-mode rounding-mode)]
                         (.doubleValue (.setScale bd scale rm)))
                       number)
             ;; Use engineering notation if requested
             base-result (if engineering?
                           (format-as-engineering rounded {::digits (or max-digits 3)})
                           (format-number rounded max-length
                                          (cond-> {}
                                            max-decimal-places (assoc ::max-decimal-places max-decimal-places)
                                            max-digits (assoc ::max-digits max-digits))))
             ;; Add thousand separators if requested (only for non-exponential)
             with-thousands (if (and thousands-sep?
                                     (not (str/includes? base-result "E")))
                              (add-thousands-separators base-result)
                              base-result)
             ;; Apply localization
             localized (if (or decimal-symbol grouping-symbol)
                         (localize-number-string with-thousands
                                                 {::decimal-symbol decimal-symbol
                                                  ::grouping-symbol grouping-symbol})
                         with-thousands)]
         localized)))))

(s/fdef format-number-extended
  :args (s/cat :number ::m/number
               :max-length ::max-length
               :opts (s/? (s/keys :opt [::decimal-symbol
                                        ::engineering?
                                        ::grouping-symbol
                                        ::max-decimal-places
                                        ::max-digits
                                        ::rounding-mode
                                        ::thousands-sep?])))
  :ret string?)

;;;CUSTOM SHORTHAND
(defn unparse-shorthand-custom
  "Like [[unparse-shorthand]] but with customizable shorthand letters.

  Options:
    ::shorthand-letters - Map of letter to multiplier (default by-letter)
    ::max-decimal-places - Limit decimal precision
    ::max-digits - Limit significant digits
    ::money? - Add $ prefix

  Examples:
    (unparse-shorthand-custom 1500 5 {::shorthand-letters {\"k\" 1000}})
    ;=> \"1.5k\""
  ([number max-length] (unparse-shorthand-custom number max-length {}))
  ([number max-length {::keys [max-decimal-places max-digits money? shorthand-letters]
                       :or {shorthand-letters by-letter}}]
   (cond
     (m/nan? number) "NaN"
     (m/inf+? number) "Inf"
     (m/inf-? number) "-Inf"

     :else
     (let [rounded-number (double
                            (if max-decimal-places
                              (safe-parse-number
                                (format-as-float number max-decimal-places))
                              number))
           shortened-number (if max-digits
                              (safe-parse-number
                                (format-as-exponential
                                  rounded-number {::digits max-digits}))
                              rounded-number)
           absolute-value (m/abs shortened-number)
           ;; Sort letters by value descending
           sorted-letters (sort-by second > shorthand-letters)
           f (fn [x letter multiplier]
               (let [adjusted-number (str (format-number
                                            (/ x multiplier)
                                            (max 1 (dec max-length))))
                     without-ending (if (str/ends-with? adjusted-number ".0")
                                      (subs adjusted-number 0 (- (count adjusted-number) 2))
                                      adjusted-number)]
                 (str without-ending letter)))
           s (or (some (fn [[letter multiplier]]
                         (when (>= absolute-value multiplier)
                           (f shortened-number letter multiplier)))
                       sorted-letters)
                 (format-number shortened-number max-length))]
       (if money?
         (if (neg? shortened-number)
           (str "-$" (strings/rest-string s))
           (str "$" s))
         s)))))

(s/fdef unparse-shorthand-custom
  :args (s/cat :number ::m/number
               :max-length ::max-length
               :opts (s/? (s/keys :opt [::max-decimal-places
                                        ::max-digits
                                        ::money?
                                        ::shorthand-letters])))
  :ret string?)
