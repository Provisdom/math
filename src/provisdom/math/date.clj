(ns provisdom.math.date
  (:require [provisdom.utility-belt
             [core :as co]
             [format :as fo]]
            [provisdom.math
             [core :as m]
             [calculus :as ca]
             [matrix :as mx]
             [core-specs :as specs]]
            [clj-time
             [core :as ti]
             [format :as tf]]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.spec :as s])
  (:import (java.util Date)))

(set! *warn-on-reflection* true)

(declare instant$)

;;;;leap-day every 4 years excluding years divisible by 100, 
;;;;     plus years divisible by 400
;;;;400 years = 480 months = 20,871 weeks = 146,097 days
;;;;146097*24*60*60*1000000*11*13*8=ticks in 400 years 
;;;;   -- chosen to have 400 years be divisible by microseconds, 
;;;;      and is divisible by 2^12 and all numbers through 16
(def ^:const ^:private date-2045 -902522649600000000)
(def ^:const ^:private date-2015 -1985628902400000000)
(def ^:const ^:private date-1970 -3610189440000000000)
(def ^:const unix-epoch "1970" 1970)
(def ^:const epoch "2070" 2070)
(def ^:const average-weeks-per-year "52.1775" 52.1775)
(def ^:const average-days-per-year "365.2425" 365.2425)
(def ^:const average-weeks-per-month "4.348125" 4.348125)
(def ^:const average-days-per-month "30.436875" 30.436875)
(def ^:const average-year 36101153088000000)
(def ^:const average-month 3008429424000000)
(def ^:const week 691891200000000)
(def ^:const day 98841600000000)
(def ^:const hour 4118400000000)
(def ^:const minute 68640000000)
(def ^:const sec 1144000000)
(def ^:const millisec 1144000)
(def ^:const microsec "1144" 1144)

;;;LEAP YEARS
(def ^:const ^:private non-leap-year-days-per-month
  [31 28 31 30 31 30 31 31 30 31 30 31])

(def ^:const ^:private non-leap-year-days-until-month
  [0 31 59 90 120 151 181 212 243 273 304 334])

(defn leap-year?
  "Returns whether a supplied year is a leap year"
  [^long year]
  (or (zero? (mod year 400)) (and (zero? (mod year 4))
                                  (not (zero? (mod year 100))))))

(s/fdef leap-year?
        :args (s/cat :year ::specs/year)
        :ret boolean?)

(defn days-per-month
  "Returns the number of days in a supplied month"
  ^long [^long year ^long month]
  {:pre [(have? pos? month) (have? #(<= % 12) month)]}
  (if (and (== month 2) (leap-year? year))
    29
    (non-leap-year-days-per-month (dec month))))

(s/fdef days-per-month
        :args (s/cat :year ::specs/year :month ::specs/month)
        :ret ::specs/days-per-month)

(defn days-until-month
  "Returns the number of days in a particular year until a supplied month starts"
  ^long [^long year ^long month]
  {:pre [(have? pos? month) (have? #(<= % 12) month)]}
  (let [leap (if (and (>= month 3) (leap-year? year)) 1 0)]
    (+ leap (non-leap-year-days-until-month (dec month)))))

(s/fdef days-until-month
        :args (s/cat :year ::specs/year :month ::specs/month)
        :ret (s/int-in 0 335))

(defn- passed-leap-days-since-2000
  ^long [^long year ^long month]
  {:pre [(have? pos? month) (have? #(<= % 12) month)]}
  (let [y (- year 2000), [a1 y1] (m/quot-and-mod y 400),
        [a2 y2] (m/quot-and-mod y1 100),
        [a3 y3] (m/quot-and-mod y2 4), d (+ (* 97 a1) (* 24 a2) a3 1),
        extra (if (and (leap-year? year) (<= month 2)) -1 0)]
    (+ d extra)))

(defn passed-leap-days
  "Returns the number of passed leap days between two dates, or since epoch"
  (^long [^long year ^long month]
   (passed-leap-days epoch 1 year month))
  (^long [^long year1 ^long month1 ^long year2 ^long month2]
   (- (passed-leap-days-since-2000 year2 month2)
      (passed-leap-days-since-2000 year1 month1))))

(s/fdef passed-leap-days
        :args (s/? (s/or (s/cat :year ::specs/year :month ::specs/month)
                         (s/cat :year1 ::specs/year :month1 ::specs/month :year2 ::specs/year :month2 ::specs/month)))
        :ret (s/int-in 0 127))                              ;;127 is perhaps slightly generous cap

;;;READING
(def ^:const full-date-map
  "Helper for supplying the default full map when adding a time-zone"
  [:hr :mi :se :ms :us])

(defn- read-f [read key div num]
  (if (some #{key} read) (m/quot-and-rem num div) [nil num]))

(defn read-ticks
  "Ticks can be read as a map of the keys :ti (ticks), and those supplied from 
   the 'read' coll as a subset of the keywords 
      :wk (weeks) :da (days) :hr (hours) :mi (minutes) :se (seconds) 
      :ms (milliseconds) :us (microseconds)."
  ([^long ticks] (read-ticks ticks (vector :wk :da :hr :mi :se :ms :us)))
  ([^long ticks read]
   (let [[wk ti] (read-f read :wk week ticks),
         [da ti] (read-f read :da day ti),
         [hr ti] (read-f read :hr hour ti),
         [mi ti] (read-f read :mi minute ti),
         [se ti] (read-f read :se sec ti),
         [ms ti] (read-f read :ms millisec ti),
         [us ti] (read-f read :us microsec ti)]
     (reduce (fn [tot [k v]] (if v (assoc tot k v) tot)) {}
             [[:wk wk] [:da da] [:hr hr] [:mi mi] [:se se] [:ms ms] [:us us]
              [:ti ti]]))))

(defn read-duration
  "A duration can be read as a map of the keys :mo (months), :ti (ticks), 
   and those supplied from the 'read' coll as a subset of the keywords 
      :yr (years), :wk (weeks), :da (days), :hr (hours), :mi (minutes), 
      :se (seconds), :ms (milliseconds), :us (microseconds)."
  ([[^long months ^long ticks]]
   (read-duration [months ticks] (vector :yr :wk :da :hr :mi :se :ms :us)))
  ([[^long months ^long ticks] read]
   (let [[yr mo] (read-f read :yr 12 months), result (read-ticks ticks read),
         result (if yr (assoc result :yr yr) result)] (assoc result :mo mo))))

(defn read-date
  "A date can be read as a map of the keys :yr (year), :mo (month), :da (day), 
   :ti (tick), :tz (time zone), and those supplied from the 'read' coll as a 
   subset of the keywords :hr (hour) :mi (minute) :se (second) 
   :ms (millisecond), :us (microsecond).
Note that month and day are 1-indexed while the rest are 0-indexed."
  ([^long d] (read-date d (vector :hr :mi :se :ms :us)))
  ([^long d read] (read-date d read 0))
  ([^long d read time-zone]
    ;;probably a little faster starting from 2015 than 2070 most of the time
   (if (< d date-2045)
     (read-date 2015 1 1 (- d date-2015) read time-zone)
     (read-date epoch 1 1 d read time-zone)))
  ([^long year ^long month ^long day-number ^long tick]
   (read-date year month day-number tick (vector :hr :mi :se :ms :us)))
  ([year month day-number tick read]
   (read-date year month day-number tick read 0))
  ([year month day-number tick read time-zone]
   {:pre [(have? m/long-able? year month day-number tick time-zone)]}
   (let [[da ti] (m/quot-and-mod (long tick) day),
         da (long (+ day-number da)),
         [yr mo] (m/quot-and-mod (dec (long month)) 12),
         yr (long (+ year yr)),
         mo (inc mo),
         [da mo yr] (loop [da da, mo mo, yr yr]
                      (let [dpm (days-per-month yr mo)]
                        (if (> da dpm)
                          (recur (- da dpm)
                                 (if (== mo 12) 1 (inc mo))
                                 (if (== mo 12) (inc yr) yr))
                          [da mo yr]))),
         [da mo yr] (loop [da da, mo mo, yr yr]
                      (if (m/non+? da)
                        (let [new-mo (if (m/one? mo) 12 (dec mo))]
                          (recur (+ da (days-per-month yr new-mo)) new-mo
                                 (if (m/one? mo) (dec yr) yr))) [da mo yr]))]
     (merge {:yr yr, :mo mo, :da da, :tz time-zone} (read-ticks ti read)))))

(defn day-of-week
  "From a supplied date, returns the day of the week as a keyword 
   :mo, :tu, :we, :th, :fr, :sa, :su"
  [^long d]
  (let [dow [:we :th :fr :sa :su :mo :tu]]
    (nth dow (m/mod' (:da (read-ticks d [:da])) 7))))

;;;CONVERTERS
(defn time-millis$
  "Returns a TimeMillis of now.  Loses precision below millisecond"
  ^long [] (.getTime ^Date (instant$)))

(defn time-millis
  "Returns a TimeMillis.  Loses precision below millisecond"
  ^long [^long d] (m/round (/ (- d date-1970) millisec)))

(defn time-millis->date
  "Converts time-millis to date"
  ^long [^long tm] (+ date-1970 (* millisec tm)))

(defn instant$
  "Returns an instant literal of now.  Loses precision below millisecond."
  [] (Date.))

(defn instant
  "Returns an instant literal.  Loses precision below millisecond."
  [^long d] (Date. ^long (time-millis d)))

(defn instant->date
  "Converts an instant literal to a date"
  ^long [ins] (time-millis->date (.getTime ^Date ins)))

(defn- subtract-joda->ticks
  ^long [dt1 dt2]
  (let [a? (ti/after? dt1 dt2),
        i (if a? (ti/interval dt2 dt1) (ti/interval dt1 dt2)),
        ti (* millisec (ti/in-millis i)), ti (if a? ti (- ti))]
    ti))

(defn- get-time-zone ^long [joda-date-time]
  (/ (subtract-joda->ticks
       (ti/from-time-zone joda-date-time
                          (ti/time-zone-for-offset 0)) joda-date-time) hour))

(defn joda->date-with-time-zone
  "Returns a tuple of a date and time-zone from a joda date-time"
  [joda-date-time]
  [(subtract-joda->ticks joda-date-time (ti/date-time epoch))
   (get-time-zone joda-date-time)])

(defn joda
  "Returns a joda date-time.  
Any precision higher than milliseconds is dropped."
  ([^long d ^long time-zone]
   (let [r (read-date d [:ms]), ms (m/round (+ (:ms r) (/ (:ti r) millisec)))]
     (joda (:yr r) (:mo r) (:da r) ms time-zone)))
  ([year month day-number millisecond time-zone]
   {:pre [(have? m/long-able? year month day-number millisecond time-zone)]}
   (let [{hr :hr, mi :mi, se :se, ms :ms, ti :ti}
         (read-ticks (* millisec (long millisecond))
                     (vector :hr :mi :se :ms))]
     (ti/to-time-zone
       (ti/date-time (long year) (long month) (long day-number) hr mi se ms)
       (ti/time-zone-for-offset (long time-zone))))))

;;;FACTORIES
(defn period
  "Returns period from ticks or interval"
  ^double [ticks-or-interval]
  (if (number? ticks-or-interval)
    (/ ticks-or-interval average-year)
    (period (- (second ticks-or-interval) (first ticks-or-interval)))))

(s/fdef period
        :args (s/cat :ticks-or-interval (s/or :ticks ::specs/ticks :interval ::specs/interval))
        :ret ::specs/period)

(defn as-ticks
  "Returns ticks as a long.  
Ticks are often used to represent durations that have zero months."
  (^long [^long weeks] (as-ticks weeks 0 0))
  (^long [^long weeks ^long days] (as-ticks weeks days 0))
  ([weeks days ticks
    & {:keys [hr mi se ms us] :or {hr 0, mi 0, se 0, ms 0, us 0}}]
   {:pre [(have? m/long-able? weeks days ticks)]}
   (+ (* (long weeks) week) (* (long days) day) (* hr hour) (* mi minute)
      (* se sec) (* ms millisec) (* us microsec) (long ticks))))

(s/fdef as-ticks
        :args (s/? (s/or (s/cat :weeks ::specs/weeks)
                         (s/cat :weeks ::specs/weeks :days ::specs/days)
                         (s/cat :weeks ::specs/weeks :days ::specs/days :ticks ::specs/ticks
                                :opts (s/keys* :opt-un [::specs/hr ::specs/mi ::specs/se ::specs/ms ::specs/us]))))
        :ret ::specs/ticks)

(defn maybe-duration-as-ticks
  "Returns ticks as a long if duration has zero months."
  [[^long months ^long ticks]]
  (if (zero? months) ticks nil))

(defn duration
  "A duration is a long tuple of months and ticks.  
For durations that do not depend on the calendar, 
   months will be set to zero and durations can be represented by just ticks."
  ([^long years] (duration years 0 0 0))
  ([^long years ^long months] (duration years months 0 0))
  ([^long years ^long months ^long days] (duration years months days 0))
  ([years months days ticks
    & {:keys [wk hr mi se ms us] :or {wk 0, hr 0, mi 0, se 0, ms 0, us 0}}]
   {:pre [(have? m/long-able? years months days ticks)]}
   [(+ (long months) (* (long years) 12))
    (+ (as-ticks 0 0 (long ticks) :hr hr :mi mi :se se :ms ms :us us)
       (* wk week) (* (long days) day))]))

(s/fdef duration
        :args (s/? (s/or (s/cat :years ::specs/years)
                         (s/cat :years ::specs/years :months ::specs/months)
                         (s/cat :years ::specs/years :months ::specs/months :days ::specs/days)
                         (s/cat :years ::specs/years :months ::specs/months :days ::specs/days :ticks ::specs/ticks
                                :opts (s/keys* :opt-un [::specs/wk ::specs/hr ::specs/mi ::specs/se ::specs/ms
                                                        ::specs/us]))))
        :ret ::specs/duration)

(defn date$
  "Now, returned to the nearest millisecond"
  ^long [] (first (joda->date-with-time-zone (ti/now))))

(defn date
  "A date is a long in tick units representing the number of ticks starting from 'epoch' in the UTC time zone.
A date is often paired as a tuple with a long representing the time zone offset in hours from the UTC.
A date must be later than 7/8/1814 and earlier than 6/29/2325.
Note that month and day are 1-indexed while the rest are 0-indexed."
  (^long [^long year] (date year 1 1 0))
  (^long [^long year ^long month] (date year month 1 0))
  (^long [^long year ^long month ^long day-number] (date year month day-number 0))
  ([year month day-number ticks
    & {:keys [hr mi se ms us] :or {hr 0, mi 0, se 0, ms 0, us 0}}]
   {:pre [(have? m/long-able? year month day-number ticks)]}
   (let [{yr :yr, mo :mo, da :da, ti :ti}
         (read-date (long year) (long month) (long day-number)
                    (as-ticks 0 0 (long ticks) :hr hr :mi mi :se se :ms ms :us us)
                    []),
         td (+ (passed-leap-days yr mo) (* 365 (- yr epoch))
               (days-until-month yr mo) (dec da))]
     (+ (* day td) ti))))

(s/fdef date                                                ;;also: A date must be entered as later than 7/8/1814 and earlier than 6/29/2325.
        :args (s/? (s/or (s/cat :year ::specs/year)
                         (s/cat :year ::specs/year :month ::specs/month)
                         (s/cat :year ::specs/year :month ::specs/month :day-number ::specs/day-number)
                         (s/cat :year ::specs/year :month ::specs/month :day-number ::specs/day-number
                                :ticks ::specs/ticks
                                :opts (s/keys* :opt-un [::specs/hr ::specs/mi ::specs/se ::specs/ms ::specs/us]))))
        :ret ::specs/date)

(def year "A year's duration" (duration 1))
(def month "A month's duration" (duration 0 1))

;;;DATE MANIPULATION
(defn- add-date-and-durations
  "Adds one or more durations to a date and returns a new date."
  ([^long d [^long months ^long ticks]]
   (let [r (read-date d []), r (assoc r :mo (+ (:mo r) months)),
         r (assoc r :ti (+ (:ti r) ticks))]
     (date (:yr r) (:mo r) (:da r) (:ti r))))
  ([d dur & durs]
   {:pre [(have? m/long-able? d)]}
   (let [dus (conj durs dur), months (mx/esum (map first dus)),
         ticks (mx/esum (map second dus)), r (read-date (long d) []),
         r (assoc r :mo (+ (:mo r) months)),
         r (assoc r :ti (+ (:ti r) ticks))]
     (date (:yr r) (:mo r) (:da r) (:ti r)))))

(defn add-duration
  "Add dates and durations, or multiple durations.  
To add dates with ticks, use '+'"
  ([d1 d2]
    #_{:pre [(have? #(or (vector %1) (vector %2)) d1 d2)]}
   (if (vector? d1)
     (if (vector? d2) (mx/add d1 d2)
                      (add-date-and-durations d2 d1))
     (if (vector? d2) (add-date-and-durations d1 d2)
                      (throw (ex-info "Can't add two dates." {:fn (var add-duration)})))))
  ([d1 d2 & ds] (reduce (fn [tot e] (add-duration tot e)) d1 (cons d2 ds))))

(defn add-ticks
  "Add ticks and durations.  To add dates with ticks, use '+'"
  ([d1 d2]
   (if (vector? d1)
     (if (vector? d2) (mx/add d1 d2) [(first d1) (+ (second d1) d2)])
     (if (vector? d2) [(first d2) (+ d1 (second d2))] (+ d1 d2))))
  ([d1 d2 & ds] (reduce (fn [tot e] (add-ticks tot e)) d1 (cons d2 ds))))

(defn- sub-durations-from-date
  "Subtracts one or more durations from a date and returns a new date."
  ([^long d [^long months ^long ticks]]
   (add-date-and-durations d [(- months) (- ticks)]))
  ([d dur & durs]
   (add-date-and-durations
     d [(- (first dur)) (- (second dur))]
     (map #(vector (- (first %)) (- (second %))) durs))))

(defn- interval
  "Subtracts two dates and returns a duration.  
To get the total ticks between dates, just subtract the dates without this
   function."
  [^long d1 ^long d2]
  (let [r1 (read-date d1 []), r2 (read-date d2 [])]
    (duration (- (:yr r1) (:yr r2)) (- (:mo r1) (:mo r2))
              (- (:da r1) (:da r2)) (- (:ti r1) (:ti r2)))))

(defn sub-dates
  "Subtract dates and durations. 
To subtract ticks from dates or to get the total ticks between dates, use '-'."
  ([d1 d2]
   (if (vector? d1)
     (if (vector? d2) (mx/sub d1 d2)
                      (throw (ex-info "Can't subtract date from duration." {:fn (var sub-dates)})))
     (if (vector? d2) (sub-durations-from-date d1 d2) (interval d1 d2))))
  ([d1 d2 & ds] (reduce (fn [tot e] (sub-dates tot e)) d1 (cons d2 ds))))

(defn sub-ticks
  "Subtract ticks and durations.  To subtract ticks from dates, use '+'"
  ([d1 d2]
   (if (vector? d1)
     (if (vector? d2) (mx/sub d1 d2)
                      [(first d1) (- (second d1) d2)])
     (if (vector? d2) [(- (first d2)) (- d1 (second d2))] (- d1 d2))))
  ([d1 d2 & ds] (reduce (fn [tot e] (sub-ticks tot e)) d1 (cons d2 ds))))

(defn mul
  "Multiplies a duration by a long"
  [d1 d2]
  {:pre [(have? (fn [[d1 d2]] (let [l1? (m/long-able? d1), l2? (m/long-able? d2)]
                                (and (or l1? l2?)
                                     (or l1? (first d1))
                                     (or l2? (first d2)))))
                [d1 d2])]}
  (let [r (mx/mul d1 d2)] (if (number? r) (long r) (vec (map long r)))))

(defn to-ticks
  "Returns ticks from a supplied duration and date
Date can represent the start or end time of the duration"
  ([[months ticks] start-date]
   (to-ticks [months ticks] start-date false))
  ([[months ticks] d end-date?]
   {:pre [(have? m/long-able? months ticks d)]}
   (let [months (long months), ticks (long ticks), d (long d)]
     (if end-date?
       (- d (sub-dates d [months ticks]))
       (- (add-duration d [months ticks]) d)))))

(defn to-duration
  "Returns ticks from a supplied duration and date
Date can represent the start or end time of the duration"
  ([^long ticks ^long start-date] (to-duration ticks start-date false))
  ([^long ticks ^long d end-date?]
   (if end-date? (sub-dates d (- d ticks)) (sub-dates (+ d ticks) d))))

;;;TIME ZONES
(defn environment-time-zone
  "Returns the current environment time zone.  
A time zone is a long representing the number of hours offset from the UTC time
   zone."
  ^long [] (get-time-zone (ti/to-time-zone (ti/epoch) (ti/default-time-zone))))

;;;PERIODIC
(defn lazy-period
  "Returns an infinite sequence of date values growing over a specific 
   duration"
  [^long start-date [^long months ^long ticks]]
  (iterate #(add-duration % [months ticks]) start-date))

(defn intervals
  "Returns a finite set of adjacent intervals"
  ([^long start ^long steps ticks-or-duration]
   (if (number? ticks-or-duration)
     (vec (map (fn [d] (vector d (+ d ticks-or-duration)))
               (range start (+ start (* steps ticks-or-duration)) ticks-or-duration)))
     (intervals identity start steps ticks-or-duration)))
  ([f ^long start ^long steps ticks-or-duration]
   (vec (if (number? ticks-or-duration)
          (map (fn [i] (vector (+ start (* (f i) ticks-or-duration))
                               (+ start (* (f (inc i)) ticks-or-duration))))
               (range steps))
          (map (fn [i] (vector
                         (add-duration start (mx/mul (f i) ticks-or-duration))
                         (add-duration
                           start (mx/mul (f (inc i)) ticks-or-duration))))
               (range steps))))))

(defn dates
  "Returns date values from a finite set of adjacent intervals"
  [intervals] (cons (ffirst intervals) (map second intervals)))

;;;FORMATTING
(def ^:const ^:private day-of-week-set
  #{[:mo "Monday" "Mon"] [:tu "Tuesday" "Tue"] [:we "Wednesday" "Wed"]
    [:th "Thursday" "Thu"] [:fr "Friday" "Fri"] [:sa "Saturday" "Sat"]
    [:su "Sunday" "Sun"]})

(def ^:const ^:private months-long
  ["January", "February", "March", "April", "May", "June", "July", "August",
   "September", "October", "November", "December"])

(def ^:const ^:private months-short
  ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
   "Nov", "Dec"])

(defn unparse-day-of-week
  "Unparses the day-or-week key as string.
Use 'day-of-week' to get key."
  ([key] (unparse-day-of-week key false))
  ([key short?]
   (let [d (first (filter #(= (first %) key) day-of-week-set))]
     (if short? (nth d 2) (second d)))))

(defn unparse-month
  "Unparses the month as string."
  ([month] (unparse-month month false))
  ([month short?]
   {:pre [(have? m/long-able? month)]}
   (if-not (and (pos? month) (<= month 12)) nil
                                            (if short? (nth months-short (dec month))
                                                       (nth months-long (dec month))))))

(defn- unparse-full-time
  ([time-map]
   (let [d time-map]
     (str "T" (:hr d 0) ":" (:mi d 0) ":" (:se d 0) "." (:ms d 0) "."
          (:us d 0) ":" (:ti d 0))))
  ([time-map ^long hr-precision]
   (if (m/non+? hr-precision) (unparse-full-time time-map)
                              (let [d time-map,
                                    ti (as-ticks 0 0 (:ti d 0) :hr (:hr d 0), :mi (:mi d 0),
                                                 :se (:se d 0), :ms (:ms d 0), :us (:us d 0)),
                                    fs (str "H%." hr-precision "f")]
                                (format fs (double (/ ti hour)))))))

(defn- unparse? [l] (and l (not (zero? l))))

(defn unparse-duration
  "Unparses the duration-map as string.  
Use 'read-duration' to get duration-map."
  ([duration-map] (unparse-duration duration-map 0))
  ([duration-map hr-precision]
   (let [d duration-map, yr (:yr d), wk (:wk d), da (:da d)]
     (str (if (unparse? yr) (str "Y" yr)) "M" (:mo d)
          (if (unparse? wk) (str "W" wk)) (if (unparse? da) (str "D" da))
          (unparse-full-time d hr-precision)))))

(defn unparse-ticks
  "Unparses the ticks-map as string.
Use 'read-ticks' to get ticks-map."
  ([ticks-map] (unparse-ticks ticks-map 0))
  ([ticks-map hr-precision]
   (let [d ticks-map, wk (:wk d), da (:da d)]
     (str (if (unparse? wk) (str "W" wk)) (if (unparse? da) (str "D" da))
          (unparse-full-time d hr-precision)))))

(defn unparse-date-map
  "Unparses the date-map as string.  
Use 'read-date' to get date-map, which applies formatting.
If 'hr' precision is non-zero, format is finished in hours with 'hr-precision' 
   significant digits (default 0)"
  ([date-map] (unparse-date-map date-map 0))
  ([date-map ^long hr-precision]
   (let [d date-map, tz (:tz d), f #(format "%02d" %)]
     (str (f (:yr d)) "-" (f (:mo d)) "-" (f (:da d))
          (unparse-full-time d hr-precision) "Z" (if-not (zero? tz) tz)))))

(defn unparse-time
  "Unparses ticks as string.
  'format' options
:basic-t-time                           T210644.474Z
:basic-t-time-no-ms                     T210644Z
:basic-time                             210644.474Z
:basic-time-no-ms                       210644Z
:hour                                   21
:hour-minute                            21:06
:hour-minute-second                     21:06:44
:hour-minute-second-fraction            21:06:44.474
:hour-minute-second-ms                  21:06:44.474
:t-time                                 T21:06:44.474Z
:t-time-no-ms                           T21:06:44Z
:time                                   21:06:44.474Z
:time-no-ms                             21:06:44Z"
  [^long ticks format]
  (tf/unparse (format tf/formatters) (joda (+ ticks date-1970) 0)))

(defn unparse-date
  "Unparses date as string.
   'format' options (default is :date-full):
:basic-date                             20140511
:basic-date-time                        20140511T210644.474Z
:basic-date-time-no-ms                  20140511T210644Z
:basic-ordinal-date                     2014131
:basic-ordinal-date-time                2014131T210644.474Z
:basic-ordinal-date-time-no-ms          2014131T210644Z
:basic-week-date                        2014W197
:basic-week-date-time                   2014W197T210644.474Z
:basic-week-date-time-no-ms             2014W197T210644Z
:date                                   2014-05-11
:date-hour                              2014-05-11T21
:date-hour-minute                       2014-05-11T21:06
:date-hour-minute-second                2014-05-11T21:06:44
:date-hour-minute-second-fraction       2014-05-11T21:06:44.474
:date-hour-minute-second-ms             2014-05-11T21:06:44.474
:date-full                              2014-05-11T0:4:51.375.291:429Z-8 
                                           or 2014-05-11H0.081Z
:date-time                              2014-05-11T21:06:44.474Z
:date-time-no-ms                        2014-05-11T21:06:44Z
:mysql                                  2014-05-11 21:06:44
:ordinal-date                           2014-131
:ordinal-date-time                      2014-131T21:06:44.474Z
:ordinal-date-time-no-ms                2014-131T21:06:44Z
:rfc822                                 Sun, 11 May 2014 21:06:44 +0000
:week-date                              2014-W19-7
:week-date-time                         2014-W19-7T21:06:44.474Z
:week-date-time-no-ms                   2014-W19-7T21:06:44Z
:weekyear                               2014
:weekyear-week                          2014-W19
:weekyear-week-day                      2014-W19-7
:year                                   2014
:year-month                             2014-05
:year-month-day                         2014-05-11"
  ([^long date] (unparse-date date :date-full))
  ([^long date format] (unparse-date date format 0))
  ([^long date format ^long hr-precision]
   (if (or (nil? format) (= format :date-full))
     (unparse-date-map (read-date date) hr-precision)
     (tf/unparse (format tf/formatters) (joda date 0)))))

(defn parse-day-of-week
  "Returns day-of-week key from string"
  [day-of-week-string]
  (let [d (first (filter #(or (= (second %) day-of-week-string)
                              (= (nth % 2) day-of-week-string))
                         day-of-week-set))]
    (first d)))

(defn parse-month
  "Returns month number from string"
  [month-string]
  (let [i (ffirst (filter (fn [[i m]] (= m month-string)) (map vector (range) months-long)))]
    (if i (inc i) (let [i (ffirst (filter (fn [[i m]] (= m month-string)) (map vector (range) months-short)))]
                    (if i (inc i) nil)))))

(defn- parse-full-time
  [time-string]
  (if (fo/substring? ":" time-string)
    (let [s time-string, s (map read-string (fo/split s #":|\."))]
      (as-ticks 0 0 (nth s 5) :hr (first s), :mi (second s), :se (nth s 2),
                :ms (nth s 3), :us (nth s 4)))
    (m/round (* (read-string time-string) hour))))

(defn parse-duration
  "Creates duration from string."
  [duration-string]
  (let [s duration-string, y? (fo/substring? "Y" s), w? (fo/substring? "W" s),
        d? (fo/substring? "D" s), s (fo/split s #"Y|M|W|D|T|H"),
        s (if y? (rest s) s), yr (if y? (read-string (first s)) 0),
        mo (read-string (second s)), wk (if-not w? 0 (read-string (nth s 2))),
        da (if-not d? 0 (read-string (nth s (- (count s) 2)))),
        ti (parse-full-time (last s))]
    (duration yr mo da ti :wk wk)))

(defn parse-ticks
  "Creates ticks from string."
  ^long [ticks-string]
  (let [s ticks-string, w? (fo/substring? "W" s), d? (fo/substring? "D" s),
        s (fo/split s #"W|D|T|H"), s (if w? (rest s) s),
        wk (if w? (read-string (first s)) 0),
        da (if d? (read-string (second s)) 0), ti (parse-full-time (last s))]
    (as-ticks wk da ti)))

(defn parse-time
  "Returns ticks from time string.
'format' options
:basic-t-time                           T210644.474Z
:basic-t-time-no-ms                     T210644Z
:basic-time                             210644.474Z
:basic-time-no-ms                       210644Z
:hour                                   21
:hour-minute                            21:06
:hour-minute-second                     21:06:44
:hour-minute-second-fraction            21:06:44.474
:hour-minute-second-ms                  21:06:44.474
:t-time                                 T21:06:44.474Z
:t-time-no-ms                           T21:06:44Z
:time                                   21:06:44.474Z
:time-no-ms                             21:06:44Z"
  ^long [time-string format]
  (subtract-joda->ticks (tf/parse (format tf/formatters) time-string)
                        (ti/epoch)))

(defn parse-date
  "Creates date with time zone tuple from string.
'format' options (default is :date-full):
:basic-date                             20140511
:basic-date-time                        20140511T210644.474Z
:basic-date-time-no-ms                  20140511T210644Z
:basic-ordinal-date                     2014131
:basic-ordinal-date-time                2014131T210644.474Z
:basic-ordinal-date-time-no-ms          2014131T210644Z
:basic-week-date                        2014W197
:basic-week-date-time                   2014W197T210644.474Z
:basic-week-date-time-no-ms             2014W197T210644Z
:date                                   2014-05-11
:date-hour                              2014-05-11T21
:date-hour-minute                       2014-05-11T21:06
:date-hour-minute-second                2014-05-11T21:06:44
:date-hour-minute-second-fraction       2014-05-11T21:06:44.474
:date-hour-minute-second-ms             2014-05-11T21:06:44.474
:date-full                              2014-05-11T0:4:51.375.291:429Z-8 
                                           or 2014-05-11H0.081Z
:date-time                              2014-05-11T21:06:44.474Z
:date-time-no-ms                        2014-05-11T21:06:44Z
:mysql                                  2014-05-11 21:06:44
:ordinal-date                           2014-131
:ordinal-date-time                      2014-131T21:06:44.474Z
:ordinal-date-time-no-ms                2014-131T21:06:44Z
:rfc822                                 Sun, 11 May 2014 21:06:44 +0000
:week-date                              2014-W19-7
:week-date-time                         2014-W19-7T21:06:44.474Z
:week-date-time-no-ms                   2014-W19-7T21:06:44Z
:weekyear                               2014
:weekyear-week                          2014-W19
:weekyear-week-day                      2014-W19-7
:year                                   2014
:year-month                             2014-05
:year-month-day                         2014-05-11"
  ([date-string] (parse-date date-string :date-full))
  ([date-string format]
   (if (or (nil? format) (= format :date-full))
     (let [s date-string, s (fo/split s #"T|H|Z|-" 5),
           tz (if (= (count s) 5) (last s) 0),
           tz (if (= "" tz) 0 (read-string tz)),
           ti (parse-full-time (nth s 3)),
           d (map read-string (subvec s 0 3))]
       [(date (first d) (second d) (nth d 2) ti) tz])
     (let [jod (try (tf/parse (format tf/formatters) date-string)
                    (catch Exception e nil))]
       (when jod (joda->date-with-time-zone jod))))))

;;;PREDICATES
(defn weekend?
  "Returns whether a supplied date occurs on a Saturday or Sunday"
  [d] (let [dow (day-of-week (long d))] (or (= dow :sa) (= dow :su))))

(defn weekday?
  "Returns whether a supplied date occurs on Monday through Friday"
  [d] (not (weekend? (long d))))

(defn first-day-of-month?
  "Returns whether a supplied date occurs on the first day of a month"
  [d] (m/one? (:da (read-date (long d)))))

(defn last-day-of-month?
  "Returns whether a supplied date occurs on the last day of a month"
  [d] (first-day-of-month? (+ (long d) day)))

(defn interval?
  "Returns true if x is an interval"
  [x]
  (and (sequential? x) (= 2 (count x)) (m/long-able? (first x))
       (m/long-able? (second x)) (<= (first x) (second x))))

(defn interval+?
  "Returns true if x is a positive interval"
  [x] (and (interval? x) (< (first x) (second x))))

;;;INTEGRATION
(defn integrate-interval
  "Returns the integral of a function 'f' over a supplied interval"
  [f interval]
  (ca/integrate (fn [d] (mx/emap period (f d))) interval))