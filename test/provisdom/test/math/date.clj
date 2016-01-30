(ns provisdom.test.math.date
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.date :refer :all]
            [provisdom.math.core :as m]))

(facts "leap year adjustments"
       (fact "leap year?"
             (leap-year? 2000) => true
             (leap-year? 2001) => false
             (leap-year? 2004) => true)
       (fact "days per month"
             (days-per-month 2014 3) => 31
             (days-per-month 2014 2) => 28
             (days-per-month 2014 1) => 31
             (days-per-month 2004 2) => 29)
       (fact "days until month"
             (days-until-month 2014 3) => 59
             (days-until-month 2014 2) => 31
             (days-until-month 2014 1) => 0
             (days-until-month 2004 2) => 31)
       (fact "passed leap days"
             epoch => 2070
             ;;passed leap days since 'epoch' by default
             (passed-leap-days 2016 3) => -13 
             (passed-leap-days 2014 2) => -14
             (passed-leap-days 2014 2 2020 9) => 2
             (passed-leap-days 2014 2 2014 5) => 0
             (passed-leap-days 2000 1 2000 3) => 1
             (passed-leap-days 2000 1 2004 1) => 1
             (passed-leap-days 2000 1 2004 3) => 2
             (passed-leap-days 2000 1 2005 1) => 2
             (passed-leap-days 2000 1 2100 1) => 25
             (passed-leap-days 2000 1 2100 3) => 25
             (passed-leap-days 2000 1 2400 1) => 97
             (passed-leap-days 2000 1 2400 3) => 98
             (passed-leap-days 2000 1 1996 3) => 0
             (passed-leap-days 2000 1 1996 1) => -1
             (passed-leap-days 2000 1 1900 3) => -24
             (passed-leap-days 2000 1 1900 1) => -24
             (passed-leap-days 2000 1 1600 3) => -96
             (passed-leap-days 2000 1 1600 1) => -97)) 

(facts "read" 
       (fact "read ticks"
             (read-ticks 1348333636369480) =>  
             {:da 6, :hr 15, :mi 23, :ms 318, :se 33, :ti 904, :us 504, :wk 1}
             (read-ticks 1348333636369480 [:da :us]) => 
             {:da 13, :ti 904, :us 55413318504}) 
       (fact "read duration"
             (read-duration [26 197683212098329]) => 
             {:da 2, :hr 0, :mi 0, :mo 2, :ms 10, :se 0, :ti 529, :us 575, 
              :wk 0, :yr 2}
             (read-duration [26 197683212098329] [:hr :mi]) => 
             {:hr 48, :mi 0, :mo 26, :ti 12098329})
       (fact "read date"
             (read-date -9223305062400000000) => 
             {:da 9, :hr 0, :mi 0, :mo 7, :ms 0, :se 0, :ti 0, :tz 0, :us 0,
              :yr 1814}
             (read-date 9223305062400000000 ) => 
             {:da 28, :hr 0, :mi 0, :mo 6, :ms 0, :se 0, :ti 0, :tz 0, :us 0, 
              :yr 2325}
             (read-date -2015676748786905120) => 
             {:da 3, :hr 0, :mi 0, :mo 3, :ms 11, :se 0, :ti 656, :tz 0, 
              :us 446, :yr 2014}
             (read-date -2015676748786905120 [:se :ms]) =>
             {:da 3, :mo 3, :ms 11, :se 0, :ti 510880, :tz 0, :yr 2014} 
             (read-date -2015676748786905120 [:se :ms] 3) => 
             {:da 3, :mo 3, :ms 11, :se 0, :ti 510880, :tz 3, :yr 2014}
             (read-date -1044162662400000000) => 
             {:da 29, :hr 0, :mi 0, :mo 1, :ms 0, :se 0, :ti 0, :tz 0, :us 0, 
              :yr 2041} 
             (read-date -1036932733410500558) => 
             {:da 12, :hr 3, :mi 31, :mo 4, :ms 997, :se 7, :ti 226, :tz 0, 
              :us 814, :yr 2041}
             (read-date 2015 13 53 0) =>  
             {:da 22, :hr 0, :mi 0, :mo 2, :ms 0, :se 0, :ti 0, :tz 0, :us 0, 
              :yr 2016}
             (read-date 2015 13 53 0 [:se :ms]) => 
             {:da 22, :mo 2, :ms 0, :se 0, :ti 0, :tz 0, :yr 2016}
             (read-date 2015 13 53 0 [:se :ms] -8) => 
             {:da 22, :mo 2, :ms 0, :se 0, :ti 0, :tz -8, :yr 2016}
             (read-date (date 2015)) => 
             {:da 1, :hr 0, :mi 0, :mo 1, :ms 0, :se 0, :ti 0, :tz 0, :us 0, 
              :yr 2015}
             ;;useful when adding a time zone
             full-date-map => [:hr :mi :se :ms :us] 
             (read-date 2014 3 3 22364893338294 full-date-map -8) => 
             {:da 3, :hr 5, :mi 25, :mo 3, :ms 731, :se 49, :ti 78, :tz -8, 
              :us 939, :yr 2014})
       (fact "day of week"
             (day-of-week (date 2014 1 1 0)) => :we
             (day-of-week (date 2015 1 1 0)) => :th))

(facts "converters"
       (fact "instant"
             ;(instant$) => #inst "2014-12-31T23:26:15.913-00:00"
             (instant (date 2014)) => #inst "2014-01-01T00:00:00.000-00:00"
             (instant->date (instant (date 2013 3 3))) => -2051753932800000000)
       (fact "time-millis"
             ;(time-millis$) => 1420068455280
             (time-millis (date 2014)) => 1388534400000
             (time-millis->date 1388534400000) => -2021706086400000000)
       (fact "create date with time zone"
             (joda->date-with-time-zone (joda (date 2014 1 1 0) -8)) => 
             [-2021706086400000000 -8]))

(facts "factories"
       (fact "period"
             ;;a period is a double representing 'average years'
             (period (as-ticks 3)) => 0.05749604714675866 
             ;;a period is a double representing 'average years'
             (period [(date 2014) (date 2015)]) => 0.9993360575508053) 
       (fact "as ticks"
             (as-ticks 3) => 2075673600000000
             (as-ticks 3 3) => 2372198400000000
             (as-ticks 3 3 3) => 2372198400000003
             (as-ticks 3 3 3 :se 23 :ms 24) => 2372224739456003)
       (fact "maybe duration as ticks"
             (maybe-duration-as-ticks [0 2347]) => 2347
             (maybe-duration-as-ticks [1 2346]) => nil)
       (fact "duration"
             (duration 2) => [24 0]
             (duration 2 2) => [26 0]
             (duration 2 2 2) => [26 197683200000000]
             (duration 2 2 2 12098329) => [26 197683212098329]
             (duration 1 6 0 63392836369480 :hr 39 :us 23) => 
             [18 224010436395792])
       (fact "date"
             ; ;returns 'now' to nearest millisecond
             ;(date$) => -1985631107552664000
             (date 1814 7 9 0) => -9223305062400000000 
             (date 1814 7 8 0) => (throws) ;must be later than 7/8/1814
             (date 2325 6 28 0) => 9223305062400000000 
             (date 2325 6 29 0) => (throws) ;must be earlier than 6/29/2325
             (date 2014) => -2021706086400000000
             (date 2014 3) => -2015874432000000000
             (date 2014 3 3) => -2015676748800000000
             (date 2014 3 3 13094880) => -2015676748786905120
             (date 2014 3 3 13094880 :mi 93) => -2015670365266905120
             (date 2045 1 1 0) => -902522649600000000
             (date 2015 1 1 0) => -1985628902400000000
             (date 2041 2 1 0) => -1043866137600000000
             (date 2041 1 29 0) => -1044162662400000000
             (date 2041 4 13 0) => -1036848384000000000
             ;;ok to put months and days out of range
             (read-date (date 2041 13 13 0) []) => 
             {:da 13, :mo 1, :ti 0, :tz 0, :yr 2042} 
             (read-date (date 2041 13 32 0) []) => 
             {:da 1, :mo 2, :ti 0, :tz 0, :yr 2042}
             (read-date (date 2041 -1 -1 0) []) => 
             {:da 31, :mo 10, :ti 0, :tz 0, :yr 2040})
       (fact "year"
             year => [12 0])
       (fact "month"
             month => [1 0]))

(facts "date manipulation"
       (fact "adding -- use 'add-dates' for adding with dates and durations.  
              'add-ticks' for ticks with durations.  
              '+' for ticks and dates."
             (+ (as-ticks 13) 12321993) => 8994585612321993 ;add ticks
             ;;alternative
             (add-ticks (as-ticks 13) 12321993) => 8994585612321993 
             (add-ticks (as-ticks 13) (duration 1 1 1)) => 
             [13 9093427200000000] ;add ticks and duration 
             (add-ticks (duration 1 1 1) (as-ticks 13)) => 
             [13 9093427200000000] ;add duration and ticks 
             (read-date (+ (date 2014) (as-ticks 13)) []) => 
             {:da 2, :mo 4, :ti 0, :tz 0, :yr 2014} ;add ticks and date
             (read-date (+ (as-ticks 13) (date 2014)) []) => 
             {:da 2, :mo 4, :ti 0, :tz 0, :yr 2014} ;add date and ticks
             (read-date (add-duration (date 2014) [3 0]) []) => 
             {:da 1, :mo 4, :ti 0, :tz 0, :yr 2014} ;add date and duration
             (read-date (add-duration [3 0] (date 2014)) []) => 
             {:da 1, :mo 4, :ti 0, :tz 0, :yr 2014} ;add duration and date
             (add-duration [3 0] [3 1]) => [6 1] ;add durations
             (add-ticks [3 0] [3 1]) => [6 1] ;alternative
             ;;can't add dates
             (add-duration (date 2014) (date 2015)) => (throws) 
             ;;often can't add dates like this due to overflow
             (+ (date 2014) (date 2015)) => -4007334988800000000 
             (read-date (add-duration (date 2014) [3 (as-ticks 2)]) []) => 
             {:da 15, :mo 4, :ti 0, :tz 0, :yr 2014}
             (read-date (add-duration (date 2014) (duration -1 2 3) 
                                      (duration 1 1 1)) []) => 
             {:da 5, :mo 4, :ti 0, :tz 0, :yr 2014})
       (fact "subtracting -- use 'sub-dates' for subtracting with dates and 
                 durations.  
              'sub-ticks' for ticks with durations.  
              '-' for ticks and dates."
             (- (as-ticks 13) 12321993) => 8994585587678007 ;subtract ticks
             ;;alternative
             (sub-ticks (as-ticks 13) 12321993) => 8994585587678007 
             (sub-ticks (duration 1 1 1) (as-ticks 13)) => 
             [13 -8895744000000000] ;subtract ticks from duration
             (sub-ticks (as-ticks 13) (duration 1 1 1)) => 
             [-13 8895744000000000] ;subtract duration from ticks
             (read-date (- (date 2014) (as-ticks 13)) []) => 
             {:da 2, :mo 10, :ti 0, :tz 0, :yr 2013} ;subtract ticks from date
             (read-date (- (as-ticks 13) (date 2014)) []) => 
             ;;don't do this!  subtract date from ticks
             {:da 3, :mo 4, :ti 0, :tz 0, :yr 2126} 
             ;;subtract duration from date
             (read-date (sub-dates (date 2014) [3 0]) []) => 
             {:da 1, :mo 10, :ti 0, :tz 0, :yr 2013} 
             (read-date (sub-dates [3 0] (date 2014)) []) => 
             (throws) ;can't subtract date from duration
             ;;subtract dates into a duration
             (sub-dates (date 2015) (date 2014 3 3 3)) => [10 -197683200000003] 
             ;;subtract dates into ticks
             (- (date 2015) (date 2014 3 3 3)) => 30047846399999997 
             ;;alternative
             (sub-ticks (date 2015) (date 2014 3 3 3)) => 30047846399999997 
             (sub-dates (date 2015) (date 2014)) => [12 0] ;12 months
             ;;52 weeks and 1 day
             (read-ticks (- (date 2015) (date 2014))) => 
             {:da 1, :hr 0, :mi 0, :ms 0, :se 0, :ti 0, :us 0, :wk 52}) 
       (fact "multiply a duration by a long"
             (mul (duration 1 1 1) 3) => [39 296524800000000]
             (mul 3 (duration 1 1 1)) => [39 296524800000000]
             (mul 3 3) => 9
             ;;can't multiply durations
             (mul (duration 1 1 1) (duration 1 1 1)) => (throws) 
             ;;3.0 is long-able
             (mul 3.0 (duration 1 1 1)) => [39 296524800000000] 
             ;;can't multiply by a number that isn't long-able
             (mul 3.3 (duration 1 1 1)) => (throws)) 
       (fact "convert duration to ticks"
             (to-ticks (duration 1 1 1) (date 2014)) => 39240115200000000
             (to-ticks (duration 1 1 1) (date 2014) true) => 39141273600000000)
       (fact "convert ticks to duration"
             (read-duration (to-duration (as-ticks 13) (date 2014)) [:da]) => 
             {:da 1, :mo 3, :ti 0}
             (read-duration (to-duration (as-ticks 13) (date 2014) true) 
                            [:da]) => {:da -1, :mo 3, :ti 0}))

(facts "time zones"
       (fact "time zone for current environment" 
             ;(environment-time-zone) => -8) ;-8 in PST
       ))

(facts "periodic"
       (fact "lazy period"
             (map #(read-date % []) (take 3 (lazy-period (date 2014 3 3) 
                                                         [3 0]))) => 
             [{:da 3, :mo 3, :ti 0, :tz 0, :yr 2014} 
              {:da 3, :mo 6, :ti 0, :tz 0, :yr 2014} 
              {:da 3, :mo 9, :ti 0, :tz 0, :yr 2014}]
             (map #(read-date % []) 
                  (take 3 (lazy-period (date 2014 3 3) [0 (as-ticks 4 3)]))) =>
             [{:da 3, :mo 3, :ti 0, :tz 0, :yr 2014} 
              {:da 3, :mo 4, :ti 0, :tz 0, :yr 2014} 
              {:da 4, :mo 5, :ti 0, :tz 0, :yr 2014}])
       (fact "intervals"
             (intervals m/sq (date 2014) 3 average-month) => 
             [[-2021706086400000000 -2018697656976000000] 
              [-2018697656976000000 -2009672368704000000] 
              [-2009672368704000000 -1994630221584000000]]
             (intervals m/sq (date 2014) 3 month) => 
             [[-2021706086400000000 -2018641996800000000] 
              [-2018641996800000000 -2009845094400000000] 
              [-2009845094400000000 -1994722329600000000]]
             (intervals (date 2014) 3 average-month) => 
             [[-2021706086400000000 -2018697656976000000] 
              [-2018697656976000000 -2015689227552000000] 
              [-2015689227552000000 -2012680798128000000]]
             (intervals (date 2014) 3 month) =>
             [[-2021706086400000000 -2018641996800000000] 
              [-2018641996800000000 -2015874432000000000] 
              [-2015874432000000000 -2012810342400000000]])
       (fact "dates"
             (dates (intervals (date 2014) 3 month)) => 
             [-2021706086400000000 -2018641996800000000 -2015874432000000000 
              -2012810342400000000]))

(facts "formatting"
       (fact "unparse day of week"
             (unparse-day-of-week :we) => "Wednesday"
             (unparse-day-of-week :we true) => "Wed"
             (unparse-day-of-week :w) => nil)
       (fact "unparse month"
             (unparse-month 8) => "August"
             (unparse-month 8 true) => "Aug"
             (unparse-month 0) => nil)
       (fact "unparse duration"
             ;;duration only uses days and years when it needs them
             (unparse-duration (read-duration [0 3333333333333])) => 
             "M0T0:48:33.752.913:861" 
             (unparse-duration (read-duration [3 333333333333333])) => 
             "M3D3T8:56:15.291.375:333"
             (unparse-duration (read-duration [35 333333333333333])) => 
             "Y2M11D3T8:56:15.291.375:333" 
             ;;shape of map determines string -- just ticks here
             (unparse-duration (read-duration [35 333333333333333] [])) => 
             "M35T0:0:0.0.0:333333333333333" 
             (unparse-duration (read-duration [35 333333333333333]) 3) => 
             "Y2M11D3H8.938") ;can unparse hrs with loss of info
       (fact "unparse ticks"
             ;;ticks only uses days and weeks when it needs them
             (unparse-ticks (read-ticks 3333)) => 
             "T0:0:0.0.2:1045" 
             (unparse-ticks (read-ticks 3333333333333)) => 
             "T0:48:33.752.913:861" 
             (unparse-ticks (read-ticks 333333333333333333)) => 
             "W481D5T9:34:51.375.291:429"
             ;;shape of map determines string -- just ticks here
             (unparse-ticks (read-ticks 333333333333333 [])) => 
             "T0:0:0.0.0:333333333333333" 
              ;;can unparse hrs with no or little loss of info 
              ;;if precision is high enough
             (unparse-ticks (read-ticks 333333333333333) 15) => 
             "D3H8.937580937580856"
             ;;can unparse hrs with loss of info
             (unparse-ticks (read-ticks 3333333333) 3) => "H0.001") 
       (fact "unparse date-map"
             ;;time is hrs, minutes, secs, millis, micros, 
             ;;then ticks and time zone
             (unparse-date-map (read-date 2014 3 3 333333333333)) => 
             "2014-03-03T0:4:51.375.291:429Z" 
             (unparse-date-map 
               (read-date 2014 3 3 333333333333 full-date-map -8)) => 
             "2014-03-03T0:4:51.375.291:429Z-8" ;full map gives full breakdown
             ;;shape of map determines string -- just ticks here
             (unparse-date-map (read-date 2014 3 3 333333333333 [] -8)) => 
             "2014-03-03T0:0:0.0.0:333333333333Z-8" 
             ;;can unparse hrs to supplied precision instead of time
             (unparse-date-map (read-date 2014 3 3 333333333333) 3) => 
             "2014-03-03H0.081Z") 
       (fact "unparse time"
             (unparse-time 14547533489534 :hour-minute) => "03:31")
       (fact "unparse date"
             (unparse-date -23094783294823) => 
             "2069-12-31T18:23:32.252.364:761Z"
             (unparse-date -23094783294823 :basic-date) => "20691231"
             (unparse-date -23094783294823 nil 4) => "2069-12-31H18.3923Z")
       (fact "parse day of week"
             (parse-day-of-week "Wednesday") => :we
             (parse-day-of-week "Wed") => :we
             (parse-day-of-week "W") => nil)
       (fact "parse month"
             (parse-month "August") => 8
             (parse-month "Aug") => 8
             (parse-month "A") => nil)
       (fact "parse duration"
             (read-duration (parse-duration "M0T0:48:33.752.913:861") []) => 
             {:mo 0, :ti 3333333333333}
             (parse-duration "M3D3T8:56:15.291.375:333") => [3 333333333333333]
             (parse-duration "Y2M11D3T8:56:15.291.375:333") => 
             [35 333333333333333]
             (parse-duration "M35T0:0:0.0.0:333333333333333") => 
             [35 333333333333333]
             ;;lost some precision
             (parse-duration "Y2M11D3H8.938") => [35 333335059200000]) 
       (fact "parse-ticks"
             (parse-ticks "T0:0:0.0.2:1045") => 3333
             (parse-ticks "T0:48:33.752.913:861") => 3333333333333
             (parse-ticks "W481D5T9:34:51.375.291:429") => 333333333333333333
             (parse-ticks "T0:0:0.0.0:333333333333333") => 333333333333333
             ;;didn't lose precision
             (parse-ticks "D3H8.937580937580856") =>  333333333333333 
             (parse-ticks "H0.001") => 4118400000)
       (fact "parse-time"
             (parse-time "21:06" :hour-minute) => 86898240000000)
       (fact "parse date"
             (read-date (first (parse-date "2014-03-03T0:4:51.375.291:429Z")) 
                        []) => {:da 3, :mo 3, :ti 333333333333, :tz 0, 
                                :yr 2014}
             (parse-date "20140511" :basic-date) => 
             [-2008856678400000000 0]
             (parse-date "20140231" :date) => nil
             (parse-date "2014-03-03T0:4:51.375.291:429Z-8") => 
             [-2015676415466666667 -8]
             (parse-date "2014-03-03T0:0:0.0.0:333333333333Z-8") => 
             [-2015676415466666667 -8]
             (read-date (first (parse-date "2014-03-03H0.081Z"))) =>  
             {:da 3, :hr 0, :mi 4, :mo 3, :ms 600, :se 51, :ti 0, :tz 0, 
              :us 0, :yr 2014})) ;lost some precision)
                           
(facts "predicates"
       (fact "weekend"
             (weekend? (date 2014 1 1)) => false
             (weekend? (date 2014 1 4)) => true)
       (fact "weekday"
             (weekday? (date 2014 1 1)) => true
             (weekday? (date 2014 1 4)) => false)
       (fact "first day of month"
             (first-day-of-month? (date 2014 1 1)) => true
              (first-day-of-month? (date 2014 1 2)) => false)
       (fact "last day of month"
             (last-day-of-month? (date 2014 1 1)) => false
             (last-day-of-month? (date 2013 1 31)) => true)
       (fact "interval"
             (interval? [(date 2012 1 1) (date 2012 1 2)]) => true
             (interval? [(date 2012 1 1) (date 2012 1 1)]) => true
             (interval? [[(date 2012 1 1) (date 2012 1 1)]]) => false
             (interval? [[(date 2012 1 1) (date 2012 1 1)]]) => false
             (interval? [(date 2012 1 1) (date 2012 1 1) (date 2012 1 1)]) => 
             false
             (interval? (date 2012 1 1)) => false
             (interval? [(date 2012 1 1) (date 2011 1 2)]) => false)
       (fact "positive interval"
             (interval+? [(date 2012 1 1) (date 2012 1 2)]) => true
             (interval+? [(date 2012 1 1) (date 2012 1 1)]) => false
             (interval+? [(date 2012 1 1) (date 2011 1 2)]) => false))

(facts "integrate"
       (fact "integrate interval"
             (integrate-interval (fn [d] 1.0) [(date 2014) (date 2015)]) => 
             0.9993360575508053))
       
