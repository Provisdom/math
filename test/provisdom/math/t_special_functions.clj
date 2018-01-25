(ns provisdom.math.t-special-functions
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.special-functions :as mf]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(def done-wods-8-17+
  [#{(str "Work on variations of Kalsu, like EMOM20 of 5 burpees + 5 thrusters @75, "
          "behind neck thrusters, just front squats or push press, "
          "less or more burpees or an alternative exercise, or with dumbbells")} ;8/17
   #{}
   #{"21 unbroken kipping C2B"}                             ;8/19
   #{}
   #{"185-lb 20-rep Grace in 10-min; did it!"}              ;8/21
   #{"E2MOM4 Handstand holds for 45->60-sec"
     "E3MOM5 Handstand holds for 45->60-sec"
     "Work on chin-ups; did it!"}                           ;8/22
   #{"10-min EMOM odd: 3->5 bar muscle-up, even: rest"
     "E4MOM5 thruster@85->95x21+run200M"}                   ;8/23
   #{"7x15 cal Assault bike in 60 sec with 90-sec rest (did non-Assault); did it!"
     "Work on thrusters at 2x21+1, then Front squats and press at 15, then down."} ;8/24
   #{"Work on butterfly c2b"}                               ;8/25
   #{"3x30-sec 1->2-leg L-sits"
     "Sitting-up straddle leg-lift holds (for pistols, L-sits, etc.)"
     "2 rounds of 15-sec 1-arm bar hangs; did it!"}         ;8/26
   #{}                                                      ;8/27
   #{"5x15 cal Assault bike in 50 sec with 70-sec rest; did it!"
     (str "5 rds of 10 hollow rocks, 10 V-ups, 10 tuck-ups, 10-sec hollow hold, "
          "rest 1 minute (unbroken 3->5 sets or 10:58->9:00)")} ;8/28
   #{}                                                      ;8/29
   #{}                                                      ;8/30
   #{}                                                      ;8/31
   #{}                                                      ;9/1
   #{}                                                      ;9/2
   #{}                                                      ;9/3
   #{}                                                      ;9/4
   #{}                                                      ;9/5
   #{}                                                      ;9/6
   #{}                                                      ;9/7
   #{}                                                      ;9/8
   #{}                                                      ;9/9
   #{}                                                      ;9/10
   #{}                                                      ;9/11
   #{}                                                      ;9/12
   #{"E3MOM5 Handstand holds for 45->60-sec"
     "Fran+1-mile run 14:47->14 minutes"
     "Work on bottom of muscle-up; did it!"}                ;9/13
   #{"Side planks on elbow :45 on a side, :75 off; 5 rds alt each side; did it!"
     "Back Squat 265->275x10 to 1 in 30 minutes"}           ;9/14
   #{"E5MOM3 55->60 second 135# OH hold"
     "Handstand walking 7x50' in 60-sec with 2-min rest"}   ;9/15
   #{}                                                      ;9/16
   #{"30 seconds of handstand shoulder taps; did it!"
     "Build up lateral shoulder raises; did it!"
     "3x30-sec 1->2-leg L-sits"}                            ;9/17
   #{}                                                      ;9/18
   #{}                                                      ;9/19
   #{"E2MOM5 of 5 strict HSPUs"}                            ;9/20
   #{}                                                      ;9/21
   #{}                                                      ;9/22
   #{}                                                      ;9/23
   #{}                                                      ;9/24
   #{"3 rounds of 20 pistols w/ 90-sec rest between in 10 minutes"} ;9/25
   #{}                                                      ;9/26
   #{}                                                      ;9/27
   #{}                                                      ;9/28
   #{}                                                      ;9/29
   #{}                                                      ;9/30
   #{}                                                      ;10/1
   #{}                                                      ;10/2
   #{}                                                      ;10/3
   #{}                                                      ;10/4
   #{"Squat Grace at 155 in 10 minutes; did it!"
     "EMOM4 6x c2b + EMOM8 2x bar muscle-up; did it!"}      ;10/5
   #{(str "Work on pistols either on box (build up to 20 in a row per leg then lower box), "
          "with elevated heels, or with a counterbalance weight held out in front")} ;10/6
   #{}                                                      ;10/7
   #{}                                                      ;10/8
   #{"Eva in 45 minutes (3->5 rds of 800M run, 30x 70# KBS, 30x pull-ups)"} ;10/9                                                      ;10/9
   #{}                                                      ;10/10
   #{}                                                      ;10/11
   #{}                                                      ;10/12
   #{"16 rounds of E2MOM fast-shuffling 100M farmer’s carry with 45#->53#"} ;10/13
   #{}                                                      ;10/14
   #{"50 cal Assault bike in 2.5 min (~70 RPM)"
     "7 rounds of 35->45-sec hollow rock + 75-sec rest"
     "Tabata (15->20sec) 1->2-leg L-sits"}                  ;10/15
   #{"7x10->20-sec straight-arm hanging 1->2-leg L-sits"
     "30x 95# S2OH, 9 MUs, 20x 135# S2OH, 7 MUs, 10x 185# S2OH, 5 MUs in 15:47->15 minutes"
     "Work on tying together natural-looking triple-unders"} ;10/16
   #{"Work on butterfly c2b"
     "Work on high-bar back squat with Oly Shoes; did it!"} ;10/17
   #{(str "5 rds of 5x(HPC,thruster,back thruster)+12x barbell facing burpees "
          "in 12:35->11-min @105->135")
     "15->21 unbroken Thrusters @95"}                       ;10/18
   #{"Work on ski machine; did it!"
     "12->21 kipping HSPUs in a row"
     "Work on butterfly pull-ups"
     "3->5 rds of 10 hollow rocks, 10 V-ups, 10 tuck-ups, 10-sec hollow hold, rest 1 minute"} ;10/19
   #{}                                                      ;10/20
   #{"20->30-sec each side of side plank with top leg up"
     "3x25->30-sec 1->2-leg L-sits"
     "E2MOM4 Handstand holds for 48->60-sec"}               ;10/21
   #{"EMOM15 of PCx3, FSx3, PJx3 @135->185 (watch low back)"
     "14->21 unbroken kipping C2B"}                         ;10/22
   #{}                                                      ;10/23
   #{"EMOM20 of odd: bike 8-cal, even: FSx7@75# + burpee over barbell x5"} ;10/24
   #{"5 Rounds of 200M run + 70# KBS x12 in 12 minutes"}    ;10/25
   #{}                                                      ;10/26
   #{}                                                      ;10/27
   #{}                                                      ;10/28
   #{}                                                      ;10/29
   #{"5 Rounds of 10-cal bike, 155# S2OH x10, double-unders x35 in 15 minutes"} ;10/30
   #{}                                                      ;10/31
   #{}                                                      ;11/1
   #{}                                                      ;11/2
   #{}                                                      ;11/3
   #{}                                                      ;11/4
   #{"Handstand walking 7x50' in 60-sec with 2-min rest"
     "Couch Stretch -> Bulgarian split squats; did it!"
     "3x20->30-sec straight-arm hanging 1->2-leg L-sits"}   ;11/5
   #{"E5MOM3 55->60 second 135# OH hold"
     "Tabata (14->20sec) 1->2-leg L-sits"}                  ;11/6
   #{}                                                      ;11/7
   #{}                                                      ;11/8
   #{"12-min EMOM odd: 18->20-cal row, even: 13->15x 24-inch box jump overs"} ;11/9
   #{"10-min EMOM odd: 4->5 bar muscle-up, even: rest"
     "10->21 continuous-kipping TTB"}                       ;11/10
   #{"5 rds of 9->10 kipping HSPU in 1-min, rest 2-min"
     (str "2-min of sit-ups, 2-min of planks continuously cycling from elbows "
          "to hands, 2-min of hollow-ups; did it!")}        ;11/11
   #{"E3MOM5 Handstand holds for 45->60-sec"
     (str "Work on pistols either on box (build up to 20 in a row per leg then lower box), "
          "with elevated heels, or with a counterbalance weight held out in front")
     "Sitting-up straddle leg-lift holds (for pistols, L-sits, etc.)"} ;11/12
   #{"9->10 to 1 of 165# Power Cleans and Front Squats in 15 minutes"} ;11/13
   #{}                                                      ;11/14
   #{}                                                      ;11/15
   #{}                                                      ;11/16
   #{}                                                      ;11/17
   #{}                                                      ;11/18
   #{}                                                      ;11/19
   #{}                                                      ;11/20
   #{}                                                      ;11/21
   #{}                                                      ;11/22
   #{}                                                      ;11/23
   #{"E2MOM7 of 35->45-sec hollow rock"
     "3 rounds of 20 pistols w/ 90-sec rest"}               ;11/24
   #{}                                                      ;11/25
   #{}                                                      ;11/26
   #{}                                                      ;11/27
   #{"Tabata (14->20sec) 1->2-leg L-sits"
     "20->30-sec each side of side plank with top leg up"
     "4->5 rds of 10 hollow rocks, 10 V-ups, 10 tuck-ups, 10-sec hollow hold, rest 1 minute"} ;11/28
   #{}                                                      ;11/29
   #{"E2MOM4 Handstand holds for 48->60-sec"}               ;11/30
   #{}                                                      ;12/1
   #{"3x25->30-sec 1->2-leg L-sits in 10 minutes"
     "16 rounds of E2MOM fast-shuffling 100M farmer’s carry with 50#->53#"} ;12/2
   #{}
   #{"E2MOM5 of 5 strict HSPUs"
     "Work on tying together natural-looking triple-unders"} ;12/4
   #{}                                                      ;12/5
   #{}                                                      ;12/6
   #{}                                                      ;12/7
   #{"Dumbbell Kalsu 35->45"}                               ;12/8
   #{}                                                      ;12/9
   #{"E2MOM7 of 35->45-sec hollow rock"
     "E2MOM7 of 10->20-sec straight-arm hanging 1->2-leg L-sits"} ;12/10
   #{(str "Fight Gone Bad: 286->300 3x(wall ball, SDHP @75, 20-inch box jumps, "
          "PP, cal row, rest) 1 minute each")}              ;12/11
   #{}                                                      ;12/12
   #{}                                                      ;12/13
   #{}                                                      ;12/14
   #{"Back Squat 265->275x10 to 1 in 30 minutes"}           ;12/15
   #{}                                                      ;12/16
   #{}                                                      ;12/17
   #{"5 rds of 10 kipping HSPU in 1-min, rest 2-min"
     "3 rounds of 20 pistols w/ 90-sec rest"
     "Sitting-up straddle leg-lift holds (for pistols, L-sits, etc.)"} ;12/18
   #{"E3MOM5 Handstand holds for 46->60-sec"
     "Tabata (14->20sec) 1->2-leg L-sits"
     "E2MOM7 of 35->45-sec hollow rock"}                    ;12/19
   #{}                                                      ;12/20
   #{"12->21 kipping HSPUs in a row"
     (str "Work on pistols either on box (build up to 20 in a row per leg then lower box), "
          "with elevated heels, or with a counterbalance weight held out in front")
     "Handstand walking 7x50' in 60-sec with 2-min rest"}   ;12/21
   #{"Work on Front squats and press at (15,15,5), then thrusters, then down."
     "10->21 continuous-kipping TTB"}                       ;12/22
   #{"E2MOM7 of 11->20-sec straight-arm hanging 1->2-leg L-sits"
     "E2MOM4 Handstand holds for 48->60-sec"
     "3x20->30-sec straight-arm hanging 1->2-leg L-sits"}   ;12/23
   #{"3x25->30-sec 1->2-leg L-sits in 10 minutes"
     "20->30-sec each side of side plank with top leg up"}  ;12/24
   #{}                                                      ;12/25
   #{"Kalsu from floor 105->115 in 30 minutes"}             ;12/26
   #{"E2MOM5 of 5 strict HSPUs"}                            ;12/27
   #{}                                                      ;12/28
   #{"Back Squat 345x1"}                                    ;12/29
   #{}                                                      ;12/30
   #{}                                                      ;12/31
   #{"4->5 rds of 10 hollow rocks, 10 V-ups, 10 tuck-ups, 10-sec hollow hold, rest 1 minute"} ;1/1
   #{"5 rds of 10 kipping HSPU in 1-min, rest 2-min"}       ;1/2
   #{}                                                      ;1/3
   #{}                                                      ;1/4
   #{"Sitting-up straddle leg-lift holds (for pistols, L-sits, etc.)"
     "E3MOM5 Handstand holds for 46->60-sec"}               ;1/5
   #{}                                                      ;1/6
   #{}                                                      ;1/7
   #{"Fran in 6:20->6 minutes"}                             ;1/8
   #{}                                                      ;1/9
   #{}                                                      ;1/10
   #{"15->21 kipping HSPUs in a row"
     "E2MOM7 of 36->45-sec hollow rock"}                    ;1/11
   #{"Work on butterfly pull-ups"
     "E2MOM5 of 11->15 each of thruster@95+TTB"}            ;1/12
   #{"3x25->30-sec 1->2-leg L-sits in 10 minutes"
     "21->30-sec each side of side plank with top leg up"}  ;1/13
   #{}                                                      ;1/14
   #{"18->21 unbroken Thrusters @95"}                       ;1/15
   #{}                                                      ;1/16
   #{}                                                      ;1/17
   #{"Eva in 45 minutes (3->5 rds of 800M run, 30x 70# KBS, 30x pull-ups)"} ;1/18
   #{}                                                      ;1/19
   #{}                                                      ;1/20
   #{}                                                      ;1/21
   #{"5 rds of 19->20 24” box jump overs + 9->10 TTB + 9->10 push-ups in 10 min"
     "5 Rounds of 200M run + 70# KBS x12 in 12 minutes; did it!"} ;1/22
   #{"41->50 cal Assault bike in 2.5 min (~63->70 RPM)"}    ;1/23
   #{}                                                      ;1/24
   ])

(comment
  #{"14->21 unbroken kipping C2B"
    "Fran+1-mile run 14:47->14 minutes"
    "15-cal bike, 155# S2OH x15, double-unders x50"
    })

(def current-wod-list
  ["5 rds of 19->20 24” box jump overs + 9->10 TTB + 9->10 push-ups in 10 min"
   "EMOM30 alternating 5 strict TTB with 1-sec pause at top, 6x PP@155, and 15-cal row"
   "5 rds of 10 burpee pull-ups + 5x 185# PC in 10 min"
   "10-min EMOM odd: 12x DB snatches #65, even: 25 Wall ball"
   "5 Rounds of 10-cal bike, 155# S2OH x10, double-unders x35 in 15 minutes"
   "12-min EMOM odd: 18->20-cal row, even: 13->15x 24-inch box jump overs"
   "Eva in 45 minutes (3->5 rds of 800M run, 30x 70# KBS, 30x pull-ups)"
   "16 rounds of E2MOM fast-shuffling 100M farmer’s carry with 50#->53#"
   "E5MOM3 55->60 second 135# OH hold"
   "30x 95# S2OH, 9 MUs, 20x 135# S2OH, 7 MUs, 10x 185# S2OH, 5 MUs in 15:47->15 minutes"
   "E3MOM5 Handstand holds for 46->60-sec"
   "E2MOM4 Handstand holds for 48->60-sec"
   "15->21 kipping HSPUs in a row"
   "5 rds of 10 kipping HSPU in 1-min, rest 2-min"
   "E2MOM5 of 5 strict HSPUs"
   "Handstand walking 7x50' in 60-sec with 2-min rest"
   "10->21 continuous-kipping TTB"
   "14->21 unbroken kipping C2B"
   "Tabata (14->20sec) 1->2-leg L-sits"
   "3x25->30-sec 1->2-leg L-sits in 10 minutes"
   "E2MOM7 of 11->20-sec straight-arm hanging 1->2-leg L-sits"
   "3x20->30-sec straight-arm hanging 1->2-leg L-sits"
   "21->30-sec each side of side plank with top leg up"
   "4->5 rds of 10 hollow rocks, 10 V-ups, 10 tuck-ups, 10-sec hollow hold, rest 1 minute"
   "Sitting-up straddle leg-lift holds (for pistols, L-sits, etc.)"
   "E2MOM7 of 36->45-sec hollow rock"
   "41->50 cal Assault bike in 2.5 min (~63->70 RPM)"
   "EMOM20 of odd: bike 8-cal, even: FSx7@75# + burpee over barbell x5"
   "Back Squat 265->275x10 to 1 in 30 minutes"
   "Back Squat 345x1"
   "Front Squats 5x9@185 start every 2 minutes from floor"
   "EMOM15 of PCx3, FSx3, PJx3 @135->185 (watch low back)"
   "9->10 to 1 of 165# Power Cleans and Front Squats in 15 minutes"
   "18->21 unbroken Thrusters @95"
   "Full Snatch E2MOM: 6x6@115->135, 5x4@135->155, 4x3@155->185 w/ 1-min extra break between groups"
   "Fran in 6:20->6 minutes"
   "Fran+1-mile run 14:47->14 minutes"
   "E2MOM5 of 11->15 each of thruster@95+TTB"
   "E4MOM5 thruster@85->95x21+run200M"
   (str "5 rds of 5x(HPC,thruster,back thruster)+12x barbell facing burpees "
        "in 12:35->11-min @105->135")
   "Work on butterfly pull-ups"
   "Work on butterfly c2b"
   "10-min EMOM odd: 4->5 bar muscle-up, even: rest"
   "8 strict muscle-ups in 20 minutes"
   (str "Work on pistols either on box (build up to 20 in a row per leg then lower box), "
        "with elevated heels, or with a counterbalance weight held out in front")
   "3 rounds of 20 pistols w/ 90-sec rest"
   "Work on tying together natural-looking triple-unders"
   "Work on Front squats and press at (15,15,5), then thrusters, then down."
   "Kalsu from floor 105->115 in 30 minutes"
   "Dumbbell Kalsu 35->45"
   "EMOM30 of 2->3 each of burpees + 135# thrusters from rack"
   "EMOM25 3->4x 135# thrusters from rack"
   (str "Fight Gone Bad: 286->300 3x(wall ball, SDHP @75, 20-inch box jumps, "
        "PP, cal row, rest) 1 minute each")])

(defn update-wods
  ""
  [wod-list done-wods start-days]
  (let [total-days (count done-wods)
        wods (mapv (fn [name]
                     {:name               name
                      :days-since-attempt (+ total-days start-days)})
                   wod-list)
        new-wods (reduce-kv (fn [outer-wods days names-set]
                              (reduce-kv (fn [inner-wods wod-index wod]
                                           (if (contains? names-set (:name wod))
                                             (assoc-in inner-wods [wod-index :days-since-attempt] (- total-days days))
                                             inner-wods))
                                         outer-wods
                                         outer-wods))
                            wods
                            done-wods)]
    new-wods))

(defn add-wod-weights-and-probs
  ""
  [wods]
  (let [adjustment (m/sqrt (dec (count wods)))
        wods (map (fn [m]
                    (assoc m :weight (max 0.0 (- (:days-since-attempt m) adjustment))))
                  wods)
        total-weight (apply + (map :weight wods))
        wods (map (fn [m]
                    (assoc m :prob (/ (:weight m) total-weight)))
                  wods)]
    wods))

(require 'provisdom.math.random)

(defn select-wod
  ""
  [wods]
  (let [rnd (provisdom.math.random/rnd!)
        selected (reduce (fn [tot m]
                           (let [tot (+ tot (:prob m))]
                             (if (> tot rnd)
                               (reduced m)
                               tot)))
                         0.0
                         wods)]
    selected))

(def start-days 50)

(defn get-today-wod
  ""
  []
  (let [a current-wod-list
        b done-wods-8-17+
        c (update-wods a b start-days)
        d (add-wod-weights-and-probs c)
        e (select-wod d)]
    e))

;;2 SECONDS

(set! *warn-on-reflection* true)

(ost/instrument)

;;;ERF FUNCTIONS
(deftest erf-test
  (is= -1.0 (mf/erf m/inf-))
  (is= 1.0 (mf/erf m/inf+))
  (is (m/nan? (mf/erf m/nan)))
  (is= 0.0 (mf/erf 0.0))
  (is= -0.997020533343667 (mf/erf -2.1))
  ;;Math 0.842700792949714869341220635082609259296066997966302908459937
  (is= 0.842700792949715 (mf/erf 1.0))
  ;;Math 0.999977909503001414558627223870417679620152292912600750342761
  (is= 0.9999779095030014 (mf/erf 3.0))
  ;;Math 0.428392355046668455103603845320172444121862928522590383495086
  (is= 0.4283923550466685 (mf/erf 0.4))
  ;;Math -0.11246291601828489220327507174396838322169629915970254753449
  (is= -0.11246291601828491 (mf/erf -0.1)))

(deftest erf-diff-test
  (is= 2.0 (mf/erf-diff m/inf- m/inf+))
  (is= 0.0 (mf/erf-diff m/inf+ m/inf+))
  (is= -1.0 (mf/erf-diff m/inf+ 0.0))
  (is= 1.839721326293382 (mf/erf-diff -2.1 1.0))
  (is= -1.8380230579686678 (mf/erf-diff 1.0 -2.0))
  (is= 0.0 (mf/erf-diff 1.0 1.0))
  (is= 0.1526214720692377 (mf/erf-diff 1.0 2.0))
  (is= 1.68540158589943 (mf/erf-diff -1.0 1.0))
  (is= 0.842700792949715 (mf/erf-diff 0.0 1.0)))

(deftest erf-derivative-test
  (is= 0.0 (mf/erf-derivative m/inf-))
  (is= 0.0 (mf/erf-derivative m/inf+))
  (is= 1.1283791670955126 (mf/erf-derivative 0.0))
  (is= 0.4151074974205947 (mf/erf-derivative 1.0))
  (is= 0.013715649999806838 (mf/erf-derivative -2.1))
  (is= 0.9615412988393078 (mf/erf-derivative 0.4)))

(deftest erfc-test
  (is= 2.0 (mf/erfc m/inf-))
  (is= 0.0 (mf/erfc m/inf+))
  (is (m/nan? (mf/erfc m/nan)))
  (is= 1.0 (mf/erfc 0.0))
  (is= 0.157299207050285 (mf/erfc 1.0))
  (is= 1.997020533343667 (mf/erfc -2.1))
  ;;Math 0.571607644953331544896396154679827555878137071477409616504913
  (is= 0.5716076449533315 (mf/erfc 0.4)))

(deftest inv-erf-test
  (is= m/inf- (mf/inv-erf -1.0))
  (is= m/inf+ (mf/inv-erf 1.0))
  (is= 0.0 (mf/inv-erf 0.0))
  (is= 1.0000000000000002 (mf/inv-erf 0.842700792949715))
  (is= -2.100000000000001 (mf/inv-erf -0.997020533343667))
  ;;Math 0.5951160814499948500193003601681082534396168862798484
  (is= 0.5951160814499948 (mf/inv-erf 0.6))
  ;;Math -1.64497635713318705017720343524951162466534303628880
  (is= -1.644976357133187 (mf/inv-erf -0.98))
  ;;Math 1.6449763571331870501772034352495116246653430362888071
  (is= 1.644976357133187 (mf/inv-erf 0.98))
  ;;Math 1.3859038243496779452779737236901775216912369580866617
  (is= 1.3859038243496777 (mf/inv-erf 0.95))
  ;;Math 1.1630871536766740867262542605629475934779325500020816
  (is= 1.1630871536766743 (mf/inv-erf 0.9))
  ;;Math 0.9061938024368232200711627030956628666508668747462206
  (is= 0.9061938024368233 (mf/inv-erf 0.8))
  ;;Math 0.3708071585935579290582494775224491386043048831629311
  (is= 0.37080715859355795 (mf/inv-erf 0.4)))

(deftest inv-erfc-test
  (is= m/inf- (mf/inv-erfc 2.0))
  (is= m/inf+ (mf/inv-erfc 0.0))
  (is= 0.37080715859355795 (mf/inv-erfc 0.6))
  (is= 0.0 (mf/inv-erfc 1.0))
  (is= 1.0000000000000002 (mf/inv-erfc 0.157299207050285))
  (is= -2.100000000000001 (mf/inv-erfc 1.997020533343667)))

(deftest inv-cdf-standard-normal-test
  (is= m/inf- (mf/inv-cdf-standard-normal 0.0))
  (is= -0.5244005127080409 (mf/inv-cdf-standard-normal 0.3))
  (is= 0.0 (mf/inv-cdf-standard-normal 0.5))
  (is= m/inf+ (mf/inv-cdf-standard-normal 1.0))
  (is= -1.0056199694085204 (mf/inv-cdf-standard-normal 0.157299207050285))
  (is= 2.750032615602772 (mf/inv-cdf-standard-normal 0.997020533343667)))

(deftest cdf-standard-normal-test
  (is= 0.0 (mf/cdf-standard-normal m/inf-))
  (is= 0.5 (mf/cdf-standard-normal 0.0))
  (is= 1.0 (mf/cdf-standard-normal m/inf+))
  (is (m/nan? (mf/cdf-standard-normal m/nan)))
  (is= 0.15729920705028516 (mf/cdf-standard-normal -1.0056199694085204))
  (is= 0.997020533343667 (mf/cdf-standard-normal 2.750032615602772)))

(defspec-test test-erf `mf/erf)
(defspec-test test-erf-diff `mf/erf-diff)
(defspec-test test-erf-derivative `mf/erf-derivative)
(defspec-test test-erfc `mf/erfc)
(defspec-test test-inv-erf `mf/inv-erf)
(defspec-test test-inv-erfc `mf/inv-erfc)
(defspec-test test-inv-cdf-standard-normal `mf/inv-cdf-standard-normal)
(defspec-test test-cdf-standard-normal `mf/cdf-standard-normal)

;;;GAMMA
(deftest gamma-test
  (is= 0.0 (mf/gamma m/inf-))
  (is= m/inf+ (mf/gamma m/inf+))
  (is (m/nan? (mf/gamma m/nan)))
  (is= 9.513507698668732 (mf/gamma 0.1))
  (is= 1.0 (mf/gamma 1.0))
  (is= -4.626098277572807 (mf/gamma -2.1))
  ;;Math 0.951350769866873183629248717726540219255057862608837734305000
  (is= 0.9513507698668734 (mf/gamma 1.1))
  ;;Math 1.298055332647557785681171179152811617784141170553946247921645
  (is= 1.2980553326475581 (mf/gamma 0.7))
  ;;;Math -10.6862870211931935489730533569448077816983878506097317904937
  (is= -10.686287021193193 (mf/gamma -0.1)))

(deftest lower-gamma-test
  (is (m/nan? (mf/lower-gamma m/nan m/nan)))
  (is= 0.0 (mf/lower-gamma m/inf+ 0.0))
  (is= 0.0 (mf/lower-gamma 0.1 0.0))
  (is= 0.8775435717470181 (mf/lower-gamma 1.0 2.1))
  (is= 1.0 (mf/lower-gamma 0.1 m/inf+))
  (is (m/nan? (mf/lower-gamma m/nan m/inf+)))
  (is (m/nan? (mf/lower-gamma m/inf+ m/nan)))
  (is (m/nan? (mf/lower-gamma 1.0 m/nan)))
  (is= 0.6671289163019205 (mf/lower-gamma 1 1.1))
  (is= 0.6321205588285577 (mf/lower-gamma 1 1))
  (is= 0.0 (mf/lower-gamma 1 0))
  (is= 9.283972028379889 (mf/lower-gamma 0.1 1)))

(deftest upper-gamma-test
  (is (m/nan? (mf/upper-gamma m/nan m/nan)))
  (is= m/inf+ (mf/upper-gamma m/inf+ 0.0))
  (is= 9.513507698668732 (mf/upper-gamma 0.1 0.0))
  (is= 0.1224564282529819 (mf/upper-gamma 1.0 2.1))
  (is= 0.0 (mf/upper-gamma 0.1 m/inf+))
  (is (m/nan? (mf/upper-gamma m/nan m/inf+)))
  (is (m/nan? (mf/upper-gamma m/inf+ m/nan)))
  (is (m/nan? (mf/upper-gamma 1.0 m/nan)))
  (is= 0.33287108369807955 (mf/upper-gamma 1 1.1))
  (is= 0.36787944117144233 (mf/upper-gamma 1 1))
  (is= 1.0 (mf/upper-gamma 1 0))
  (is= 0.22953567028884256 (mf/upper-gamma 0.1 1)))

(deftest upper-gamma-derivative-x-test
  (is (m/nan? (mf/upper-gamma-derivative-x m/nan m/nan)))
  (is= 0.0 (mf/upper-gamma-derivative-x m/inf+ 0.0))
  (is (m/nan? (mf/upper-gamma-derivative-x m/inf+ m/inf+)))
  (is (m/nan? (mf/upper-gamma-derivative-x m/inf+ 1.0)))
  (is= m/inf+ (mf/upper-gamma-derivative-x 0.1 0.0))
  (is= 0.1224564282529819 (mf/upper-gamma-derivative-x 1.0 2.1))
  (is= 0.0 (mf/upper-gamma-derivative-x 0.1 m/inf+))
  (is (m/nan? (mf/upper-gamma-derivative-x m/nan m/inf+)))
  (is (m/nan? (mf/upper-gamma-derivative-x m/inf+ m/nan)))
  (is (m/nan? (mf/upper-gamma-derivative-x 1.0 m/nan)))
  (is= 0.33287108369807955 (mf/upper-gamma-derivative-x 1 1.1))
  (is= 0.36787944117144233 (mf/upper-gamma-derivative-x 1 1))
  (is= 1.0 (mf/upper-gamma-derivative-x 1 0))
  (is= 0.03866916944030238 (mf/upper-gamma-derivative-x 0.1 1)))

(deftest regularized-gamma-p-test
  (is (m/nan? (mf/regularized-gamma-p m/nan m/nan)))
  (is= 0.0 (mf/regularized-gamma-p m/inf+ 0.0))
  (is= 0.0 (mf/regularized-gamma-p 0.1 0.0))
  (is= 0.8775435717470181 (mf/regularized-gamma-p 1.0 2.1))
  (is= 1.0 (mf/regularized-gamma-p 0.1 m/inf+))
  (is (m/nan? (mf/regularized-gamma-p m/nan m/inf+)))
  (is (m/nan? (mf/regularized-gamma-p m/inf+ m/nan)))
  (is (m/nan? (mf/regularized-gamma-p 1.0 m/nan)))
  (is= 0.6671289163019202 (mf/regularized-gamma-p 1 1.1))
  (is= 0.6321205588285578 (mf/regularized-gamma-p 1 1))
  (is= 0.0 (mf/regularized-gamma-p 1 0))
  (is= 0.9758726562736726 (mf/regularized-gamma-p 0.1 1)))

(deftest regularized-gamma-q-test
  (is (m/nan? (mf/regularized-gamma-q m/nan m/nan)))
  (is= 1.0 (mf/regularized-gamma-q m/inf+ 0.0))
  (is= 1.0 (mf/regularized-gamma-q 0.1 0.0))
  (is= 0.1224564282529819 (mf/regularized-gamma-q 1.0 2.1))
  (is= 0.0 (mf/regularized-gamma-q 0.1 m/inf+))
  (is (m/nan? (mf/regularized-gamma-q m/nan m/inf+)))
  (is (m/nan? (mf/regularized-gamma-q m/inf+ m/nan)))
  (is (m/nan? (mf/regularized-gamma-q 1.0 m/nan)))
  (is= 0.33287108369807983 (mf/regularized-gamma-q 1 1.1))
  (is= 0.3678794411714422 (mf/regularized-gamma-q 1 1))
  (is= 1.0 (mf/regularized-gamma-q 1 0))
  (is= 0.02412734372632741 (mf/regularized-gamma-q 0.1 1)))

(deftest log-gamma-test
  (is= m/inf+ (mf/log-gamma m/inf+))
  (is (m/nan? (mf/log-gamma m/nan)))
  (is= 2.2527126517342055 (mf/log-gamma 0.1))
  (is= 0.04543773854448518 (mf/log-gamma 2.1))
  (is= -0.049872441259839764 (mf/log-gamma 1.1))
  (is= 0.0 (mf/log-gamma 1))
  (is= 0.2608672465316666 (mf/log-gamma 0.7)))

(deftest log-gamma-derivative-test                          ;same as digamma
  (is= m/inf+ (mf/log-gamma-derivative m/inf+))
  (is (m/nan? (mf/log-gamma-derivative m/nan)))
  (is= -10.423754943278134 (mf/log-gamma-derivative 0.1))
  (is= -0.5772156677920671 (mf/log-gamma-derivative 1))
  (is= 0.48533596581277155 (mf/log-gamma-derivative 2.1))
  (is= -0.4237549432781376 (mf/log-gamma-derivative 1.1))
  (is= -1.2200235564290471 (mf/log-gamma-derivative 0.7))
  (is= 0.7031566378697294 (mf/digamma 2.5))
  (is= 1.1031566378697286 (mf/digamma -2.5))
  (is= m/inf- (mf/digamma -2.0))
  (is= (mf/log-gamma-derivative 1.0) (mf/digamma 1.0)))

(deftest gamma-derivative-test
  (is= m/inf+ (mf/gamma-derivative m/inf+))
  (is (m/nan? (mf/gamma-derivative m/nan)))
  (is= -99.16647290191278 (mf/gamma-derivative 0.1))
  (is= -0.5772156677920671 (mf/gamma-derivative 1))
  (is= 0.507897219192069 (mf/gamma-derivative 2.1))
  (is= -0.40313959152254947 (mf/gamma-derivative 1.1))
  (is= -1.0428235898368972 (mf/gamma-derivative -2.5))
  (is= -1.5836580833783638 (mf/gamma-derivative 0.7)))

(deftest trigamma-test
  (is= m/inf+ (mf/trigamma m/inf+))
  (is (m/nan? (mf/trigamma m/nan)))
  (is= 101.43329914974142 (mf/trigamma 0.1))
  (is= 0.6068528687496855 (mf/trigamma 2.1))
  (is= 1.4332991497414205 (mf/trigamma 1.1))
  (is= 1.6449340657861162 (mf/trigamma 1))
  (is= m/inf+ (mf/trigamma 0.0))
  (is= 2.8340491557052214 (mf/trigamma 0.7))
  (is= 101.9225399585074 (mf/trigamma -0.1))
  (is= m/inf+ (mf/trigamma -2.0)))

(deftest multivariate-gamma-test
  (is (m/nan? (mf/multivariate-gamma m/nan 0)))
  (is= 1.0 (mf/multivariate-gamma m/inf+ 0))
  (is= 1.0 (mf/multivariate-gamma 0.1 0))
  (is= 1.0 (mf/multivariate-gamma 1.1 0))
  (is= 0.9513507698668734 (mf/multivariate-gamma 1.1 1))
  (is= 2.511113699545877 (mf/multivariate-gamma 1.1 2))
  (is= 75.05107616754486 (mf/multivariate-gamma 1.1 3)))

(deftest multivariate-log-gamma-test
  (is (m/nan? (mf/multivariate-log-gamma m/nan 0)))
  (is= 0.0 (mf/multivariate-log-gamma m/inf+ 0))
  (is= 0.0 (mf/multivariate-log-gamma 0.1 0))
  (is= 0.0 (mf/multivariate-log-gamma 1.1 0))
  (is= -0.049872441259839764 (mf/multivariate-log-gamma 1.1 1))
  (is= 0.9207263597340951 (mf/multivariate-log-gamma 1.1 2)))

(defspec-test test-gamma `mf/gamma)
(defspec-test test-lower-gamma `mf/lower-gamma)
(defspec-test test-upper-gamma `mf/upper-gamma)
(defspec-test test-upper-gamma-derivative-x `mf/upper-gamma-derivative-x)
(defspec-test test-regularized-gamma-p `mf/regularized-gamma-p)
(defspec-test test-regularized-gamma-q `mf/regularized-gamma-q)
(defspec-test test-log-gamma `mf/log-gamma)
;(defspec-test test-log-gamma-derivative `mf/log-gamma-derivative)
;(defspec-test test-digamma `mf/digamma)
;(defspec-test test-gamma-derivative `mf/gamma-derivative)
;(defspec-test test-trigamma `mf/trigamma)
(defspec-test test-multivariate-gamma `mf/multivariate-gamma)
(defspec-test test-multivariate-log-gamma `mf/multivariate-log-gamma)

;;;BETA
(deftest beta-test
  (is (m/nan? (mf/beta m/nan m/nan)))
  (is= 0.47619047619047616 (mf/beta 1.0 2.1))
  (is (m/nan? (mf/beta 0.1 m/inf+)))
  (is (m/nan? (mf/beta m/nan m/inf+)))
  (is (m/nan? (mf/beta m/inf+ m/nan)))
  (is (m/nan? (mf/beta 1.0 m/nan)))
  (is= 0.9090909090909091 (mf/beta 1 1.1))
  (is= 0.9090909090909091 (mf/beta 1.1 1))
  (is= 1.0 (mf/beta 1 1))
  (is= 9.999999999999998 (mf/beta 0.1 1))
  (is= 9.999999999999998 (mf/beta 1 0.1)))

(deftest log-beta-test
  (is (m/nan? (mf/log-beta m/nan m/nan)))
  (is= -0.7419373447293773 (mf/log-beta 1.0 2.1))
  (is (m/nan? (mf/log-beta 0.1 m/inf+)))
  (is (m/nan? (mf/log-beta m/nan m/inf+)))
  (is (m/nan? (mf/log-beta m/inf+ m/nan)))
  (is (m/nan? (mf/log-beta 1.0 m/nan)))
  (is= -0.09531017980432493 (mf/log-beta 1 1.1))
  (is= -0.09531017980432493 (mf/log-beta 1.1 1))
  (is= 0.0 (mf/log-beta 1 1))
  (is= 2.302585092994046 (mf/log-beta 0.1 1))
  (is= 2.302585092994046 (mf/log-beta 1 0.1)))

(deftest regularized-beta-test
  (is= 0.7667417521157982 (mf/regularized-beta 0.5 1.0 2.1))
  (is= 1.0 (mf/regularized-beta 1 1 1.1))
  (is= 1.0 (mf/regularized-beta 1 1.1 1))
  (is= 1.0 (mf/regularized-beta 1 1 1))
  (is= 0.8865681505652135 (mf/regularized-beta 0.3 0.1 1))
  (is= 0.9330329915368076 (mf/regularized-beta 0.5 0.1 1))
  (is= 0.9649610951198179 (mf/regularized-beta 0.7 0.1 1))
  (is= 1.0 (mf/regularized-beta 1 1 0.1))
  (is= 0.5000000000000001 (mf/regularized-beta 0.5 0.5 0.5))
  (is= 0.0 (mf/regularized-beta 0 1 1)))

(deftest incomplete-beta-test
  (is= 0.36511512005514196 (mf/incomplete-beta 0.5 1.0 2.1))
  (is= 0.9090909090909091 (mf/incomplete-beta 1 1 1.1))
  (is= 0.9090909090909091 (mf/incomplete-beta 1 1.1 1))
  (is= 1.0 (mf/incomplete-beta 1 1 1))
  (is= 8.865681505652134 (mf/incomplete-beta 0.3 0.1 1))
  (is= 9.330329915368075 (mf/incomplete-beta 0.5 0.1 1))
  (is= 9.649610951198177 (mf/incomplete-beta 0.7 0.1 1))
  (is= 9.999999999999998 (mf/incomplete-beta 1 1 0.1))
  (is= 1.5707963267948968 (mf/incomplete-beta 0.5 0.5 0.5))
  (is= 0.0 (mf/incomplete-beta 0 1 1)))

(defspec-test test-beta `mf/beta)
(defspec-test test-log-beta `mf/log-beta)
(defspec-test test-regularized-beta `mf/regularized-beta)
(defspec-test test-incomplete-beta `mf/incomplete-beta)

#_(ost/unstrument)