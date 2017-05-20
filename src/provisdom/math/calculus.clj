(ns provisdom.math.calculus
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as st]
            [provisdom.utility-belt [core :as co]
             [async :as as]]
            [provisdom.math [core :as m]
             [matrix :as mx]
             [combinatorics :as mc]]
            [taoensso.truss :as truss :refer (have have! have?)]))

(set! *warn-on-reflection* true)

;;;;LOOK AT STIELTJES STUFF OR WHATEVER FOR DISCRETIZING A GAUSSIAN FOR MORE 
;;;;THAN 11 OUTCOMES -- KRONROD? vs. GAUSS? vs ...?
;;;;for integration:
;;;;    Write Clenshaw-Curtis and Fejer algos; see Wiki and 
;;;;see http://www.gnu.org/software/gsl/manual/html_node/ for more
;;;;    http://ab-initio.mit.edu/wiki/index.php/Cubature
;;;;    extend to n-dim integration over functions that output vectors 
;;;;       or matrices
;;;;work on integer integration

;;;INTEGER INTEGRATION
;;is it sometimes better to approximate slow functions 
;;  over large ranges with continuous integration?
(defn integer-integrate
  "Returns the integral of a function f over ranges from a to b.
      Options:
          ranges ([a1 b1] [a2 b2]...); default is nil"
  [f ranges]
  (let [ranges (if (sequential? (first ranges)) ranges [ranges])
        nvars (count ranges)]
    ;;this can be sped for larger seqs (use ^doubles or even approximate)
    (if (m/one? nvars) (let [[a b] ranges] (mx/esum (map f (range a b))))
                       (throw (ex-info "Not implmented" (var integer-integrate)))))) ;;to do

;;;NUMERICAL INTEGRATION
;;;GAUSSIAN-KRONROD CONSTANTS
;;;can be found here: 
;;;http://www.advanpix.com/2011/11/07/gauss-kronrod-quadrature-nodes-weights/
;;;or more generally, can be calculated here: 
;;;http://keisan.casio.com/exec/system/1289382036
(defn- ^:const convert-gk-wt
  ([v] (convert-gk-wt v true))
  ([v r?] (let [end (if r? (rest v) v)] (-> v rseq (concat end)))))

(defn- ^:const convert-gk-n [v] (concat (->> v rseq (map -)) (rest v)))

(def ^:const ^:private gauss7-weights
  [0.4179591836734693877551020, 0.3818300505051189449503698,
   0.2797053914892766679014678, 0.1294849661688696932706114])

(def ^:const ^:private kronrod15-weights
  [0.2094821410847278280129992, 0.2044329400752988924141620,
   0.1903505780647854099132564, 0.1690047266392679028265834,
   0.1406532597155259187451896, 0.1047900103222501838398763,
   0.0630920926299785532907007, 0.0229353220105292249637320])

(def ^:const ^:private kronrod15-nodes
  [0.0, 0.2077849550078984676006894, 0.4058451513773971669066064,
   0.5860872354676911302941448, 0.7415311855993944398638648,
   0.8648644233597690727897128, 0.9491079123427585245261897,
   0.9914553711208126392068547])

(def ^:const ^:private g7-k15
  [(convert-gk-wt gauss7-weights) (convert-gk-wt kronrod15-weights)
   (convert-gk-n kronrod15-nodes)])

(def ^:const ^:private gauss10-weights
  [0.2955242247147528701738930, 0.2692667193099963550912269,
   0.2190863625159820439955349, 0.1494513491505805931457763,
   0.0666713443086881375935688])

(def ^:const ^:private kronrod21-weights
  [0.1494455540029169056649365, 0.1477391049013384913748415,
   0.1427759385770600807970943, 0.1347092173114733259280540,
   0.1234919762620658510779581, 0.1093871588022976418992106,
   0.0931254545836976055350655, 0.0750396748109199527670431,
   0.0547558965743519960313813, 0.0325581623079647274788190,
   0.0116946388673718742780644])

(def ^:const ^:private kronrod21-nodes
  [0.0, 0.1488743389816312108848260, 0.2943928627014601981311266,
   0.4333953941292471907992659, 0.5627571346686046833390001,
   0.6794095682990244062343274, 0.7808177265864168970637176,
   0.8650633666889845107320967, 0.9301574913557082260012072,
   0.9739065285171717200779640, 0.9956571630258080807355273])

(def ^:const ^:private g10-k21
  [(convert-gk-wt gauss10-weights false) (convert-gk-wt kronrod21-weights)
   (convert-gk-n kronrod21-nodes)])

(def ^:const ^:private gauss15-weights
  [0.2025782419255612728806202, 0.1984314853271115764561183,
   0.1861610000155622110268006, 0.1662692058169939335532009,
   0.1395706779261543144478048, 0.1071592204671719350118695,
   0.0703660474881081247092674, 0.0307532419961172683546284])

(def ^:const ^:private kronrod31-weights
  [0.1013300070147915490173748, 0.1007698455238755950449467,
   0.0991735987217919593323932, 0.0966427269836236785051799,
   0.0931265981708253212254869, 0.0885644430562117706472754,
   0.0830805028231330210382892, 0.0768496807577203788944328,
   0.0698541213187282587095201, 0.0620095678006706402851392,
   0.0534815246909280872653431, 0.0445897513247648766082273,
   0.0353463607913758462220379, 0.0254608473267153201868740,
   0.0150079473293161225383748, 0.0053774798729233489877921])

(def ^:const ^:private kronrod31-nodes
  [0.0, 0.1011420669187174990270742, 0.2011940939974345223006283,
   0.2991800071531688121667800, 0.3941513470775633698972074,
   0.4850818636402396806936557, 0.5709721726085388475372267,
   0.6509967412974169705337359, 0.7244177313601700474161861,
   0.7904185014424659329676493, 0.8482065834104272162006483,
   0.8972645323440819008825097, 0.9372733924007059043077589,
   0.9677390756791391342573480, 0.9879925180204854284895657,
   0.9980022986933970602851728])

(def ^:const ^:private g15-k31
  [(convert-gk-wt gauss15-weights) (convert-gk-wt kronrod31-weights)
   (convert-gk-n kronrod31-nodes)])

(def ^:const ^:private gauss20-weights
  [0.1527533871307258506980843, 0.1491729864726037467878287,
   0.1420961093183820513292983, 0.1316886384491766268984945,
   0.1181945319615184173123774, 0.1019301198172404350367501,
   0.0832767415767047487247581, 0.0626720483341090635695065,
   0.0406014298003869413310400, 0.0176140071391521183118620])

(def ^:const ^:private kronrod41-weights
  [0.0766007119179996564450499, 0.0763778676720807367055028,
   0.0757044976845566746595428, 0.0745828754004991889865814,
   0.0730306903327866674951894, 0.0710544235534440683057904,
   0.0686486729285216193456234, 0.0658345971336184221115636,
   0.0626532375547811680258701, 0.0591114008806395723749672,
   0.0551951053482859947448324, 0.0509445739237286919327077,
   0.0464348218674976747202319, 0.0416688733279736862637883,
   0.0366001697582007980305572, 0.0312873067770327989585431,
   0.0258821336049511588345051, 0.0203883734612665235980102,
   0.0146261692569712529837880, 0.0086002698556429421986618,
   0.0030735837185205315012183])

(def ^:const ^:private kronrod41-nodes
  [0.0, 0.0765265211334973337546404, 0.1526054652409226755052202,
   0.2277858511416450780804962, 0.3016278681149130043205554,
   0.3737060887154195606725482, 0.4435931752387251031999922,
   0.5108670019508270980043641, 0.5751404468197103153429460,
   0.6360536807265150254528367, 0.6932376563347513848054907,
   0.7463319064601507926143051, 0.7950414288375511983506388,
   0.8391169718222188233945291, 0.8782768112522819760774430,
   0.9122344282513259058677524, 0.9408226338317547535199827,
   0.9639719272779137912676661, 0.9815078774502502591933430,
   0.9931285991850949247861224, 0.9988590315882776638383156])

(def ^:const ^:private g20-k41
  [(convert-gk-wt gauss20-weights false) (convert-gk-wt kronrod41-weights)
   (convert-gk-n kronrod41-nodes)])

(def ^:const ^:private gauss25-weights
  [0.1231760537267154512039029, 0.1222424429903100416889595,
   0.1194557635357847722281781, 0.1148582591457116483393255,
   0.1085196244742636531160940, 0.1005359490670506442022069,
   0.0910282619829636498114972, 0.0801407003350010180132350,
   0.0680383338123569172071872, 0.0549046959758351919259369,
   0.0409391567013063126556235, 0.0263549866150321372619018,
   0.0113937985010262879479030])

(def ^:const ^:private kronrod51-weights
  [0.0615808180678329350787598, 0.0614711898714253166615441,
   0.0611285097170530483058590, 0.0605394553760458629453603,
   0.0597203403241740599790993, 0.0586896800223942079619742,
   0.0574371163615678328535827, 0.0559508112204123173082407,
   0.0542511298885454901445434, 0.0523628858064074758643667,
   0.0502776790807156719633253, 0.0479825371388367139063923,
   0.0455029130499217889098706, 0.0428728450201700494768958,
   0.0400838255040323820748393, 0.0371162714834155435603306,
   0.0340021302743293378367488, 0.0307923001673874888911090,
   0.0274753175878517378029485, 0.0240099456069532162200925,
   0.0204353711458828354565683, 0.0168478177091282982315167,
   0.0132362291955716748136564, 0.0094739733861741516072077,
   0.0055619321353567137580402, 0.0019873838923303159265079])

(def ^:const ^:private kronrod51-nodes
  [0.0, 0.0615444830056850788865464, 0.1228646926107103963873598,
   0.1837189394210488920159699, 0.2438668837209884320451904,
   0.3030895389311078301674789, 0.3611723058093878377358217,
   0.4178853821930377488518144, 0.4730027314457149605221821,
   0.5263252843347191825996238, 0.5776629302412229677236898,
   0.6268100990103174127881227, 0.6735663684734683644851206,
   0.7177664068130843881866541, 0.7592592630373576305772829,
   0.7978737979985000594104109, 0.8334426287608340014210211,
   0.8658470652932755954489970, 0.8949919978782753688510420,
   0.9207471152817015617463461, 0.9429745712289743394140112,
   0.9616149864258425124181300, 0.9766639214595175114983154,
   0.9880357945340772476373310, 0.9955569697904980979087849,
   0.9992621049926098341934575])

(def ^:const ^:private g25-k51
  [(convert-gk-wt gauss25-weights) (convert-gk-wt kronrod51-weights)
   (convert-gk-n kronrod51-nodes)])

(def ^:const ^:private gauss30-weights
  [0.1028526528935588403412856, 0.1017623897484055045964290,
   0.0995934205867952670627803, 0.0963687371746442596394686,
   0.0921225222377861287176327, 0.0868997872010829798023875,
   0.0807558952294202153546949, 0.0737559747377052062682439,
   0.0659742298821804951281285, 0.0574931562176190664817217,
   0.0484026728305940529029381, 0.0387991925696270495968019,
   0.0287847078833233693497192, 0.0184664683110909591423021,
   0.0079681924961666056154659])

(def ^:const ^:private kronrod61-weights
  [0.0514947294294515675583404, 0.0514261285374590259338629,
   0.0512215478492587721706563, 0.0508817958987496064922975,
   0.0504059214027823468408931, 0.0497956834270742063578116,
   0.0490554345550297788875282, 0.0481858617570871291407795,
   0.0471855465692991539452615, 0.0460592382710069881162717,
   0.0448148001331626631923556, 0.0434525397013560693168317,
   0.0419698102151642461471475, 0.0403745389515359591119953,
   0.0386789456247275929503487, 0.0368823646518212292239111,
   0.0349793380280600241374997, 0.0329814470574837260318142,
   0.0309072575623877624728843, 0.0287540487650412928439788,
   0.0265099548823331016106017, 0.0241911620780806013656864,
   0.0218280358216091922971675, 0.0194141411939423811734090,
   0.0169208891890532726275723, 0.0143697295070458048124514,
   0.0118230152534963417422329, 0.0092732796595177634284411,
   0.0066307039159312921733198, 0.0038904611270998840512672,
   0.0013890136986770076245516])

(def ^:const ^:private kronrod61-nodes
  [0.0, 0.0514718425553176958330252, 0.1028069379667370301470968,
   0.1538699136085835469637947, 0.2045251166823098914389577,
   0.2546369261678898464398051, 0.3040732022736250773726771,
   0.3527047255308781134710372, 0.4004012548303943925354762,
   0.4470337695380891767806099, 0.4924804678617785749936931,
   0.5366241481420198992641698, 0.5793452358263616917560249,
   0.6205261829892428611404776, 0.6600610641266269613700537,
   0.6978504947933157969322924, 0.7337900624532268047261711,
   0.7677774321048261949179773, 0.7997278358218390830136689,
   0.8295657623827683974428981, 0.8572052335460610989586585,
   0.8825605357920526815431165, 0.9055733076999077985465226,
   0.9262000474292743258793243, 0.9443744447485599794158313,
   0.9600218649683075122168710, 0.9731163225011262683746939,
   0.9836681232797472099700326, 0.9916309968704045948586284,
   0.9968934840746495402716301, 0.9994844100504906375713259])

(def ^:const ^:private g30-k61
  [(convert-gk-wt gauss30-weights false) (convert-gk-wt kronrod61-weights)
   (convert-gk-n kronrod61-nodes)])

(defn- adj-tol-gk ^double [^double tol] (-> tol (m/pow (/ 2.0 3.0)) (* 0.005)))

(defn- weights-and-nodes-gk [^Number points]
  (condp >= points 15 g7-k15, 21 g10-k21, 31 g15-k31, 41 g20-k41, 51 g25-k51,
                   g30-k61))

;ADAPTIVE INTEGRATION
(defn- unnormalize
  "Returns the unnormalized value for a range [a, b] with value v from normalized range [-1, 1]"
  ^double [^double half-sum ^double half-diff ^double v]
  (-> v (* half-diff) (+ half-sum)))

(defn- get-error-and-value
  "Returns a tuple containing the error and higher precision value for a single integration approximation"
  [f [a b] [lw hw n]]
  (let [half-sum (* 0.5 (+ a b))
        half-diff (* 0.5 (- b a))
        un (map (partial unnormalize half-sum half-diff) n)
        lh (into [] (map f un))
        h (mx/mul half-diff (mx/inner-product hw lh))
        l (mx/mul
            half-diff
            (mx/inner-product lw (mx/filter-kv (fn [idx _] (odd? idx)) lh)))
        err (mx/eaverage (mx/abs (mx/sub l h)))]
    [err h]))

(defn- get-error-and-value-ndim
  "Returns a vector containing the error, the higher precision value, 
      a vector of the 1-dim errors for a single integration approximation, and the ranges (unchanged)"
  [f ranges [lw hw n]]
  (let [half-sum (map mx/eaverage ranges)
        half-diff (map #(* 0.5 (- (second %) (first %))) ranges)
        un (for [v (range (count ranges))]
             (map (partial unnormalize (nth half-sum v) (nth half-diff v)) n))
        lh (map f (apply mc/cartesian-product un))
        mult (mx/eproduct half-diff)
        dim (count ranges)
        fl #(mx/inner-product lw (take-nth 2 (rest %)))
        fh #(mx/inner-product hw %)
        f1dim (co/create-dbl-layered dim dim (fn [i j] (if (= i j) fl fh)))
        fallh (repeat dim fh)
        falll (repeat dim fl)
        partitioned-lh (co/partition-recursively lh (count hw))
        reduce-f #(mx/mul mult (reduce (fn [tot ef] (ef tot))
                                       partitioned-lh %))
        h (reduce-f fallh)
        l (reduce-f falll)
        dim1 (map reduce-f f1dim)
        err-f #(mx/eaverage (mx/abs (mx/sub % h)))]
    [(err-f l) h (map err-f dim1) ranges]))

(defn- adaptive-quadrature [implementation f [a b] accu min-iter max-iter wn]
  (let [tol (adj-tol-gk accu)
        [err0 h0] (get-error-and-value f [a b] wn)
        ftot-val #(mx/coerce
                    implementation
                    (apply mx/add (map (fn [e] (-> e second first)) %)))]
    (loop [errs [[err0 [h0 a b]]], i 1]
      (let [tot-err (mx/esum (map first errs))]
        (if (and (>= i min-iter) (<= tot-err tol))
          (ftot-val errs)
          (do (when (>= i max-iter)
                (throw (ex-info (str "Iteration limit reached.  Error: " tot-err
                                     " Value: " (ftot-val errs))
                                {:fn (var adaptive-quadrature) :solver? true})))
              (let [[e [h an bn]] (peek errs)
                    mn (* 0.5 (+ an bn))
                    [[err1 h1] [err2 h2]] (as/thread
                                            :all
                                            [#(get-error-and-value f [an mn] wn)
                                             #(get-error-and-value f [mn bn]
                                                                   wn)])
                    errs (into
                           []
                           (sort-by
                             first (concat
                                     (pop errs)
                                     [[err1 [h1 an mn]] [err2 [h2 mn bn]]])))]
                (recur errs (inc i)))))))))

(defn- simple-sort-fn [err errs1d] (apply max errs1d))

(defn- simple-select-dims-fn [errs1d i min-iter]
  (let [m (apply max errs1d)
        ps (map vector (range) errs1d)]
    (if (< i min-iter) [(ffirst (filter #(= (second %) m) ps))]
                       (mapv first (filter #(>= (second %) (* m 0.1)) ps)))))

(defn- adaptive-quadrature-ndim
  "sort-fn takes the error of the approx and the sequence of 1-dim errors, 
      and should return a number representing it's importance in splitting.
   select-dims-fn takes the sequence of 1-dim errors, and should return 
      a vector of the dimensions to split."
  [implementation f ranges accu min-iter max-iter wn sort-fn select-dims-fn]
  (let [tol (adj-tol-gk accu),
        [err0 h0 errs1d0 _] (get-error-and-value-ndim f ranges wn),
        ftot-val #(mx/coerce
                    implementation
                    (apply mx/add (map (fn [e] (-> e second second)) %)))]
    (loop [errs [[(sort-fn err0 errs1d0) [err0 h0 errs1d0 ranges]]], i 1]
      (let [tot-err (mx/esum (map (fn [e] (-> e second first)) errs))]
        (if (and (>= i min-iter) (<= tot-err tol))
          (ftot-val errs)
          (do (when (>= i max-iter)
                (throw (ex-info (str "Iteration limit reached.  Error: " tot-err " Value: " (ftot-val errs))
                                {:fn (var adaptive-quadrature-ndim)})))
              (let [[_ [_ _ e1d rn]] (peek errs),
                    dims (select-dims-fn e1d i min-iter),
                    dims-with-new-ranges (map #(let [[an bn] (nth rn %),
                                                     mn (* 0.5 (+ an bn))]
                                                 [[% an mn] [% mn bn]]) dims)
                    new-ranges (map #(reduce
                                       (fn [tot [d an bn]]
                                         (assoc tot d [an bn])) rn %)
                                    (apply mc/cartesian-product
                                           dims-with-new-ranges))
                    new-fns (map (fn [r] #(get-error-and-value-ndim f r wn))
                                 new-ranges)
                    new-errs (map (fn [[e h errs1d r]]
                                    [(sort-fn e errs1d)
                                     [e h errs1d r]]) (as/thread :all new-fns))
                    errs (into [] (sort-by first (concat (pop errs) new-errs)))]
                (recur errs (inc i)))))))))

(defn change-of-variable
  "Returns two functions and the new range as a triple.  
   The first function is a multiplicative function.  
   The second is the converter for within the function to integrate."
  [[a b]]
  (cond
    (and (m/inf-? a) (m/inf+? b)) [#(let [s (m/sq %)]
                                      (/ (inc s) (m/sq (m/one- s)))),
                                   #(/ % (m/one- (m/sq %))), [-1 1]]
    (m/inf+? b) [#(/ (m/sq %)), #(+ a (/ (m/one- %) %)), [0 1]]
    (m/inf-? a) [#(/ (m/sq %)), #(- b (/ (m/one- %) %)), [0 1]]
    :else [(constantly 1.0), identity, [a b]]))

(defn- change-of-var
  "Returns the new function and range as a tuple"
  [f [a b]]
  (let [[fm fv r] (change-of-variable [a b])]
    [#(mx/mul (fm %) (f (fv %))) r]))

(defn- change-of-var-ndim
  "Returns the new function and ranges.  
   Function f takes a sequence and returns a number."
  [f ranges]
  (let [trips (map change-of-variable ranges), fms (map first trips),
        fvs (map second trips), newr (into [] (map #(nth % 2) trips))]
    [#(mx/mul (mx/eproduct (co/in-place-functional fms %))
              (f (co/in-place-functional fvs %))) newr]))

(defn integrate
  "Returns the integral of a function f over ranges from a to b using 
      global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   f can also return a vector or matrix in 1D
   ranges can be [a b] or ([a1 b1] [a2 b2]...)
   Options:
       points 15, 21, 31, 41, 51, 61; default is 21 or 15 if any range      
          is infinite or 2+ variables
       accu default is m/*dbl-close* and (m/*sgl-close* for 4+ variables 
          or for 3+ variables if any range infinite) 
          (accuracy is adjusted for quadrature type)
       max-iter default is (/ m/*max-iter* 10)
       min-iter default is m/*min-iter*
   Known Limitations:
       very large absolute ranges (not infinite) can cause approximation 
           errors due to limited rounded accuracy of the 'double' type 
           example: exp(-x^2) from -166 (or more negative) to inf returns 
              zero instead of sqrt of pi with default min-iter 
              and 51 points.
           solution: use a change of variable or increase min-iter 
              or increase points
       with more than 4 dimensions, use Monte Carlo simulation instead 
          for speed"
  [f ranges & {:keys [points accu min-iter max-iter]
               :or   {min-iter m/*min-iter*, max-iter (/ m/*max-iter* 10)}}]
  (let [ranges (if (sequential? (first ranges)) ranges [ranges])
        nvars (count ranges)
        cov (some m/inf? (flatten ranges))
        points (cond points points, (or cov (> nvars 1)) 15, :else 21)
        accu (cond accu accu,
                   (or (and cov (>= nvars 3)) (>= nvars 4)) m/*sgl-close*,
                   :else m/*dbl-close*)
        [newf newr] (if (m/one? nvars) (change-of-var f (first ranges))
                                       (change-of-var-ndim f ranges))
        implementation (newf (if (m/one? nvars) (mx/eaverage newr)
                                                (map mx/eaverage newr)))]
    (if (m/one? nvars)
      (adaptive-quadrature
        implementation newf newr accu min-iter max-iter
        (weights-and-nodes-gk points))
      (adaptive-quadrature-ndim
        implementation newf newr accu min-iter max-iter
        (weights-and-nodes-gk points) simple-sort-fn simple-select-dims-fn))))

(defn integrate-non-rectangular-2D
  "Returns the integral of a function f over ranges from a to b using 
      global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Use 'integrate' for rectangular integration because faster.
   f should be a function of (outer, inner).
   outer-range is [a b]
   inner-range-fn takes a value and returns a range [c d]
   Options:
       points 15, 21, 31, 41, 51, 61; default is 15
       accu default is m/*dbl-close* per dimension 
          (accuracy is adjusted for quadrature type)
       max-iter default per dimension is (/ m/*max-iter* 10)
       min-iter default per dimension is m/*min-iter*"
  [f outer-range inner-range-fn
   & {:keys [points accu min-iter max-iter]
      :or   {points   15, accu m/*dbl-close*, min-iter m/*min-iter*,
             max-iter (/ m/*max-iter* 10)}}]
  (integrate #(integrate (partial f %) (inner-range-fn %) :points points
                         :accu accu :min-iter min-iter :max-iter max-iter)
             outer-range :points points :accu accu :min-iter min-iter
             :max-iter max-iter))

(defn integrate-non-rectangular-3D
  "Returns the integral of a function f over ranges from a to b using 
      global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Use 'integrate' for rectangular integration because faster.
   f should be a function of (outer, middle, inner).
   outer-range is [a b]
   middle-range-rn takes a value and returns a range [c d]
   inner-range-fn takes a outer and middle value and returns a range [e f]
   Options:
       points 15, 21, 31, 41, 51, 61; default is 15
       accu default is m/*sgl-close* per dimension 
          (accuracy is adjusted for quadrature type)
       max-iter default per dimension is (/ m/*max-iter* 30)        
       min-iter default per dimension is 3"
  [f outer-range middle-range-fn inner-range-fn
   & {:keys [points accu min-iter max-iter]
      :or   {points   15, accu m/*sgl-close*, min-iter 3,
             max-iter (/ m/*max-iter* 30)}}]
  (integrate
    (fn [outer]
      (integrate
        (fn [middle]
          (integrate
            (fn [inner] (f outer middle inner)) (inner-range-fn outer middle)
            :points points :accu accu :min-iter min-iter :max-iter max-iter))
        (middle-range-fn outer) :points points :accu accu :min-iter min-iter
        :max-iter max-iter))
    outer-range :points points :accu accu :min-iter min-iter
    :max-iter max-iter))

(defn integrate-non-rectangular-4D
  "Returns the integral of a function f over ranges from a to b using global 
      adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Use 'integrate' for rectangular integration because faster.
   f should be a function of (outer, outer-middle, inner-middle, inner).
   outer-range is [a b]
   outer-middle-range-rn takes a value and returns a range [c d]
   inner-middle-range-rn takes a outer and outer-middle value and returns 
      a range [e f]
   inner-range-fn takes a outer, outer-middle, and inner-middle value and 
      returns a range [g h]
   Options:
      points 15, 21, 31, 41, 51, 61; default is 15
      accu default is m/*sgl-close* per dimension 
         (accuracy is adjusted for quadrature type)
      max-iter default per dimension is (/ m/*max-iter* 50)
      min-iter default per dimension is 2"
  [f outer-range outer-middle-range-fn inner-middle-range-fn inner-range-fn
   & {:keys [points accu min-iter max-iter]
      :or   {points   15, accu m/*sgl-close*, min-iter 2,
             max-iter (/ m/*max-iter* 50)}}]
  (integrate
    (fn [outer]
      (integrate
        (fn [outer-middle]
          (integrate
            (fn [inner-middle]
              (integrate (fn [inner] (f outer outer-middle inner-middle inner))
                         (inner-range-fn outer outer-middle inner-middle)
                         :points points :accu accu :min-iter min-iter
                         :max-iter max-iter))
            (inner-middle-range-fn outer outer-middle) :points points
            :accu accu :min-iter min-iter :max-iter max-iter))
        (outer-middle-range-fn outer) :points points :accu accu
        :min-iter min-iter :max-iter max-iter))
    outer-range :points points :accu accu :min-iter min-iter
    :max-iter max-iter))

;;;NUMERICAL DERIVATIVES
;;; references: Wiki http://en.wikipedia.org/wiki/Finite_difference 
;;;    and http://en.wikipedia.org/wiki/Finite_difference_coefficients
(comment "The zero coefficient is left out below, but can be found because 
    the sum of coefficients equals zero")
(def ^:const ^:private central-coeff1
  [[[1 (/ 2)]] [[2 (/ -12)] [1 (/ 2 3)]] [[3 (/ 60)] [2 (/ -3 20)] [1 (/ 3 4)]]
   [[4 (/ -280)] [3 (/ 4 105)] [2 (/ -5)] [1 (/ 4 5)]]])

(def ^:const ^:private central-coeff2
  [[[1 1]] [[2 (/ -12)] [1 (/ 4 3)]] [[3 (/ 90)] [2 (/ -3 20)] [1 (/ 3 2)]]
   [[4 (/ -560)] [3 (/ 8 315)] [2 (/ -5)] [1 (/ 8 5)]]])

(def ^:const ^:private central-coeff3
  [[[2 (/ 2)] [1 -1]] [[3 (/ -8)] [2 1] [1 (/ -13 8)]]
   [[4 (/ 7 240)] [3 (/ -3 10)] [2 (/ 169 120)] [1 (/ -61 30)]]])

(def ^:const ^:private central-coeff4
  [[[2 1] [1 -4]] [[3 (/ -6)] [-2 2] [1 (/ -13 2)]]
   [[4 (/ 7 240)] [3 (/ -2 5)] [2 (/ 169 60)] [1 (/ -122 15)]]])

(defn- ^:const convert-cc
  [v no-zero?] (let [r (map #(let [[e1 e2] %]
                               [(- e1) (if no-zero? (- e2) e2)]) v),
                     extra (if no-zero? nil
                                        [[0 (* -2.0 (mx/esum (map second v)))]])]
                 (concat v extra r)))

(defn- get-central-coeff
  [^long deriv ^long accuracy]
  {:pre [(have? #(>= % 2) accuracy)
         (have? #(<= % 8) accuracy)
         (have? even? accuracy)
         (have? (fn [[accuracy deriv]] (or (<= accuracy 6) (<= deriv 2))) [accuracy deriv])
         (have? #(<= % 4) deriv)
         (have? pos? deriv)]}
  (let [a (/ accuracy 2),
        v (condp = deriv 1 central-coeff1, 2 central-coeff2, 3 central-coeff3,
                         4 central-coeff4)]
    (convert-cc (nth v (dec a)) (odd? deriv))))

(def ^:const ^:private forward-coeff1
  [[[1 1]]
   [[2 (/ -2)] [1 2]]
   [[3 (/ 3)] [2 (/ -3 2)] [1 3]]
   [[4 (/ -4)] [3 (/ 4 3)] [2 -3] [1 4]]
   [[5 (/ 5)] [4 (/ -5 4)] [3 (/ 10 3)] [2 -5] [1 5]]
   [[6 (/ -6)] [5 (/ 6 5)] [4 (/ -15 4)] [3 (/ 20 3)] [2 -7.5] [1 6]]])

(def ^:const ^:private forward-coeff2
  [[[2 1] [1 -2]]
   [[3 -1] [2 4] [1 -5]]
   [[4 (/ 11 12)] [3 (/ -14 3)] [2 (/ 19 2)] [1 (/ -26 3)]]
   [[5 (/ -5 6)] [4 (/ 61 12)] [3 -13] [2 (/ 107 6)] [1 (/ -77 6)]]
   [[6 (/ 137 180)] [5 (/ -27 5)] [4 (/ 33 2)] [3 (/ -254 9)] [2 (/ 117 4)]
    [1 (/ -87 5)]]
   [[7 (/ -7 10)] [6 (/ 1019 180)] [5 (/ -201 10)] [4 41] [3 (/ -949 18)]
    [2 (/ 879 20)] [1 (/ -223 10)]]])

(def ^:const ^:private forward-coeff3
  [[[3 1] [2 -3] [1 3]]
   [[4 (/ -3 2)] [3 7] [2 -12] [1 9]]
   [[5 (/ 7 4)] [4 (/ -41 4)] [3 (/ 49 2)] [2 (/ -59 2)] [1 (/ 71 4)]]
   [[6 (/ -15 8)] [5 13] [4 (/ -307 8)] [3 62] [2 (/ -461 8)] [1 29]]
   [[7 (/ 29 15)] [6 (/ -1849 120)] [5 (/ 268 5)] [4 (/ -2545 24)]
    [3 (/ 389 3)] [2 (/ -3929 40)] [1 (/ 638 15)]]
   [[8 (/ -469 240)] [7 (/ 527 30)] [6 (/ -561 8)] [5 (/ 4891 30)]
    [4 (/ -1457 6)] [3 (/ 2391 10)] [2 (/ -18353 120)] [1 (/ 349 6)]]])

(def ^:const ^:private forward-coeff4
  [[[4 1] [3 -4] [2 6] [1 -4]]
   [[5 -2] [4 11] [3 -24] [2 26] [1 -14]]
   [[6 (/ 17 6)] [5 -19] [4 (/ 107 2)] [3 (/ -242 3)] [2 (/ 137 2)] [1 -31]]
   [[7 (/ -7 2)] [6 (/ 82 3)] [5 (/ -185 2)] [4 176] [3 (/ -1219 6)] [2 142]
    [1 (/ -111 2)]]
   [[8 (/ 967 240)] [7 (/ -536 15)] [6 (/ 2803 20)] [5 (/ -4772 15)]
    [4 (/ 10993 24)] [3 (/ -2144 5)] [2 (/ 15289 60)] [1 (/ -1316 15)]]])

(defn- get-forward-coeff
  [^long deriv ^long accuracy]
  {:pre [(have? (fn [[deriv accuracy]]
                  (and (pos? deriv)
                       (<= deriv 6)
                       (<= accuracy 6)
                       (pos? accuracy)
                       (or (<= accuracy 5) (<= deriv 3))))
                [deriv accuracy])]}
  (let [v (condp = deriv
            1 forward-coeff1
            2 forward-coeff2
            3 forward-coeff3
            4 forward-coeff4)
        coeff (nth v (dec accuracy))]
    (conj coeff [0 (- (mx/esum (map second coeff)))])))

(defn- get-backward-coeff
  "backward is like forward except for odd derivatives the sign switches"
  [^long deriv ^long accuracy]
  (let [coeff (get-forward-coeff deriv accuracy)]
    (map #(let [[e1 e2] %] [(- e1) (if (odd? deriv) (- e2) e2)]) coeff)))

(defn derivative-fn
  "Returns a numerical derivative function.  
   Function f takes a number.
   Note that derivative-fn will not be accurate when inputs or outputs are so large when divided by ::h
   that they lose precision.
   Options:
      derivative can be 0 or 1 (default) to 8
      h (default is m/*sgl-close* for 1st deriv, 10x less for others) 
         is the denominator, which is equal to (dx ^ derivative), where dx  
         is the small change (smaller h isn't usually better, changes to h can 
         be important)
      type can be :central (default), :forward, or :backward
      accuracy can be 2, 4, 6, or 8 for central (no 8 for 3rd or 4th deriv), 
         and 1-6 for forward or backward (no 6 for 4th deriv).
         (default accuracy is 2 for derivative <= 2, else 6"
  [f & {::keys [derivative h type accuracy]
        :or    {derivative 1, type :central}}]
  (let [derivative (int derivative)
        accuracy (when accuracy (int accuracy))]
    (cond (zero? derivative) f
          (> derivative 4) (let [exc (- derivative 4)
                                 x (if h (/ (* 10 h) m/*sgl-close*) 1.0)]
                             (derivative-fn
                               (derivative-fn
                                 f
                                 ::derivative exc
                                 ::h (* x (m/pow 10 (/ (+ 3 exc) -2)))
                                 ::type type
                                 ::accuracy accuracy)
                               ::derivative 4
                               ::h (* x (m/pow 10 (/ (+ 11 (- exc)) -2)))
                               ::type type
                               ::accuracy accuracy))
          :else (let [h (cond h h
                              (m/one? derivative) m/*sgl-close*
                              :else (/ m/*sgl-close* 10))
                      accuracy (cond accuracy accuracy
                                     (<= derivative 2) 2
                                     :else 6)
                      coeff-fn (condp = type
                                 :central get-central-coeff
                                 :forward get-forward-coeff
                                 :backward get-backward-coeff)
                      mult (/ h)
                      dx (m/pow h (/ derivative))
                      coeff (map #(let [[e1 e2] %] [(* dx e1) e2])
                                 (coeff-fn derivative accuracy))]
                  (fn [v]
                    (* mult (mx/esum (map #(let [[e1 e2] %]
                                             (* (f (+ v e1)) e2)) coeff))))))))

(s/def ::f (s/with-gen
             (s/fspec :args (s/cat :a ::m/number) :ret ::m/number)
             #(gen/one-of (map gen/return (list m/sq m/cube m/cos)))))
(s/def ::h ::m/finite+)
(s/def ::derivative (s/int-in 0 9))
(s/def ::type #{:central :forward :backward})
(s/def ::accuracy (s/and (s/int-in 1 9) (partial not= 7)))

(s/fdef derivative-fn
        :args (s/cat :f ::f
                     :opts
                     (s/&
                       (s/keys* :opt [::derivative ::h ::type ::accuracy])
                       (fn [v] (let [d (get v ::derivative 1)
                                     a (get v ::accuracy (if (<= d 2) 2 6))
                                     t (get v ::type :central)]
                                 (if (= t :central)
                                   (and (even? a) (or (<= d 2) (<= a 6)))
                                   (and (<= a 6) (or (<= d 3) (<= a 5))))))))
        :ret ::f)

(defn gradient-fn
  "Returns a numerical gradient function.  
   Function f takes a sequence of numbers and returns a number.
   The output function takes and returns a sequence.
   Options:
      h (default m/*sgl-close*) is the denominator, which is equal 
         to (dx ^ derivative), where dx is the small change (smaller h 
         isn't usually better, changes to h can be important)
      type can be :central (default), :forward, or :backward
      accuracy can be 2 (default), 4, 6, or 8 for central (no 8 for 3rd or 
         4th deriv), and 1-6 for forward or backward (no 6 for 4th deriv)."
  [f & {:keys [^double h type ^long accuracy]
        :or   {h m/*sgl-close*, type :central, accuracy 2}}]
  (let [coeff-fn (condp = type :central get-central-coeff,
                               :forward get-forward-coeff, :backward get-backward-coeff),
        mult (/ h),
        dx h,
        coeff (map #(let [[e1 e2] %] [(* dx e1) e2]) (coeff-fn 1 accuracy))]
    (fn [v]
      (map-indexed
        (fn [i e]
          (* mult (mx/esum
                    (map #(let [[e1 e2] %]
                            (* (f (mx/mset v i (+ e e1))) e2)) coeff)))) v))))

(defn jacobian-fn
  "Returns a numerical jacobian function.  
   Function f takes a sequence of numbers and returns a sequence.
   The output function takes a sequence and returns a double-layered sequence, 
      where each row is the gradient of f's output.
   Options:
   h (default m/*sgl-close*) is the denominator, which is equal 
      to (dx ^ derivative), where dx is the small change (smaller h isn't 
      usually better, changes to h can be important)
   type can be :central (default), :forward, or :backward
   accuracy can be 2 (default), 4, 6, or 8 for central (no 8 for 3rd or 
      4th deriv), and 1-6 for forward or backward (no 6 for 4th deriv)."
  [f & {:keys [^double h type ^long accuracy]
        :or   {h m/*sgl-close*, type :central, accuracy 2}}]
  (let [coeff-fn (condp = type :central get-central-coeff,
                               :forward get-forward-coeff, :backward get-backward-coeff),
        mult (/ h),
        dx h,
        coeff (map #(let [[e1 e2] %] [(* dx e1) e2]) (coeff-fn 1 accuracy))]
    (fn [v]
      (co/flip-dbl-layered
        (map-indexed
          (fn [i e]
            (apply
              mx/add
              (map
                #(let [[e1 e2] %] (mx/mul e2 mult (f (mx/mset v i (+ e e1)))))
                coeff)))
          v)))))

(defn- joint-central-derivative [f v i j dx mult]
  (let [i+ (mx/mset v i (+ (mx/mget v i) dx)),
        i- (mx/mset v i (- (mx/mget v i) dx)),
        e++ (mx/mset i+ j (+ (mx/mget v j) dx)),
        e+- (mx/mset i+ j (- (mx/mget v j) dx)),
        e-+ (mx/mset i- j (+ (mx/mget v j) dx)),
        e-- (mx/mset i- j (- (mx/mget v j) dx))]
    (* 0.25 mult (- (+ (f e++) (f e--)) (f e+-) (f e-+)))))

(defn hessian-fn
  "Returns a numerical Hessian function using central differences with an 
      accuracy of 2.  
   Function f takes a sequence of numbers and returns a number.
   The output function takes a sequence and returns a symmetric matrix.
   Options:
      implementation can be :clatrix, :apache-commons, nil (default nested 
         vectors), etc. or a matrix 
      h (default m/*sgl-close* / 10) is the denominator, which is equal 
         to (dx ^ derivative), where dx is the small change (smaller h 
         isn't usually better, changes to h can be important)
      type can be :joint-central (default), :central, :forward, or :backward
      accuracy can be 2 (default, only choice for joint), 4, 6, or 8 for 
         central (no 8 for 3rd or 4th deriv), and 1-6 for forward or 
         backward (no 6 for 4th deriv)."
  [f & {:keys [implementation ^double h type ^long accuracy]
        :or   {h (/ m/*sgl-close* 10), type :joint-central, accuracy 2}}]
  (if-not (= type :joint-central)
    (fn [v]
      (mx/coerce
        implementation
        ((jacobian-fn
           (gradient-fn f :h (m/sqrt h), :type type, :accuracy accuracy)
           :h (m/sqrt h), :type type, :accuracy accuracy) v)))
    (let [mult (/ h), dx (m/sqrt h),
          coeff (map #(let [[e1 e2] %]
                        [(* dx e1) e2]) (get-central-coeff 2 2))]
      (fn [v] (mx/symmetric-matrix
                implementation
                (fn [i j]
                  (if (== i j)
                    (* mult
                       (mx/esum
                         (map #(let [[e1 e2] %]
                                 (* (f (mx/mset v i (+ (mx/mget v i) e1)))
                                    e2))
                              coeff)))
                    (joint-central-derivative f v i j dx mult)))
                (count v) true)))))

(defn partial-derivative-x-of-fxy
  [fxy & {:keys [^double h] :or {h m/*sgl-close*}}]
  (fn [x y] ((derivative-fn #(fxy % y)) x)))

(defn partial-derivative-y-of-fxy
  [fxy & {:keys [^double h] :or {h m/*sgl-close*}}]
  (fn [x y] ((derivative-fn #(fxy x %)) y)))

(defn second-partial-derivative-xx-of-fxy
  [fxy & {:keys [^double h] :or {h (/ m/*sgl-close* 10)}}]
  (fn [x y] ((derivative-fn #(fxy % y) ::derivative 2) x)))

(defn second-partial-derivative-yy-of-fxy
  [fxy & {:keys [^double h] :or {h (/ m/*sgl-close* 10)}}]
  (fn [x y] ((derivative-fn #(fxy x %) ::derivative 2) y)))

(defn second-partial-derivative-xy-of-fxy
  [fxy & {:keys [^double h] :or {h (/ m/*sgl-close* 10)}}]
  (fn [x y] (joint-central-derivative
              #(fxy (first %) (second %)) [x y] 0 1 (m/sqrt h) (/ h))))

;;COMPARE AGAINST THE FOLLOWING
; Implements the adaptive quadrature described on page 511 of Numerical Analysis Kinkade et al.
; ## License
; Copyright (C) 2014 Daniel Aaron Phelps
; Distributed under the Eclipse Public License, the same as Clojure.

(defn- simpsons-estimate
  "Equation '8.5' page 509 - approximates the integral of f over [a b]."
  ^double
  [f ^double a ^double b ^double h]
  (* (/ h 3.) (+ (f a) (* 4 (f (+ a h))) (f b))))

(defn- close-enough?
  "Finds if |a - b| < |error|."
  [^double a ^double b ^double error]
  (< (Math/abs (- a b)) (Math/abs error)))

(defn- insured-approximation
  "Equation 7 page 509 in Kinkade et al."
  ^double
  [^double S* ^double S** ^double S]
  (+ S* S** (* (/ 1. 15.) (+ S* S** (* -1. S)))))

(defn- adapt-quad-internal
  "Do not call this fn directly.  Start with adaptive-quadrature instead."
  [f delta eps n k sigma a h fa fc fb S]
  (let [delta (double delta) eps (double eps)
        n (long n) k (long k)
        sigma (double sigma) a (double a)
        h (double h) fa (double fa)
        fc (double fc) fb (double fb)
        S (double S)
        b (+ a (* 2. h))
        c (+ a h)
        h (/ h 2.)
        S-left (simpsons-estimate f a c h)
        S-right (simpsons-estimate f c b h)]
    (cond
      (close-enough? (+ S-left S-right) S (/ (* 60. eps h) delta))
      (+ sigma (insured-approximation S-left S-right S))
      (>= k n) (throw (Exception. (str "Failure:  k >= n.  sigma = " sigma)))
      :else (+ (adapt-quad-internal                         ;From a to the midpoint
                 f delta eps n (inc k) sigma a h fa (f (+ a h)) fc S-left)
               (adapt-quad-internal
                 f delta eps n (inc k)                      ;From the midpoint to b
                 sigma (+ a (* 2. h)) h fc (f (+ a (* 3. h))) fb S-right)))))

(defn- adaptive-quadrature-test
  "Approximates the definite integral of f over [a b] with an error less
  or equal than eps.  f is a real valued function of one real argument.
  The parameter n specifies how many recursive calls are allowed.  An
  exception is thrown before the n+1st recursive call."
  [f a b eps n]
  (let [a (double a) b (double b)
        eps (double eps) n (long n)
        delta (- b a) sigma 0
        h (/ delta 2.) c (/ (+ a b) 2.)
        k 1 fa (f a)
        fb (f b) fc (f c)
        S (simpsons-estimate f a b h)]
    (adapt-quad-internal f delta eps n k sigma a h fa fc fb S)))

(defn- univariate-integration-test
  "Univariate Integration using Romberg."
  [f lower-bound upper-bound]
  (let [max-eval 100]
    (if (= lower-bound upper-bound)
      0.0
      (adaptive-quadrature-test f lower-bound upper-bound 1e-6 max-eval))))

(s/fdef univariate-integration-test
        :args (s/and
                (s/cat :f (s/fspec :args (s/cat :x double?) :ret double?) :lower-bound double? :upper-bound double?)
                #(> (:upper-bound %) (:lower-bound %)))
        :ret double?)