(ns provisdom.math.integrals
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.async :as async]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.combinatorics :as combo]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]))

(declare weights-and-nodes-gk)

(s/def ::number->number
  (s/with-gen
    (s/fspec :args (s/cat :number ::m/number)
             :ret ::m/number)
    #(gen/one-of (map gen/return (list m/sq m/cube m/cos)))))

(s/def ::v->tensor
  (s/with-gen
    (s/fspec :args (s/cat :v ::vector/vector)
             :ret ::tensor/tensor)
    #(gen/one-of (map gen/return (list vector/kahan-sum identity)))))

;;;;LOOK AT STIELTJES STUFF OR WHATEVER FOR DISCRETIZING A GAUSSIAN FOR MORE 
;;;;THAN 11 OUTCOMES -- KRONROD? vs. GAUSS? vs ...?
;;;;for integration:
;;;;    Write Clenshaw-Curtis and Fejer algos; see Wiki and 
;;;;see http://www.gnu.org/software/gsl/manual/html_node/ for more
;;;;    http://ab-initio.mit.edu/wiki/index.php/Cubature
;;;;    extend to n-dim integration over functions that output vectors or matrices

;;;NUMERICAL INTEGRATION
;;;GAUSSIAN-KRONROD CONSTANTS
;;;can be found here: 
;;;http://www.advanpix.com/2011/11/07/gauss-kronrod-quadrature-nodes-weights/
;;;or more generally, can be calculated here: 
;;;http://keisan.casio.com/exec/system/1289382036
(s/def ::num-intervals
  (s/with-gen
    (s/coll-of ::intervals/num-interval)
    #(gen/vector (s/gen ::intervals/num-interval) 2 2)))

(s/def ::finite-intervals
  (s/with-gen
    (s/coll-of ::intervals/finite-interval)
    #(gen/vector (s/gen ::intervals/finite-interval) 2 2)))

(s/def ::iter-interval
  (s/with-gen
    ::intervals/int+-interval
    #(intervals/long-interval-gen 1 20)))

(s/def ::number->tensor
  (s/with-gen
    (s/fspec :args (s/cat :number ::m/number)
             :ret ::tensor/tensor)
    #(gen/one-of (list (s/gen ::number->number)
                       (gen/return (fn [number]
                                     [number 4.0]))
                       (gen/return (fn [number]
                                     [[number 4.0] [3.0 (inc (double number))]]))))))

(s/def ::number->num-interval
  (s/with-gen
    (s/fspec :args (s/cat :number ::m/number)
             :ret ::intervals/num-interval)
    #(gen/one-of (map gen/return (list (fn [n]
                                         [(dec (double n)) n]))))))

(s/def ::number2->num-interval
  (s/with-gen
    (s/fspec :args (s/cat :number1 ::m/number :number2 ::m/number)
             :ret ::intervals/num-interval)
    #(gen/one-of (map gen/return (list (fn [n1 n2]
                                         [(- (m/abs n1)) (+ 0.5 (m/abs n2))]))))))

(s/def ::number3->num-interval
  (s/with-gen
    (s/fspec :args (s/cat :number1 ::m/number
                          :number2 ::m/number
                          :number3 ::m/number)
             :ret ::intervals/num-interval)
    #(gen/one-of (map gen/return
                      (list (fn [n1 n2 n3]
                              [(- (m/abs (+ (double n1) n2)))
                               (+ 0.5 (m/abs n3))]))))))

(s/def ::points #{15, 21, 31, 41, 51, 61})

(s/def ::weights-and-nodes
  (s/with-gen (s/and (s/tuple ::vector/vector ::vector/vector ::vector/vector)
                     (fn [[a b c]]
                       (and (= (count b) (count c))
                            (= (inc (* 2 (count a))) (count c)))))
              #(gen/bind (s/gen ::points)
                         (fn [points]
                           (gen/return (weights-and-nodes-gk points))))))

(s/def ::error ::tensor/tensor)
(s/def ::high-precision-values ::tensor/tensor)
(s/def ::rectangular-error ::m/number)
(s/def ::one-dimension-errors ::vector/vector)

(defn- convert-gk-weights
  ([number] (convert-gk-weights number true))
  ([number skip?]
   (let [end (if skip? (rest number) number)]
     (-> number rseq (concat end) vec))))

(s/fdef convert-gk-weights
        :args (s/cat :number ::m/number :skip? (s/? boolean?))
        :ret ::vector/vector)

(defn- convert-gk-nodes
  [number]
  (vec (concat (->> number rseq (map -)) (rest number))))

(s/fdef convert-gk-nodes
        :args (s/cat :number ::m/number)
        :ret ::vector/vector)

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
  [(convert-gk-weights gauss7-weights)
   (convert-gk-weights kronrod15-weights)
   (convert-gk-nodes kronrod15-nodes)])

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
  [(convert-gk-weights gauss10-weights false)
   (convert-gk-weights kronrod21-weights)
   (convert-gk-nodes kronrod21-nodes)])

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
  [(convert-gk-weights gauss15-weights)
   (convert-gk-weights kronrod31-weights)
   (convert-gk-nodes kronrod31-nodes)])

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
  [(convert-gk-weights gauss20-weights false)
   (convert-gk-weights kronrod41-weights)
   (convert-gk-nodes kronrod41-nodes)])

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
  [(convert-gk-weights gauss25-weights)
   (convert-gk-weights kronrod51-weights)
   (convert-gk-nodes kronrod51-nodes)])

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

(def ^:private g30-k61
  [(convert-gk-weights gauss30-weights false)
   (convert-gk-weights kronrod61-weights)
   (convert-gk-nodes kronrod61-nodes)])

(defn- adjust-accu-gk
  [accu]
  (-> accu (m/pow (/ 2.0 3.0)) (* 0.005)))

(s/fdef adjust-accu-gk
        :args (s/cat :accu ::m/accu)
        :ret ::m/number)

(defn- weights-and-nodes-gk
  [points]
  (condp = points
    15 g7-k15
    21 g10-k21
    31 g15-k31
    41 g20-k41
    51 g25-k51
    61 g30-k61))

(s/fdef weights-and-nodes-gk
        :args (s/cat :points ::points)
        :ret ::weights-and-nodes)

;ADAPTIVE INTEGRATION
(defn- unnormalize
  "Returns the unnormalized value for a range [a, b] with `value` from
  normalized range [-1, 1]."
  [half-sum half-diff value]
  (-> value double (* half-diff) (+ half-sum)))

(s/fdef unnormalize
        :args (s/cat :half-sum ::m/number
                     :half-diff ::m/number
                     :value ::m/number)
        :ret ::m/number)

(defn- get-error-and-high-precision-values
  "For a single integration approximation, returns a map containing:
   `::error`
   `::high-precision-values`."
  [number->tensor [a b] [low-precision-weights high-precision-weights nodes]]
  (let [half-sum (* 0.5 (+ a b))
        half-diff (* 0.5 (- b a))
        unnormalized-nodes (map (partial unnormalize half-sum half-diff) nodes)
        mapped-nodes (mapv number->tensor unnormalized-nodes)
        high-precision-values (tensor/multiply half-diff
                                               (tensor/inner-product high-precision-weights mapped-nodes))
        low-precision-nodes (tensor/filter-kv (fn [idx _] (odd? idx)) mapped-nodes)
        low-precision-values (tensor/multiply half-diff
                                              (tensor/inner-product low-precision-weights low-precision-nodes))
        error (tensor/emap m/abs
                           (tensor/subtract low-precision-values high-precision-values))]
    {::error                 error
     ::high-precision-values high-precision-values}))

(s/fdef get-error-and-high-precision-values
        :args (s/cat :number->tensor ::number->tensor
                     :finite-interval ::intervals/finite-interval
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/keys :req [::error ::high-precision-values]))

(defn- get-rectangular-errors-and-high-precision-value
  "For a single integration approximation, returns a map containing:
   `::rectangular-error`
   `::high-precision-value`
   `::one-dimension-errors`."
  [v->tensor finite-intervals [low-precision-weights high-precision-weights nodes]]
  (let [half-sums (mapv tensor/average finite-intervals)
        half-diffs (mapv (fn [[a b]]
                           (* 0.5 (- b a)))
                         finite-intervals)
        unnormalized-nodes (for [v (range (count finite-intervals))]
                             (mapv (partial unnormalize (nth half-sums v) (nth half-diffs v))
                                   nodes))
        mapped-nodes (mapv v->tensor
                           (tensor/to-tensor (apply combo/cartesian-product unnormalized-nodes)))
        multiplier (apply * half-diffs)
        dimensions (count finite-intervals)
        f-low-precision #(tensor/inner-product low-precision-weights (vec (take-nth 2 (rest %))))
        f-high-precision #(tensor/inner-product high-precision-weights %)
        f1dim (if (zero? dimensions)
                [[]]
                (mapv (fn [row]
                        (mapv (fn [column]
                                (if (= row column) f-low-precision f-high-precision))
                              (range dimensions)))
                      (range dimensions)))
        partitioned-nodes (tensor/partition-recursively (count high-precision-weights) mapped-nodes)
        reduce-f #(tensor/multiply multiplier (reduce (fn [tot ef]
                                                        (ef tot))
                                                      partitioned-nodes
                                                      %))
        high-precision-values (reduce-f (repeat dimensions f-high-precision))
        low-precision-values (reduce-f (repeat dimensions f-low-precision))
        dim1 (mapv reduce-f f1dim)
        err-f #(tensor/average (tensor/emap m/abs (tensor/subtract % high-precision-values)))]
    {::rectangular-error     (err-f low-precision-values)
     ::high-precision-values high-precision-values
     ::one-dimension-errors  (mapv err-f dim1)}))

(s/fdef get-rectangular-errors-and-high-precision-value
        :args (s/cat :v->tensor ::v->tensor
                     :finite-intervals ::finite-intervals
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/keys :req [::rectangular-error ::high-precision-values ::one-dimension-errors]))

(defn- adaptive-quadrature
  "Integration."
  [number->tensor [a b] accu [min-iter max-iter] weights-and-nodes]
  (let [tolerance (adjust-accu-gk accu)

        {::keys [error high-precision-values]}
        (get-error-and-high-precision-values number->tensor [a b] weights-and-nodes)]
    (loop [iter 1
           error-maps [{:uni-error       error
                        :high-values     high-precision-values
                        :finite-interval [a b]}]]
      (let [total-error (apply + (flatten (map :uni-error error-maps)))]
        (if (and (>= iter min-iter) (not (> total-error tolerance))) ;using 'not' captures NaN
          (if (<= total-error tolerance)
            (apply tensor/add (map :high-values error-maps))
            {::anomalies/message  (str "Error contains NaN. Value: "
                                       (apply tensor/add (map :high-values error-maps)))
             ::anomalies/fn       (var adaptive-quadrature)
             ::anomalies/category ::anomalies/no-solve})
          (if (>= iter max-iter)
            {::anomalies/message  (str "Iteration limit reached. Error: " total-error ". Value: "
                                       (apply tensor/add (map :high-values error-maps)))
             ::anomalies/fn       (var adaptive-quadrature)
             ::anomalies/category ::anomalies/no-solve}
            (let [{[an bn] :finite-interval} (peek error-maps)
                  mn (* 0.5 (+ an bn))

                  [{error1                 ::error
                    high-precision-values1 ::high-precision-values}
                   {error2                 ::error
                    high-precision-values2 ::high-precision-values}]
                  (async/thread :all
                                [#(get-error-and-high-precision-values number->tensor [an mn] weights-and-nodes)
                                 #(get-error-and-high-precision-values number->tensor [mn bn] weights-and-nodes)])

                  new-error-maps [{:uni-error       error1
                                   :high-values     high-precision-values1
                                   :finite-interval [an mn]}
                                  {:uni-error       error2
                                   :high-values     high-precision-values2
                                   :finite-interval [mn bn]}]
                  new-error-maps (into [] (sort-by (fn [m]
                                                     (if (m/nan? (:uni-error m))
                                                       m/inf+
                                                       (:uni-error m)))
                                                   (concat (pop error-maps) new-error-maps)))]
              (recur (inc iter) new-error-maps))))))))

(s/fdef adaptive-quadrature
        :args (s/cat :number->tensor ::number->tensor
                     :finite-interval ::intervals/finite-interval
                     :accu ::m/accu
                     :iter-interval ::iter-interval
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn- simple-splitting-importance-fn
  [rectangular-error one-dimension-errors]
  (if (empty? one-dimension-errors)
    m/inf-
    (apply max one-dimension-errors)))

(s/def ::splitting-importance-fn
  (s/with-gen (s/fspec :args (s/cat :rectangular-error ::rectangular-error
                                    :one-dimension-errors ::one-dimension-errors)
                       :ret ::m/number)
              #(gen/return simple-splitting-importance-fn)))

(s/def simple-splitting-importance-fn ::splitting-importance-fn)

(defn- simple-select-dimensions-fn
  [one-dimension-errors iter-below-min?]
  (let [m (if (empty? one-dimension-errors)
            m/inf-
            (apply max one-dimension-errors))
        ps (map vector
                (range)
                one-dimension-errors)]
    (if iter-below-min?
      (let [dim (ffirst (filter #(= (second %) m)
                                ps))]
        (if dim [dim] []))
      (mapv first
            (filter #(>= (second %) (* m 0.1))
                    ps)))))

(s/def ::select-dimensions-fn
  (s/with-gen (s/fspec :args (s/cat :one-dimension-errors ::one-dimension-errors
                                    :iter-below-min? boolean?)
                       :ret ::vector/vector)
              #(gen/return simple-select-dimensions-fn)))

(s/def simple-select-dimensions-fn ::select-dimensions-fn)

(defn- rectangular-adaptive-quadrature
  "`splitting-importance-fn` takes the multi-dimensional error and the
  one-dimension-errors,and should return a number representing it's importance
  in splitting the integration. `select-dimensions-fn` takes the sequence of
  1-dim errors, and should return a vector of the dimensions to split."
  [v->tensor finite-intervals accu [min-iter max-iter] weights-and-nodes
   splitting-importance-fn select-dimensions-fn]
  (let [tolerance (adjust-accu-gk accu)

        {rectangular-error     ::rectangular-error
         high-precision-values ::high-precision-values
         one-dimension-errors  ::one-dimension-errors}
        (get-rectangular-errors-and-high-precision-value v->tensor finite-intervals weights-and-nodes)]
    (loop [iter 1
           error-maps [{:splitting-importance (splitting-importance-fn rectangular-error one-dimension-errors)
                        :rect-error           rectangular-error
                        :high-values          high-precision-values
                        :one-dim-errors       one-dimension-errors
                        :finite-intervals     finite-intervals}]]
      (let [total-error (apply + (map :rect-error error-maps))]
        (if (and (>= iter min-iter) (not (> total-error tolerance))) ;using 'not' captures NaN
          (if (<= total-error tolerance)
            (apply tensor/add (map :high-values error-maps))
            {::anomalies/message  (str "Error contains NaN. Value: "
                                       (apply tensor/add (map :high-values error-maps)))
             ::anomalies/fn       (var rectangular-adaptive-quadrature)
             ::anomalies/category ::anomalies/no-solve})
          (if (>= iter max-iter)
            {::anomalies/message  (str "Iteration limit reached.  Error: " total-error " Value: "
                                       (apply tensor/add (map :high-values error-maps)))
             ::anomalies/fn       (var rectangular-adaptive-quadrature)
             ::anomalies/category ::anomalies/no-solve}
            (let [{intervals-n    :finite-intervals
                   one-dim-errors :one-dim-errors} (peek error-maps)
                  dimensions (select-dimensions-fn one-dim-errors (< iter min-iter))
                  dims-with-new-intervals (map #(let [[an bn] (nth intervals-n %)
                                                      mn (* 0.5 (+ an bn))]
                                                  [[% an mn] [% mn bn]])
                                               dimensions)
                  new-intervals (map #(reduce (fn [tot [d an bn]]
                                                (assoc tot d [an bn]))
                                              intervals-n
                                              %)
                                     (apply combo/cartesian-product dims-with-new-intervals))
                  new-fns (map (fn [intervals-local]
                                 #(get-rectangular-errors-and-high-precision-value
                                    v->tensor intervals-local weights-and-nodes))
                               new-intervals)
                  new-error-maps (mapv (fn [finite-intervals
                                            {rect-error     ::rectangular-error
                                             one-dim-errors ::one-dimension-errors
                                             high-values    ::high-precision-values}]
                                         {:splitting-importance (splitting-importance-fn rect-error one-dim-errors)
                                          :rect-error           rect-error
                                          :high-values          high-values
                                          :one-dim-errors       one-dim-errors
                                          :finite-intervals     finite-intervals})
                                       new-intervals
                                       (async/thread :all new-fns))
                  new-error-maps (into [] (sort-by (fn [m]
                                                     (if (m/nan? (:splitting-importance m))
                                                       m/inf+
                                                       (:splitting-importance m)))
                                                   (concat (pop error-maps) new-error-maps)))]
              (recur (inc iter) new-error-maps))))))))

(s/fdef rectangular-adaptive-quadrature
        :args (s/cat :v->tensor ::v->tensor
                     :finite-intervals ::finite-intervals
                     :accu ::m/accu
                     :iter-interval ::iter-interval
                     :weights-and-nodes ::weights-and-nodes
                     :splitting-importance-fn ::splitting-importance-fn
                     :select-dimensions-fn ::select-dimensions-fn)
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn change-of-variable
  "Takes the num-interval and returns a map containing:
    `::multiplicative-fn`
    `::converter-fn` -- for within the function to integrate.
    `::bo/finite-interval`."
  [[a b]]
  (cond
    (and (m/inf-? a) (m/inf+? b)) {::multiplicative-fn         (fn [number]
                                                                 (let [s (m/sq number)]
                                                                   (m/div (inc s) (m/sq (m/one- s)))))
                                   ::converter-fn              (fn [number]
                                                                 (m/div number (m/one- (m/sq number))))
                                   ::intervals/finite-interval [-1.0 1.0]}
    (m/inf+? b) {::multiplicative-fn         (fn [number]
                                               (m/div (m/sq number)))
                 ::converter-fn              (fn [number]
                                               (+ a (m/div (m/one- number) number)))
                 ::intervals/finite-interval [0.0 1.0]}
    (m/inf-? a) {::multiplicative-fn         (fn [number]
                                               (m/div (m/sq number)))
                 ::converter-fn              (fn [number]
                                               (- b (m/div (m/one- number) number)))
                 ::intervals/finite-interval [0.0 1.0]}
    :else {::multiplicative-fn         (constantly 1.0)
           ::converter-fn              identity
           ::intervals/finite-interval [a b]}))

(s/def ::multiplicative-fn ::number->number)
(s/def ::converter-fn ::number->number)

(s/fdef change-of-variable
        :args (s/cat :num-interval ::intervals/num-interval)
        :ret (s/keys :req [::multiplicative-fn ::converter-fn ::intervals/finite-interval]))

(defn- change-of-variable-for-integration
  "Returns the new function and new finite-interval as a tuple."
  [number->tensor [a b]]
  (let [{:keys [::multiplicative-fn ::converter-fn ::intervals/finite-interval]} (change-of-variable [a b])]
    [#(tensor/multiply (multiplicative-fn %) (number->tensor (converter-fn %)))
     finite-interval]))

(s/fdef change-of-variable-for-integration
        :args (s/cat :number->tensor ::number->tensor
                     :num-interval ::intervals/num-interval)
        :ret (s/tuple ::number->tensor ::intervals/finite-interval))

(defn- change-of-variable-for-rectangular-integration
  "Returns the new function and new finite-intervals."
  [v->tensor num-intervals]
  (let [maps (map change-of-variable num-intervals)
        fms (map ::multiplicative-fn maps)
        fvs (map ::converter-fn maps)
        new-intervals (mapv ::intervals/finite-interval maps)]
    [#(tensor/multiply (apply * (map (fn [f a]
                                       (f a))
                                     fms
                                     %))
                       (v->tensor (mapv (fn [f a]
                                          (f a))
                                        fvs
                                        %)))
     new-intervals]))

(s/fdef change-of-variable-for-rectangular-integration
        :args (s/cat :v->tensor ::v->tensor
                     :num-intervals ::num-intervals)
        :ret (s/tuple ::v->tensor ::finite-intervals))

(defn integration
  "Returns the integral of a function `number->tensor` over the num-interval
  from `a` to `b` using global adaptive integration of the Gauss-Kronrod
  Quadrature Formula.
  Options:
    `::points` -- 15, 21, 31, 41, 51, 61; default is 21 or 15 if num-interval is
      infinite
    `::m/accu` -- default is m/*dbl-close* (accuracy is also adjusted for
      Gauss-Konrod quadrature type)
    `::iter-interval` -- default is [10 1000].

  Known Limitations:
    Very large absolute ranges (not infinite) can cause approximation errors due
      to limited rounded accuracy of the 'double' type.
    Example Problem: exp(-x^2) from -166 (or more negative) to Inf+ returns zero
      instead of sqrt of PI with default `::iter-interval` and 51 `::points`.
    Solution: use a change of variable, or increase minimum of
      `::iter-interval`, or increase `::points`."
  ([number->tensor [a b]] (integration number->tensor [a b] {}))
  ([number->tensor [a b] {:keys [::points ::m/accu ::iter-interval]
                          :or   {accu m/dbl-close, iter-interval [10 1000]}}]
   (let [points (cond points points
                      (some m/inf? [a b]) 15
                      :else 21)
         [new-fn new-interval] (change-of-variable-for-integration number->tensor [a b])]
     (adaptive-quadrature new-fn new-interval accu iter-interval (weights-and-nodes-gk points)))))

(s/fdef integration
        :args (s/cat :number->tensor ::number->tensor
                     :num-interval ::intervals/num-interval
                     :opts (s/? (s/keys :opt [::points ::m/accu ::iter-interval])))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn rectangular-integration
  "Returns the integral of a function `v->tensor` over the rectangular
  `num-intervals` using global adaptive integration of the Gauss-Kronrod
  Quadrature Formula. `v->tensor` takes a vector with one element for each
  num-interval and returns a tensor.
  Options:
    `::points` -- 15, 21, 31, 41, 51, 61; default is 15
    `::m/accu` -- default is m/*dbl-close* and (m/*sgl-close* for 4+ variables
      or for 3+ variables if any num-interval is infinite) (accuracy is also
      adjusted for for Gauss-Konrod quadrature type)
    `::iter-interval` -- default is [10 1000].

  Known Limitations:
    With more than 4 dimensions, use Monte Carlo simulation instead for speed.
    Very large absolute ranges (not infinite) can cause approximation errors due
      to limited rounded accuracy of the 'double' type."
  ([v->tensor num-intervals] (rectangular-integration v->tensor num-intervals {}))
  ([v->tensor num-intervals {:keys [::points ::m/accu ::iter-interval]
                             :or   {points        15
                                    iter-interval [10 1000]}}]
   (let [dimensions (count num-intervals)
         accu (cond accu accu
                    (or (and (some m/inf? (flatten num-intervals)) (>= dimensions 3)) (>= dimensions 4)) m/sgl-close
                    :else m/dbl-close)
         [new-fn new-intervals] (change-of-variable-for-rectangular-integration v->tensor num-intervals)
         weights-and-nodes (weights-and-nodes-gk points)]
     (rectangular-adaptive-quadrature
       new-fn new-intervals accu iter-interval weights-and-nodes simple-splitting-importance-fn
       simple-select-dimensions-fn))))

(s/fdef rectangular-integration
        :args (s/cat :v->tensor ::v->tensor
                     :num-intervals ::num-intervals
                     :opts (s/? (s/keys :opt [::points ::m/accu ::iter-interval])))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn non-rectangular-2D-integration
  "Returns the integral of a function `number2->tensor` over the outer and inner
  intervals using global adaptive integration of the Gauss-Kronrod Quadrature
  Formula. Use [[rectangular-integration]] for rectangular integration with
  numbers because that function is faster. `number2->tensor` should be a
  function of (outer, inner). `outer-interval` is [a b]. `outer->inner-interval`
  takes the outer number and returns an interval [c d].
  Options:
    `::points` -- 15, 21, 31, 41, 51, 61; default is 15
    `::m/accu` -- default is m/*dbl-close* per dimension (accuracy is adjusted
      for quadrature type)
    `::iter-interval` -- default per dimension is [10 1000]."
  ([number2->tensor outer-interval outer->inner-interval]
   (non-rectangular-2D-integration number2->tensor outer-interval outer->inner-interval {}))
  ([number2->tensor outer-interval outer->inner-interval
    {:keys [::points ::m/accu ::iter-interval]
     :or   {points        15
            accu          m/dbl-close
            iter-interval [10 1000]}
     :as   props}]
   (integration
     (fn [outer]
       (let [ret (integration
                   (fn [inner] (number2->tensor outer inner))
                   (outer->inner-interval outer)
                   props)]
         (if (tensor/tensor? ret) ret m/nan)))
     outer-interval
     props)))

(s/def ::number2->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::m/number :inner ::m/number)
             :ret ::tensor/tensor)
    #(gen/one-of (map gen/return (list (fn [outer inner]
                                         [[outer 0.0] [2.0 inner]]))))))

(s/fdef non-rectangular-2D-integration
        :args (s/cat :number2->tensor ::number2->tensor
                     :outer-interval ::intervals/num-interval
                     :outer->inner-interval ::number->num-interval
                     :opts (s/? (s/keys :opt [::points ::m/accu ::iter-interval])))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn non-rectangular-3D-integration
  "Returns the integral of a function `number3->tensor` over the outer, middle,
  and inner intervals using global adaptive integration of the Gauss-Kronrod
  Quadrature Formula. Use [[rectangular-integration]] for rectangular
  integration with numbers because that function is faster. `number3->tensor`
  should be a function of (outer, middle, inner). `outer-interval` is [a b].
  `outer->middle-interval` takes the outer number and returns an interval [c d].
  `outer+middle->inner-interval` takes the outer and middle number and returns
  an interval [e f]. Options:
    `::points` -- 15, 21, 31, 41, 51, 61; default is 15
    `::m/accu` -- default is m/*sgl-close* per dimension (accuracy is adjusted
      for quadrature type)
    `::iter-interval` -- default per dimension is [3 300]."
  ([number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval]
   (non-rectangular-3D-integration
     number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval {}))
  ([number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval
    {:keys [::points ::m/accu ::iter-interval]
     :or   {points        15
            accu          m/sgl-close
            iter-interval [3 300]}
     :as   props}]
   (integration
     (fn [outer]
       (let [ret-middle
             (integration
               (fn [middle]
                 (let [ret-inner (integration
                                   (fn [inner]
                                     (number3->tensor outer middle inner))
                                   (outer+middle->inner-interval outer middle) props)]
                   (if (tensor/tensor? ret-inner) ret-inner m/nan)))
               (outer->middle-interval outer)
               props)]
         (if (tensor/tensor? ret-middle) ret-middle m/nan)))
     outer-interval
     props)))

(s/def ::number3->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::m/number
                          :middle ::m/number
                          :inner ::m/number)
             :ret ::tensor/tensor)
    #(gen/one-of (map gen/return (list (fn [outer middle inner]
                                         [[outer middle] [2.0 inner]]))))))

(s/fdef non-rectangular-3D-integration
        :args (s/cat :number3->tensor ::number3->tensor
                     :outer-interval ::intervals/num-interval
                     :outer->middle-interval ::number->num-interval
                     :outer+middle->inner-interval ::number2->num-interval
                     :opts (s/? (s/keys :opt [::points ::m/accu ::iter-interval])))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))

(defn non-rectangular-4D-integration
  "Returns the integral of a function `number4->tensor` over the outer,
  outer-middle, outer-inner, and inner intervals using global adaptive
  integration of the Gauss-Kronrod Quadrature Formula. Use
  [[rectangular-integration]] for rectangular integration with numbers because
  that function is faster. `number4->tensor` should be a function of (outer,
  outer-middle, inner-middle, inner). `outer-interval` is [a b].
  `outer->outer-middle-interval` takes the outer number and returns an interval
  [c d]. `outer+outer-middle->inner-middle-interval` takes the outer and
  outer-middle number and returns an interval [e f].
  `outer+outer-middle+inner-middle->inner-interval` takes the outer,
  outer-middle, and inner-middle numbers and returns and interval [g h].
  Options:
    `::points` -- 15, 21, 31, 41, 51, 61; default is 15
    `::m/accu` -- default is m/*sgl-close* per dimension (accuracy is adjusted
      for quadrature type)
    `::iter-interval` -- default per dimension is [2 200]."
  ([number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
    outer+outer-middle+inner-middle->inner-interval]
   (non-rectangular-4D-integration
     number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
     outer+outer-middle+inner-middle->inner-interval {}))
  ([number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
    outer+outer-middle+inner-middle->inner-interval
    {:keys [::points ::m/accu ::iter-interval]
     :or   {points        15
            accu          m/sgl-close
            iter-interval [2 200]}
     :as   props}]
   (integration
     (fn [outer]
       (let [ret-outer-middle
             (integration
               (fn [outer-middle]
                 (let [ret-inner-middle
                       (integration
                         (fn [inner-middle]
                           (let [ret-inner
                                 (integration
                                   (fn [inner]
                                     (number4->tensor outer outer-middle inner-middle inner))
                                   (outer+outer-middle+inner-middle->inner-interval outer outer-middle inner-middle)
                                   props)]
                             (if (tensor/tensor? ret-inner) ret-inner m/nan)))
                         (outer+outer-middle->inner-middle-interval outer outer-middle)
                         props)]
                   (if (tensor/tensor? ret-inner-middle) ret-inner-middle m/nan)))
               (outer->outer-middle-interval outer)
               props)]
         (if (tensor/tensor? ret-outer-middle) ret-outer-middle m/nan)))
     outer-interval
     props)))

(s/def ::number4->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::m/number
                          :outer-middle ::m/number
                          :inner-middle ::m/number
                          :inner ::m/number)
             :ret ::tensor/tensor)
    #(gen/one-of (map gen/return (list (fn [outer outer-middle inner-middle inner]
                                         [[outer outer-middle] [inner-middle inner]]))))))

(s/fdef non-rectangular-4D-integration
        :args (s/cat :number4->tensor ::number4->tensor
                     :outer-interval ::intervals/num-interval
                     :outer->outer-middle-interval ::number->num-interval
                     :outer+outer-middle->inner-middle-interval ::number2->num-interval
                     :outer+outer-middle+inner-middle->inner-interval ::number3->num-interval
                     :opts (s/? (s/keys :opt [::points ::m/accu ::iter-interval])))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :tensor ::tensor/tensor))