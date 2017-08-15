(ns provisdom.math.calculus
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.utility-belt.core :as co]
            [provisdom.utility-belt.async :as as]
            [provisdom.math.core :as m]
            [provisdom.math.bounds :as bo]
            [provisdom.math.combinatorics :as mc]
            [provisdom.math.tensor :as tensor]
            [provisdom.math.vector :as vector]
            [provisdom.math.matrix :as mx]))

(set! *warn-on-reflection* true)

(declare weights-and-nodes-gk)

(s/def ::number ::m/number)
(s/def ::vector ::vector/vector)
(s/def ::number->number (s/with-gen
                          (s/fspec :args (s/cat :number ::number) :ret ::number)
                          #(gen/one-of (map gen/return (list m/sq m/cube m/cos)))))
(s/def ::v->number (s/with-gen
                     (s/fspec :args (s/cat :v ::vector) :ret ::number)
                     #(gen/one-of (map gen/return (list vector/kahan-sum tensor/average tensor/norm1)))))

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
(s/def ::interval ::bo/interval)
(s/def ::intervals (s/coll-of ::interval))
(s/def ::tensor ::tensor/tensor)
(s/def ::accu ::mx/accu)
(s/def ::iter-interval (s/with-gen ::bo/int+-interval #(bo/long-interval-gen 1 100)))
(s/def ::number->tensor (s/with-gen (s/fspec :args (s/cat :number ::number) :ret ::tensor)
                                    (gen/one-of (list (s/gen ::number->number)
                                                      (gen/return (fn [number] [number 4.0]))
                                                      (gen/return (fn [number] [[number 4.0] [3.0 (inc number)]]))))))
(s/def ::number->interval
  (s/with-gen
    (s/fspec :args (s/cat :number ::number) :ret ::interval)
    #(gen/one-of (map gen/return (list (fn [n] [(dec n) n]))))))
(s/def ::number2->interval
  (s/with-gen
    (s/fspec :args (s/cat :number1 ::number :number2 ::number) :ret ::interval)
    #(gen/one-of (map gen/return (list (fn [n1 n2] [(- (m/abs n1)) (+ 0.5 (m/abs n2))]))))))
(s/def ::number3->interval
  (s/with-gen
    (s/fspec :args (s/cat :number1 ::number :number2 ::number :number3 ::number) :ret ::interval)
    #(gen/one-of (map gen/return (list (fn [n1 n2 n3] [(- (m/abs (+ n1 n2))) (+ 0.5 (m/abs n3))]))))))
(s/def ::points #{15, 21, 31, 41, 51, 61})
(s/def ::weights-and-nodes
  (s/with-gen (s/and (s/tuple ::vector ::vector ::vector)
                     (fn [[a b c]] (and (= (count b) (count c)) (= (inc (* 2 (count a))) (count c)))))
              #(gen/bind (s/gen ::points) (fn [points] (gen/return (weights-and-nodes-gk points))))))
(s/def ::uni-dimensional-error ::tensor)
(s/def ::high-precision-values ::tensor)
(s/def ::multi-dimensional-error ::number)
(s/def ::high-precision-value ::number)
(s/def ::one-dimension-errors ::vector)
(s/def ::exception (partial instance? Exception))

(defn- convert-gk-weights
  ([number] (convert-gk-weights number true))
  ([number skip?] (let [end (if skip? (rest number) number)] (-> number rseq (concat end) vec))))

(s/fdef convert-gk-weights
        :args (s/cat :number ::number :skip? (s/? boolean?))
        :ret ::vector)

(defn- convert-gk-nodes [number] (vec (concat (->> number rseq (map -)) (rest number))))

(s/fdef convert-gk-nodes
        :args (s/cat :number ::number)
        :ret ::vector)

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
  [(convert-gk-weights gauss7-weights) (convert-gk-weights kronrod15-weights) (convert-gk-nodes kronrod15-nodes)])

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
  [(convert-gk-weights gauss10-weights false) (convert-gk-weights kronrod21-weights) (convert-gk-nodes kronrod21-nodes)])

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
  [(convert-gk-weights gauss15-weights) (convert-gk-weights kronrod31-weights) (convert-gk-nodes kronrod31-nodes)])

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
  [(convert-gk-weights gauss20-weights false) (convert-gk-weights kronrod41-weights) (convert-gk-nodes kronrod41-nodes)])

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
  [(convert-gk-weights gauss25-weights) (convert-gk-weights kronrod51-weights) (convert-gk-nodes kronrod51-nodes)])

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

(defn- adjust-accu-gk [accu] (-> accu (m/pow (/ 2.0 3.0)) (* 0.005)))

(s/fdef adjust-accu-gk
        :args (s/cat :accu ::accu)
        :ret ::number)

(defn- weights-and-nodes-gk
  [points] (condp = points 15 g7-k15, 21 g10-k21, 31 g15-k31, 41 g20-k41, 51 g25-k51, 61 g30-k61))

(s/fdef weights-and-nodes-gk
        :args (s/cat :points ::points)
        :ret ::weights-and-nodes)

;ADAPTIVE INTEGRATION
(defn- unnormalize
  "Returns the unnormalized value for a range [a, b] with `value` from normalized range [-1, 1]."
  [half-sum half-diff value] (-> value double (* half-diff) (+ half-sum)))

(s/fdef unnormalize
        :args (s/cat :half-sum ::number :half-diff ::number :value ::number)
        :ret ::number)

(defn- get-uni-dimensional-error-and-high-precision-values
  "For a single integration approximation, returns a map containing:
   `::univariate-error`
   `::high-precision-values`."
  [number->tensor [a b] [low-precision-weights high-precision-weights nodes]]
  (let [half-sum (* 0.5 (+ a b))
        half-diff (* 0.5 (- b a))
        unnormalized-nodes (map (partial unnormalize half-sum half-diff) nodes)
        mapped-nodes (mapv number->tensor unnormalized-nodes)
        high-precision-values (tensor/multiply half-diff (tensor/inner-product high-precision-weights mapped-nodes))
        low-precision-nodes (tensor/filter-kv (fn [idx _] (odd? idx)) mapped-nodes)
        low-precision-values (tensor/multiply half-diff
                                              (tensor/inner-product low-precision-weights low-precision-nodes))
        univariate-error (tensor/emap m/abs (tensor/subtract low-precision-values high-precision-values))]
    {::uni-dimensional-error univariate-error ::high-precision-values high-precision-values}))

(s/fdef get-uni-dimensional-error-and-high-precision-values
        :args (s/cat :number->tensor ::number->tensor
                     :interval ::interval
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/keys :req [::uni-dimensional-error ::high-precision-values]))

(defn- get-multi-dimensional-errors-and-high-precision-value
  "For a single integration approximation, returns a map containing:
   `::multivariate-error`
   `::high-precision-value`
   `::one-dimension-errors`."
  [v->number intervals [low-precision-weights high-precision-weights nodes]]
  (let [half-sum (mapv tensor/average intervals)
        half-diff (mapv #(* 0.5 (- (second %) (first %))) intervals)
        unnormalized-nodes (for [v (range (count intervals))]
                             (mapv (partial unnormalize (nth half-sum v) (nth half-diff v)) nodes))
        mapped-nodes (mapv v->number (apply mc/cartesian-product unnormalized-nodes))
        multiplier (apply * half-diff)
        dimensions (count intervals)
        f-low-precision #(tensor/inner-product low-precision-weights (take-nth 2 (rest %)))
        f-high-precision #(tensor/inner-product high-precision-weights %)
        f1dim (co/create-dbl-layered dimensions dimensions (fn [i j] (if (= i j) f-low-precision f-high-precision)))
        partitioned-nodes (tensor/to-tensor (co/partition-recursively mapped-nodes (count high-precision-weights)))
        reduce-f #(tensor/multiply multiplier (reduce (fn [tot ef] (ef tot)) partitioned-nodes %))
        high-precision-value (reduce-f (repeat dimensions f-high-precision))
        low-precision-value (reduce-f (repeat dimensions f-low-precision))
        dim1 (map reduce-f f1dim)
        err-f #(tensor/average (tensor/emap m/abs (tensor/subtract % high-precision-value)))]
    {::multi-dimensional-error (err-f low-precision-value)
     ::high-precision-value    high-precision-value
     ::one-dimension-errors    (mapv err-f dim1)}))

(s/fdef get-multi-dimensional-errors-and-high-precision-value
        :args (s/cat :v->number ::v->number
                     :intervals ::intervals
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/keys :req [::multi-dimensional-error ::high-precision-value ::one-dimension-errors]))

(defn- uni-dimensional-adaptive-quadrature
  "Uni-dimensional Integration."
  [number->tensor [a b] accu [min-iter max-iter] weights-and-nodes]
  (let [tolerance (adjust-accu-gk accu)

        {::keys [uni-dimensional-error high-precision-values]}
        (get-uni-dimensional-error-and-high-precision-values number->tensor [a b] weights-and-nodes)]
    (loop [iter 1, error-maps [{:uni-error uni-dimensional-error :high-values high-precision-values :interval [a b]}]]
      (let [total-error (apply + (flatten (map :uni-error error-maps)))]
        (if (and (>= iter min-iter) (<= total-error tolerance))
          (apply tensor/add (map :high-values error-maps))
          (if (>= iter max-iter)
            (ex-info (str "Iteration limit reached.  Error: " total-error " Value: "
                          (apply tensor/add (map :high-values error-maps)))
                     {:fn (var uni-dimensional-adaptive-quadrature)})
            (let [{[an bn] :interval} (peek error-maps)
                  mn (* 0.5 (+ an bn))

                  [{error1 ::uni-dimensional-error, high-precision-values1 ::high-precision-values}
                   {error2 ::uni-dimensional-error, high-precision-values2 ::high-precision-values}]
                  (as/thread :all
                             [#(get-uni-dimensional-error-and-high-precision-values
                                 number->tensor [an mn] weights-and-nodes)
                              #(get-uni-dimensional-error-and-high-precision-values
                                 number->tensor [mn bn] weights-and-nodes)])

                  new-error-maps [{:uni-error error1 :high-values high-precision-values1 :interval [an mn]}
                                  {:uni-error error2 :high-values high-precision-values2 :interval [mn bn]}]
                  new-error-maps (into [] (sort-by :uni-error (concat (pop error-maps) new-error-maps)))]
              (recur (inc iter) new-error-maps))))))))

(s/fdef uni-dimensional-adaptive-quadrature
        :args (s/cat :number->tensor ::number->tensor
                     :interval ::interval
                     :accu ::accu
                     :iter-interval ::iter-interval
                     :weights-and-nodes ::weights-and-nodes)
        :ret (s/or :exception ::exception :tensor ::tensor))

(s/def ::splitting-importance-fn
  (s/fspec :args (s/cat :multi-dimensional-error ::multi-dimensional-error
                        :one-dimension-errors ::one-dimension-errors)
           :ret ::number))

(defn- simple-splitting-importance-fn
  [multi-dimensional-error one-dimension-errors]
  (if (empty? one-dimension-errors) m/inf- (apply max one-dimension-errors)))

(s/def simple-splitting-importance-fn ::splitting-importance-fn)

(s/def ::select-dimensions-fn
  (s/fspec :args (s/cat :one-dimension-errors ::one-dimension-errors :iter-below-min? boolean?) :ret ::vector))

(defn- simple-select-dimensions-fn [one-dimension-errors iter-below-min?]
  (let [m (if (empty? one-dimension-errors) m/inf- (apply max one-dimension-errors))
        ps (map vector (range) one-dimension-errors)]
    (if iter-below-min?
      [(ffirst (filter #(= (second %) m) ps))]
      (mapv first (filter #(>= (second %) (* m 0.1)) ps)))))

(s/def simple-select-dimensions-fn ::select-dimensions-fn)

(defn- multi-dimensional-adaptive-quadrature
  "`splitting-importance-fn` takes the multi-dimensional error and the one-dimension-errors,
      and should return a number representing it's importance in splitting the integration.
   `select-dimensions-fn` takes the sequence of 1-dim errors, and should return a vector of the dimensions to split."
  [v->number intervals accu [min-iter max-iter] weights-and-nodes splitting-importance-fn select-dimensions-fn]
  (let [tolerance (adjust-accu-gk accu)

        {multi-dimensional-error ::multi-dimensional-error
         high-precision-value    ::high-precision-value
         one-dimension-errors    ::one-dimension-errors}
        (get-multi-dimensional-errors-and-high-precision-value v->number intervals weights-and-nodes)]
    (loop [iter 1,
           error-maps [{:splitting-importance (splitting-importance-fn multi-dimensional-error one-dimension-errors)
                        :multi-error          multi-dimensional-error
                        :high-value           high-precision-value
                        :one-dim-errors       one-dimension-errors
                        :intervals            intervals}]]
      (let [total-error (apply + (map :multi-error error-maps))]
        (if (and (>= iter min-iter) (<= total-error tolerance))
          (apply tensor/add (map :high-value error-maps))
          (if (>= iter max-iter)
            (ex-info (str "Iteration limit reached.  Error: " total-error " Value: "
                          (apply tensor/add (map :high-value error-maps)))
                     {:fn (var multi-dimensional-adaptive-quadrature)})
            (let [{intervals-n :intervals one-dim-errors :one-dim-errors} (peek error-maps),
                  dimensions (select-dimensions-fn one-dim-errors (< iter min-iter)),
                  dims-with-new-intervals (map #(let [[an bn] (nth intervals-n %)
                                                      mn (* 0.5 (+ an bn))]
                                                  [[% an mn] [% mn bn]])
                                               dimensions)
                  new-intervals (map #(reduce (fn [tot [d an bn]] (assoc tot d [an bn])) intervals-n %)
                                     (apply mc/cartesian-product dims-with-new-intervals))
                  new-fns (map (fn [intervals-local] #(get-multi-dimensional-errors-and-high-precision-value
                                                        v->number intervals-local weights-and-nodes))
                               new-intervals)
                  new-error-maps (mapv (fn [intervals
                                            {multi-error    ::multi-dimensional-error
                                             one-dim-errors ::one-dimension-errors
                                             high-value     ::high-precision-value}]
                                         {:splitting-importance (splitting-importance-fn multi-error one-dim-errors)
                                          :multi-error          multi-error
                                          :high-value           high-value
                                          :one-dim-errors       one-dim-errors
                                          :intervals            intervals})
                                       new-intervals
                                       (as/thread :all new-fns))
                  new-error-maps (into [] (sort-by :splitting-importance (concat (pop error-maps) new-error-maps)))]
              (recur (inc iter) new-error-maps))))))))

(s/fdef multi-dimensional-adaptive-quadrature
        :args (s/cat :v->number ::v->number
                     :intervals ::intervals
                     :accu ::accu
                     :iter-interval ::iter-interval
                     :weights-and-nodes ::weights-and-nodes
                     :splitting-importance-fn ::splitting-importance-fn
                     :select-dimensions-fn ::select-dimensions-fn)
        :ret (s/or :exception ::exception :number ::number))

(defn change-of-variable
  "Takes the interval and returns a map containing:
  ::multiplicative-fn
  ::converter-fn -- for within the function to integrate.
  ::interval."
  [[a b]]
  (cond
    (and (m/inf-? a) (m/inf+? b)) {::multiplicative-fn (fn [number]
                                                         (let [s (m/sq number)] (m/div (inc s) (m/sq (m/one- s)))))
                                   ::converter-fn      (fn [number] (m/div number (m/one- (m/sq number))))
                                   ::interval          [-1.0 1.0]}
    (m/inf+? b) {::multiplicative-fn (fn [number] (m/div (m/sq number)))
                 ::converter-fn      (fn [number] (+ a (m/div (m/one- number) number)))
                 ::interval          [0.0 1.0]}
    (m/inf-? a) {::multiplicative-fn (fn [number] (m/div (m/sq number)))
                 ::converter-fn      (fn [number] (- b (m/div (m/one- number) number)))
                 ::interval          [0.0 1.0]}
    :else {::multiplicative-fn (constantly 1.0)
           ::converter-fn      identity
           ::interval          [a b]}))

(s/def ::multiplicative-fn ::number->number)
(s/def ::converter-fn ::number->number)
(s/fdef change-of-variable
        :args (s/cat :interval ::interval)
        :ret (s/keys :req [::multiplicative-fn ::converter-fn ::interval]))

(defn- change-of-variable-for-uni-dimensional-integration
  "Returns the new function and new interval as a tuple."
  [number->tensor [a b]]
  (let [{::keys [multiplicative-fn converter-fn interval]} (change-of-variable [a b])]
    [#(tensor/multiply (multiplicative-fn %) (number->tensor (converter-fn %))) interval]))

(s/fdef change-of-variable-for-uni-dimensional-integration
        :args (s/cat :number->tensor ::number->tensor :interval ::interval)
        :ret (s/tuple ::number->tensor ::interval))

(defn- change-of-variable-for-multi-dimensional-integration
  "Returns the new function and new intervals."
  [v->number intervals]
  (let [maps (map change-of-variable intervals)
        fms (map ::multiplicative-fn maps)
        fvs (map ::converter-fn maps)
        new-intervals (mapv ::interval maps)]
    [#(tensor/multiply (apply * (co/in-place-functional fms %)) (v->number (vec (co/in-place-functional fvs %))))
     new-intervals]))

(s/fdef change-of-variable-for-multi-dimensional-integration
        :args (s/cat :v->number ::v->number :intervals ::intervals)
        :ret (s/tuple ::v->number ::intervals))

(defn integration
  "Returns the integral of a function `number->tensor` over the interval from a to b using
  global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Options:
       `::points` -- 15, 21, 31, 41, 51, 61; default is 21 or 15 if interval is infinite
       `::accu` -- default is m/*dbl-close* (accuracy is also adjusted for Gauss-Konrod quadrature type)
       `::iter-interval` -- default is [10 1000].

   Known Limitations:
       Very large absolute ranges (not infinite) can cause approximation errors due to limited rounded accuracy
          of the 'double' type.
       Example Problem: exp(-x^2) from -166 (or more negative) to inf+ returns
          zero instead of sqrt of pi with default `::iter-interval` and 51 `::points`.
       Solution: use a change of variable, or increase minimum of `::iter-interval`, or increase `::points`."
  ([number->tensor [a b]] (integration number->tensor [a b] {}))
  ([number->tensor [a b] {::keys [points accu iter-interval]
                          :or    {accu m/*dbl-close*, iter-interval [10 1000]}}]
   (let [points (cond points points, (some m/inf? [a b]) 15, :else 21)
         [new-fn new-interval] (change-of-variable-for-uni-dimensional-integration number->tensor [a b])]
     (uni-dimensional-adaptive-quadrature new-fn new-interval accu iter-interval (weights-and-nodes-gk points)))))

(s/fdef integration
        :args (s/cat :number->tensor ::number->tensor
                     :interval ::interval
                     :opts (s/? (s/keys :opt [::points ::accu ::iter-interval])))
        :ret (s/or :exception ::exception :tensor ::tensor))

(defn rectangular-integration
  "Returns the integral of a function `v->number` over the rectangular `intervals` using global adaptive integration of
  the Gauss-Kronrod Quadrature Formula.
   `v->number` takes a vector with one element for each interval and returns a number.
   Options:
       `::points` -- 15, 21, 31, 41, 51, 61; default is 15
       `::accu` -- default is m/*dbl-close* and
          (m/*sgl-close* for 4+ variables or for 3+ variables if any interval is infinite)
          (accuracy is also adjusted for for Gauss-Konrod quadrature type)
       `::iter-interval` -- default is [10 1000].

   Known Limitations:
       With more than 4 dimensions, use Monte Carlo simulation instead for speed.
       Very large absolute ranges (not infinite) can cause approximation errors due to limited rounded accuracy
          of the 'double' type."
  ([v->number intervals] (rectangular-integration v->number intervals {}))
  ([v->number intervals {::keys [points accu iter-interval]
                         :or    {points 15, iter-interval [10 1000]}}]
   (let [dimensions (count intervals)
         accu (cond accu accu
                    (or (and (some m/inf? (flatten intervals)) (>= dimensions 3)) (>= dimensions 4)) m/*sgl-close*
                    :else m/*dbl-close*)
         [new-fn new-intervals] (change-of-variable-for-multi-dimensional-integration v->number intervals)
         weights-and-nodes (weights-and-nodes-gk points)]
     (multi-dimensional-adaptive-quadrature
       new-fn new-intervals accu iter-interval weights-and-nodes simple-splitting-importance-fn
       simple-select-dimensions-fn))))

(s/fdef rectangular-integration
        :args (s/cat :v->number ::v->number
                     :intervals ::intervals
                     :opts (s/? (s/keys :opt [::points ::accu ::iter-interval])))
        :ret (s/or :exception ::exception :number ::number))


(defn non-rectangular-2D-integration
  "Returns the integral of a function `number2->tensor` over the outer and inner intervals using global adaptive
  integration of the Gauss-Kronrod Quadrature Formula.
  Use [[rectangular-integration]] for rectangular integration with numbers because that function is faster.
  `number2->tensor` should be a function of (outer, inner).
  'outer-interval' is [a b]
  'inner-interval-fn' takes the outer number and returns an interval [c d]
   Options:
       `::points` -- 15, 21, 31, 41, 51, 61; default is 15
       `::accu` -- default is m/*dbl-close* per dimension (accuracy is adjusted for quadrature type)
       `::iter-interval` -- default per dimension is [10 1000]."
  ([number2->tensor outer-interval outer->inner-interval]
   (non-rectangular-2D-integration number2->tensor outer-interval outer->inner-interval {}))
  ([number2->tensor outer-interval outer->inner-interval
    {::keys [points accu iter-interval]
     :or    {points 15, accu m/*dbl-close*, iter-interval [10 1000]}
     :as    props}]
   (integration #(integration (partial number2->tensor %) (outer->inner-interval %) props) outer-interval props)))

(s/def ::number2->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::number :inner ::number) :ret ::tensor)
    #(gen/one-of (map gen/return (list (fn [outer inner] [[outer 0.0] [2.0 inner]]))))))

(s/fdef non-rectangular-2D-integration
        :args (s/cat :number2->tensor ::number2->tensor
                     :outer-interval ::interval
                     :outer->inner-interval ::number->interval
                     :opts (s/? (s/keys :opt [::points ::accu ::iter-interval])))
        :ret (s/or :exception ::exception :tensor ::tensor))

(defn non-rectangular-3D-integration
  "Returns the integral of a function `number3->tensor` over the outer, middle, and inner intervals using
  global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Use [[rectangular-integration]] for rectangular integration with numbers because that function is faster.
   `number3->tensor` should be a function of (outer, middle, inner).
   `outer-interval` is [a b]
   `outer->middle-interval` takes the outer number and returns an interval [c d]
   `outer+middle->inner-interval` takes the outer and middle number and returns an interval [e f]
   Options:
   `::points` -- 15, 21, 31, 41, 51, 61; default is 15
   `::accu` -- default is m/*sgl-close* per dimension (accuracy is adjusted for quadrature type)
   `::iter-interval` -- default per dimension is [3 300]."
  ([number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval]
   (non-rectangular-3D-integration
     number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval {}))
  ([number3->tensor outer-interval outer->middle-interval outer+middle->inner-interval
    {::keys [points accu iter-interval]
     :or    {points 15, accu m/*sgl-close*, iter-interval [3 300]}
     :as    props}]
   (integration
     (fn [outer]
       (integration
         (fn [middle]
           (integration (fn [inner] (number3->tensor outer middle inner))
                        (outer+middle->inner-interval outer middle) props))
         (outer->middle-interval outer)
         props))
     outer-interval
     props)))

(s/def ::number3->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::number :middle ::number :inner ::number) :ret ::tensor)
    #(gen/one-of (map gen/return (list (fn [outer middle inner] [[outer middle] [2.0 inner]]))))))

(s/fdef non-rectangular-3D-integration
        :args (s/cat :number3->tensor ::number3->tensor
                     :outer-interval ::interval
                     :outer->middle-interval ::number->interval
                     :outer+middle->inner-interval ::number2->interval
                     :opts (s/? (s/keys :opt [::points ::accu ::iter-interval])))
        :ret (s/or :exception ::exception :tensor ::tensor))

(defn non-rectangular-4D-integration
  "Returns the integral of a function `number4->tensor` over the outer, outer-middle, outer-inner, and inner intervals
  using global adaptive integration of the Gauss-Kronrod Quadrature Formula.
   Use [[rectangular-integration]] for rectangular integration with numbers because that function is faster.
   `number4->tensor` should be a function of (outer, outer-middle, inner-middle, inner).
   `outer-interval` is [a b]
   `outer->outer-middle-interval` takes the outer number and returns an interval [c d]
   `outer+outer-middle->inner-middle-interval` takes the outer and outer-middle number and returns an interval [e f]
   `outer+outer-middle+inner-middle->inner-interval` takes the outer, outer-middle,
        and inner-middle numbers and returns and interval [g h]
   Options:
   `::points` -- 15, 21, 31, 41, 51, 61; default is 15
   `::accu` -- default is m/*sgl-close* per dimension (accuracy is adjusted for quadrature type)
   `::iter-interval` -- default per dimension is [2 200]."
  ([number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
    outer+outer-middle+inner-middle->inner-interval]
   (non-rectangular-4D-integration
     number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
     outer+outer-middle+inner-middle->inner-interval {}))
  ([number4->tensor outer-interval outer->outer-middle-interval outer+outer-middle->inner-middle-interval
    outer+outer-middle+inner-middle->inner-interval
    {::keys [points accu iter-interval]
     :or    {points 15, accu m/*sgl-close*, iter-interval [2 200]}
     :as    props}]
   (integration
     (fn [outer]
       (integration
         (fn [outer-middle]
           (integration
             (fn [inner-middle]
               (integration (fn [inner] (number4->tensor outer outer-middle inner-middle inner))
                            (outer+outer-middle+inner-middle->inner-interval outer outer-middle inner-middle)
                            props))
             (outer+outer-middle->inner-middle-interval outer outer-middle)
             props))
         (outer->outer-middle-interval outer)
         props))
     outer-interval
     props)))

(s/def ::number4->tensor
  (s/with-gen
    (s/fspec :args (s/cat :outer ::number :outer-middle ::number :inner-middle ::number :inner ::number) :ret ::tensor)
    #(gen/one-of (map gen/return (list (fn [outer outer-middle inner-middle inner]
                                         [[outer outer-middle] [inner-middle inner]]))))))

(s/fdef non-rectangular-4D-integration
        :args (s/cat :number4->tensor ::number4->tensor
                     :outer-interval ::interval
                     :outer->outer-middle-interval ::number->interval
                     :outer+outer-middle->inner-middle-interval ::number2->interval
                     :outer+outer-middle+inner-middle->inner-interval ::number3->interval
                     :opts (s/? (s/keys :opt [::points ::accu ::iter-interval])))
        :ret (s/or :exception ::exception :tensor ::tensor))

;;;NUMERICAL DERIVATIVES
;;; references: Wiki http://en.wikipedia.org/wiki/Finite_difference 
;;;    and http://en.wikipedia.org/wiki/Finite_difference_coefficients
(s/def ::matrix ::mx/matrix)
(s/def ::v->v
  (s/with-gen
    (s/fspec :args (s/cat :v ::vector) :ret ::vector)
    #(gen/one-of (map gen/return (list (fn [v] (mapv m/sq v)) (fn [v] (mapv m/cube v)))))))
(s/def ::v->m (s/fspec :args (s/cat :v ::vector) :ret ::matrix))
(s/def ::v->symmetric-m (s/fspec :args (s/cat :v ::vector) :ret ::mx/symmetric-matrix))
(s/def ::fxy (s/with-gen
               (s/fspec :args (s/cat :x ::number :y ::number) :ret ::number)
               #(gen/one-of (map gen/return (list + - (fn [x y] (+ x (* 2 y))))))))
(s/def ::h (s/with-gen ::m/finite+ #(gen/double* {:infinite? false :NaN? false :min m/tiny-dbl :max 0.1})))
(s/def ::dx ::h)
(s/def ::multiplier ::h)
(s/def ::type #{:central :forward :backward})
(s/def ::accuracy (s/and (s/int-in 1 9) (partial not= 7)))
(s/def ::coefficients (s/coll-of ::vector/vector-2D :kind clojure.core/vector? :into []))
(s/def ::row ::mx/row)
(s/def ::column ::mx/column)
(s/def ::derivative (s/int-in 0 9))

(comment "The zero coefficient is left out below, but can be found because 
    the sum of coefficients equals zero")
(def ^:const ^:private central-coefficient1
  [[[1 (/ 2)]] [[2 (/ -12)] [1 (/ 2 3)]] [[3 (/ 60)] [2 (/ -3 20)] [1 (/ 3 4)]]
   [[4 (/ -280)] [3 (/ 4 105)] [2 (/ -5)] [1 (/ 4 5)]]])

(def ^:const ^:private central-coefficient2
  [[[1 1]] [[2 (/ -12)] [1 (/ 4 3)]] [[3 (/ 90)] [2 (/ -3 20)] [1 (/ 3 2)]]
   [[4 (/ -560)] [3 (/ 8 315)] [2 (/ -5)] [1 (/ 8 5)]]])

(def ^:const ^:private central-coefficient3
  [[[2 (/ 2)] [1 -1]] [[3 (/ -8)] [2 1] [1 (/ -13 8)]]
   [[4 (/ 7 240)] [3 (/ -3 10)] [2 (/ 169 120)] [1 (/ -61 30)]]])

(def ^:const ^:private central-coefficient4
  [[[2 1] [1 -4]] [[3 (/ -6)] [-2 2] [1 (/ -13 2)]]
   [[4 (/ 7 240)] [3 (/ -2 5)] [2 (/ 169 60)] [1 (/ -122 15)]]])

(def ^:const ^:private forward-coefficient1
  [[[1 1]]
   [[2 (/ -2)] [1 2]]
   [[3 (/ 3)] [2 (/ -3 2)] [1 3]]
   [[4 (/ -4)] [3 (/ 4 3)] [2 -3] [1 4]]
   [[5 (/ 5)] [4 (/ -5 4)] [3 (/ 10 3)] [2 -5] [1 5]]
   [[6 (/ -6)] [5 (/ 6 5)] [4 (/ -15 4)] [3 (/ 20 3)] [2 -7.5] [1 6]]])

(def ^:const ^:private forward-coefficient2
  [[[2 1] [1 -2]]
   [[3 -1] [2 4] [1 -5]]
   [[4 (/ 11 12)] [3 (/ -14 3)] [2 (/ 19 2)] [1 (/ -26 3)]]
   [[5 (/ -5 6)] [4 (/ 61 12)] [3 -13] [2 (/ 107 6)] [1 (/ -77 6)]]
   [[6 (/ 137 180)] [5 (/ -27 5)] [4 (/ 33 2)] [3 (/ -254 9)] [2 (/ 117 4)]
    [1 (/ -87 5)]]
   [[7 (/ -7 10)] [6 (/ 1019 180)] [5 (/ -201 10)] [4 41] [3 (/ -949 18)]
    [2 (/ 879 20)] [1 (/ -223 10)]]])

(def ^:const ^:private forward-coefficient3
  [[[3 1] [2 -3] [1 3]]
   [[4 (/ -3 2)] [3 7] [2 -12] [1 9]]
   [[5 (/ 7 4)] [4 (/ -41 4)] [3 (/ 49 2)] [2 (/ -59 2)] [1 (/ 71 4)]]
   [[6 (/ -15 8)] [5 13] [4 (/ -307 8)] [3 62] [2 (/ -461 8)] [1 29]]
   [[7 (/ 29 15)] [6 (/ -1849 120)] [5 (/ 268 5)] [4 (/ -2545 24)]
    [3 (/ 389 3)] [2 (/ -3929 40)] [1 (/ 638 15)]]
   [[8 (/ -469 240)] [7 (/ 527 30)] [6 (/ -561 8)] [5 (/ 4891 30)]
    [4 (/ -1457 6)] [3 (/ 2391 10)] [2 (/ -18353 120)] [1 (/ 349 6)]]])

(def ^:const ^:private forward-coefficient4
  [[[4 1] [3 -4] [2 6] [1 -4]]
   [[5 -2] [4 11] [3 -24] [2 26] [1 -14]]
   [[6 (/ 17 6)] [5 -19] [4 (/ 107 2)] [3 (/ -242 3)] [2 (/ 137 2)] [1 -31]]
   [[7 (/ -7 2)] [6 (/ 82 3)] [5 (/ -185 2)] [4 176] [3 (/ -1219 6)] [2 142]
    [1 (/ -111 2)]]
   [[8 (/ 967 240)] [7 (/ -536 15)] [6 (/ 2803 20)] [5 (/ -4772 15)]
    [4 (/ 10993 24)] [3 (/ -2144 5)] [2 (/ 15289 60)] [1 (/ -1316 15)]]])

(defn- convert-central-coefficients
  [m no-zero?]
  (let [r (map #(let [[e1 e2] %] [(- e1) (if no-zero? (- e2) e2)]) m)
        extra (when-not no-zero? [[0 (* -2.0 (apply + (map second m)))]])]
    (vec (concat m extra r))))

(s/fdef convert-central-coefficients
        :args (s/cat :m ::matrix :no-zero? boolean?)
        :ret ::matrix)

(defn- get-central-coefficients
  [deriv accuracy]
  (let [a (/ accuracy 2)
        m (condp = deriv
            1 central-coefficient1
            2 central-coefficient2
            3 central-coefficient3
            4 central-coefficient4)]
    (convert-central-coefficients (nth m (dec a)) (odd? deriv))))

(s/fdef get-central-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 5) :accuracy #{2 4 6 8})
                     #(or (<= (:accuracy %) 6) (<= (:deriv %) 2)))
        :ret ::coefficients)

(defn- get-forward-coefficients
  [deriv accuracy]
  (let [m (condp = deriv
            1 forward-coefficient1
            2 forward-coefficient2
            3 forward-coefficient3
            4 forward-coefficient4)
        coefficient (nth m (dec accuracy))]
    (conj coefficient [0 (- (apply + (map second coefficient)))])))

(s/fdef get-forward-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 7) :accuracy (s/int-in 1 7))
                     #(or (<= (:accuracy %) 5) (<= (:deriv %) 3)))
        :ret ::coefficients)

(defn- get-backward-coefficients
  "backward is like forward, except for odd derivatives the sign switches."
  [deriv accuracy]
  (let [coefficient (get-forward-coefficients deriv accuracy)]
    (mapv #(let [[e1 e2] %] [(- e1) (if (odd? deriv) (- e2) e2)]) coefficient)))

(s/fdef get-backward-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 7) :accuracy (s/int-in 1 7))
                     #(or (<= (:accuracy %) 5) (<= (:deriv %) 3)))
        :ret ::coefficients)

(defn derivative-fn
  "Returns a numerical derivative function.  
   Function `number->number` takes and returns a number.
   Note that [[derivative-fn]] will not be accurate when inputs or outputs are so large when
   divided by `::h` that they lose precision.
   Options:
      `::derivative` -- can be 0 or 1 (default) to 8
      `::h` -- (default is m/*sgl-close* for 1st deriv, 10x less for others) is the denominator,
         which is equal to (dx ^ `::derivative`),
         where dx is the small change (smaller `::h` isn't usually better, changes to `::h` can be important)
      `::type` -- can be `:central` (default), `:forward`, or `:backward`
      `::accuracy` --can be 2, 4, 6, or 8 for central (no 8 for 3rd or 4th deriv),
         and 1-6 for forward or backward (no 6 for 4th deriv).
         (default `::accuracy` is 2 for `::derivative` <= 2, else 6.
         `::accuracy` is ignored for `::derivative` > 4 and default accuracies are used."
  ([number->number] (derivative-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy] :or {derivative 1, type :central}}]
   (let [derivative (int derivative)
         accuracy (when accuracy (int accuracy))
         h (when h (double h))]
     (cond (zero? derivative) number->number
           (> derivative 4) (let [exc (- derivative 4)
                                  x (if h (/ h (/ m/*sgl-close* 10)) 1.0)]
                              (derivative-fn
                                (derivative-fn
                                  number->number
                                  {::derivative exc
                                   ::h          (* x (m/pow 10 (/ (+ 3 exc) -2)))
                                   ::type       type})
                                {::derivative 4
                                 ::h          (* x (m/pow 10 (/ (+ 11 (- exc)) -2)))
                                 ::type       type}))
           :else (let [h (cond h h
                               (m/one? derivative) m/*sgl-close*
                               :else (/ m/*sgl-close* 10))
                       accuracy (cond accuracy accuracy
                                      (<= derivative 2) 2
                                      (and (== derivative 4) (not= type :central)) 5
                                      :else 6)
                       coefficient-fn (condp = type
                                        :central get-central-coefficients
                                        :forward get-forward-coefficients
                                        :backward get-backward-coefficients)
                       multiplier (/ h)
                       dx (m/pow h (/ derivative))
                       coefficient (map #(let [[e1 e2] %] [(* dx e1) e2]) (coefficient-fn derivative accuracy))]
                   (fn [v] (* multiplier
                              (apply + (map #(let [[e1 e2] %] (* (number->number (+ v e1)) e2)) coefficient)))))))))

(s/fdef derivative-fn
        :args (s/cat :number->number ::number->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::derivative ::h ::type ::accuracy])
                                  (fn [v] (let [d (get v ::derivative 1)
                                                t (get v ::type :central)
                                                a (get v ::accuracy (cond (<= d 2) 2
                                                                          (and (== d 4) (not= t :central)) 5
                                                                          :else 6))]
                                            (if (= t :central)
                                              (and (even? a) (or (and (<= d 2) (<= a 8)) (<= a 6)))
                                              (and (<= a 6) (or (<= d 3) (<= a 5)))))))))
        :ret ::number->number)

(defn gradient-fn
  "Returns a numerical gradient function.  
   Function `v->number` takes a vector and returns a number.
   The output function takes and returns a vector.
   Options:
      `::h` (default m/*sgl-close*) is the denominator, which is equal to dx,
         where dx is the small change (smaller h isn't usually better, changes to h can be important)
      `::type` can be `:central` (default), `:forward`, or `:backward`
      `::accuracy` can be 2 (default), 4, 6, or 8 for `:central`, and 1-6 for `:forward` or `:backward`."
  ([v->number] (gradient-fn v->number {}))
  ([v->number {::keys [h type accuracy] :or {h m/*sgl-close*, type :central, accuracy 2}}]
   (let [coefficient-fn (condp = type :central get-central-coefficients
                                      :forward get-forward-coefficients
                                      :backward get-backward-coefficients)
         multiplier (/ h)
         dx (double h)
         coefficient (map #(let [[e1 e2] %] [(* dx e1) e2]) (coefficient-fn 1 accuracy))]
     (fn [v]
       (vec
         (map-indexed
           (fn [i e]
             (* multiplier
                (apply + (map #(let [[e1 e2] %] (* (v->number (assoc v i (+ e e1))) e2)) coefficient))))
           v))))))

(s/fdef gradient-fn
        :args (s/cat :v->number ::v->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v] (let [t (get v ::type :central)
                                                a (get v ::accuracy 2)]
                                            (if (= t :central)
                                              (and (even? a) (<= a 8))
                                              (<= a 6)))))))
        :ret ::v->v)

(defn jacobian-fn
  "Returns a numerical jacobian function.  
   Function `v->v` takes a vector and returns a vector.
   The output function takes a vector and returns a matrix, where each row is the gradient of f's output.
   Options:
   `::h` -- (default m/*sgl-close*) is the denominator, which is equal to dx,
        where dx is the small change (smaller `::h` isn't usually better, changes to `::h` can be important)
   `::type` -- can be `:central` (default), `:forward`, or `:backward`
   `::accuracy` -- can be 2 (default), 4, 6, or 8 for `:central`, and 1-6 for `:forward` or `:backward`."
  ([v->v] (jacobian-fn v->v {}))
  ([v->v {::keys [h type accuracy] :or {h m/*sgl-close*, type :central, accuracy 2}}]
   (let [coefficient-fn (condp = type
                          :central get-central-coefficients
                          :forward get-forward-coefficients
                          :backward get-backward-coefficients)
         multiplier (/ h)
         dx (double h)
         coefficient (map #(let [[e1 e2] %] [(* dx e1) e2]) (coefficient-fn 1 accuracy))]
     (fn [v]
       (if (empty? v)
         [[]]
         (mx/transpose
           (let [m (vec
                     (map-indexed
                       (fn [i e] (apply tensor/add (mapv #(let [[e1 e2] %]
                                                            (tensor/multiply e2 multiplier (v->v (assoc v i (+ e e1)))))
                                                         coefficient)))
                       v))]
             m)))))))

(s/fdef jacobian-fn
        :args (s/cat :v->v ::v->v
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v] (let [t (get v ::type :central)
                                                a (get v ::accuracy 2)]
                                            (if (= t :central)
                                              (and (even? a) (<= a 8))
                                              (<= a 6)))))))
        :ret ::v->m)

(defn- joint-central-derivative
  [v->number v row column dx multiplier]
  (if (or (>= row (count v)) (>= column (count v)))
    m/nan
    (let [i+ (assoc v row (+ (get v row) dx)),
          i- (assoc v row (- (get v row) dx)),
          e++ (assoc i+ column (+ (get v row) dx)),
          e+- (assoc i+ column (- (get v row) dx)),
          e-+ (assoc i- column (+ (get v row) dx)),
          e-- (assoc i- column (- (get v row) dx))]
      (* 0.25 multiplier (- (+ (v->number e++) (v->number e--)) (v->number e+-) (v->number e-+))))))

(s/fdef joint-central-derivative
        :args (s/cat :v->number ::v->number :v ::vector :row ::row :column ::column :dx ::dx :multiplier ::multiplier)
        :ret ::number)

(defn hessian-fn
  "Returns a numerical Hessian function.
   Function `v->number` takes a vector and returns a number.
   The output function takes a vector and returns a symmetric matrix.
   Options:
      `::h` -- (default m/*sgl-close* / 10) is the denominator, which is equal to (dx * dx),
          where dx is the small change (smaller `::h` isn't usually better, changes to `::h` can be important)
      `::type` can be `:joint-central` (default), `:central`, `:forward`, or `:backward`
      `::accuracy` can be 2 (default, only choice for `:joint-central`); 2, 4, 6, or 8 for `:central`;
          and 1-6 for `:forward` or `:backward`."
  ([v->number] (hessian-fn v->number {}))
  ([v->number {::keys [h type accuracy] :or {h (* m/*sgl-close* 0.1), type :joint-central, accuracy 2}}]
   (if-not (= type :joint-central)
     (fn [v] (mx/symmetric-matrix-by-averaging
               ((jacobian-fn (gradient-fn v->number {::h (m/sqrt h), ::type type, :accuracy accuracy})) v)))
     (let [multiplier (/ h),
           dx (m/sqrt h),
           coefficient (map #(let [[e1 e2] %] [(* dx e1) e2]) (get-central-coefficients 2 2))]
       (fn [v]
         (if (empty? v)
           [[]]
           (let [cv (count v)]
             (mx/symmetric-matrix-by-averaging
               (mx/compute-matrix
                 cv
                 cv
                 (fn [row column]
                   (cond
                     (or (>= row cv) (>= column cv)) m/nan

                     (== row column)
                     (* multiplier (apply + (map #(let [[e1 e2] %]
                                                    (* (v->number (assoc v row (+ (get v row) e1))) e2))
                                                 coefficient)))

                     :else (joint-central-derivative v->number v row column dx multiplier))))))))))))

(s/fdef hessian-fn
        :args (s/cat :v->number ::v->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v] (let [t (get v ::type :joint-central)
                                                a (get v ::accuracy 2)]
                                            (cond (= t :joint-central) (== a 2)
                                                  (= t :central) (and (even? a) (<= a 8))
                                                  :else (<= a 6)))))))
        :ret ::v->symmetric-m)

(defn partial-derivative-x-of-fxy
  ([fxy] (partial-derivative-x-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h m/*sgl-close*}}]
   (fn [x y] ((derivative-fn #(fxy % y) {::h h}) x))))

(s/fdef partial-derivative-x-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn partial-derivative-y-of-fxy
  ([fxy] (partial-derivative-y-of-fxy fxy {}))
  ([fxy {:keys [h] :or {h m/*sgl-close*}}]
   (fn [x y] ((derivative-fn #(fxy x %) {::h h}) y))))

(s/fdef partial-derivative-y-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-xx-of-fxy
  ([fxy] (second-partial-derivative-xx-of-fxy fxy {}))
  ([fxy {:keys [h] :or {h (* m/*sgl-close* 0.1)}}]
   (fn [x y] ((derivative-fn #(fxy % y) {::derivative 2 ::h h}) x))))

(s/fdef second-partial-derivative-xx-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-yy-of-fxy
  ([fxy] (second-partial-derivative-yy-of-fxy fxy {}))
  ([fxy {:keys [h] :or {h (* m/*sgl-close* 0.1)}}]
   (fn [x y] ((derivative-fn #(fxy x %) {::derivative 2 ::h h}) y))))

(s/fdef second-partial-derivative-yy-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-xy-of-fxy
  ([fxy] (second-partial-derivative-xy-of-fxy fxy {}))
  ([fxy {:keys [h] :or {h (* m/*sgl-close* 0.1)}}]
   (fn [x y] (joint-central-derivative #(fxy (first %) (second %)) [x y] 0 1 (m/sqrt h) (/ h)))))

(s/fdef second-partial-derivative-xy-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)