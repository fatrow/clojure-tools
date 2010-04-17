(use 'clojure.contrib.test-is)

(defn slice
  ([coll start]
     (let [start (max start 0)]
       (drop start coll)))       
  ([coll start end]
     (let [start (max start 0)]
       (take (- end start) (drop start coll))))
  {:doc "Extract a subsequencet like a slice of python."
   :test #(are (= _1 _2)
	       (slice [] 2 7) []
	       (slice [1 2 3 4 5] 1) [2 3 4 5]
	       (slice '(a b c d e f g h i k) 2 4) '(c d)
	       (slice "abcdefg" 3) '(\d \e \f \g)
	       (slice "abcdefg" 0 2) '(\a \b)
	       (slice [1 2 3 4] 5) '()
	       (slice [1 2 3 4] 3 0) '()
	       (slice "abcd" -1 6) '(\a \b \c \d)
	       (slice "abcd" -1 -3) '()
	       (slice "abcd" -4 -1) '())})

(defn count-item
  ([coll] (count coll))
  ([item coll]
     (reduce (fn [sum x] (if (= item x) (inc sum) sum)) 0 coll))
     ;(count (filter #(= item %) coll))
  {:doc "count a number of times of item from coll."
   :test #(are (= _1 _2)
	       (count-item \a "abcd") 1
	       (count-item \a "abAcaeabcsa") 4
	       (count-item "c" "bcabcda") 0
	       (count-item "bc" "bcabcda") 0
	       (count-item 'a '(e a b c a)) 2)})

(defn touch [coll target-index]
  (-> [(coll target-index)]
      (into (subvec coll 0 target-index))
      (into (subvec coll (inc target-index)))))

(use 'clojure.contrib.pprint)
(pprint (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))

(defn elem? [x]
  (not (coll? x)))

(defmacro visu [form]
  (letfn [(make-table [m node]
		      (if (coll? node)
			(reduce (fn [ma lis]
				  (assoc ma lis (eval lis)))))))))
(defmacro re-const [form]
  `(letfn [(~(second form) ~(nth form 2)
	    ~(nth form 3))]))
(re-const (defn aaa [] (+ (inc 1) (inc 2))))

(defmacro re-const [m form]
  (let [mem (atom {})]
  `(letfn [(~(second form) ~(nth form 2)
	    ~(if (coll? (nth form 3))
	       ])])))

(defn func? [x]
  (or (ifn? x) (fn? x)))

(defn capn [mem form]
  (cap mem form))
(defmacro cap [mem form]
  (cond (elem? form) `(do (swap! ~mem assoc '~form ~form) '~form)
	(list? form) `(do (doall (map capn (repeat ~mem) (rest ~form)))
			  (swap! ~mem assoc '~form ~form)
			  '~form)))

(defmacro caprest [mem form]
  (cond (elem? form) `(do (swap! ~mem assoc '~form ~form) '~form)
	(coll? form) `(do (if (list? (first ~form))
			    (cap ~mem (first ~form))
			    (caprest ~mem (first ~form)))
			  (swap! ~mem assoc '~form ~form)
			  (when-not (empty? (rest ~form))
			    (caprest ~mem (rest ~form)))
			  (@~mem '~form))))
(declare caprest)
(defn cap [stack form]
  (cond (elem? form) (cons form stack)
	(empty? form) (cons form stack)
	(list? form) (if (func? (first form))
		       (caprest (cons form stack) (rest form))
		       (caprest (cons form stack) form))))

(defn caprest [stack form]
  (cond (elem? form) (cons form stack)
	(empty? form) (cons form stack)
	:else (let [head (first form)
		    res (rest form)]
		(cond (list? head) (caprest (cap stack head) res)
		      (coll? head) (caprest (caprest stack head) res)
		      :else (caprest (cons head stack) res)))))


(defn a1 [test]
  (if test (println "hello")))
(defn a2 [test s]
  (if test s))
(defn a3 [test s]
  (if test (eval s)))

(Defn my-when-not [test & body]
  (if test nil (eval `(do ~@body))))

(defn my-when-not [test body]
  (println test body)
  (if test nil body))

(defn my-read-line
  "Reads the next line from stream that is the current value of *in* ."
  ([]
     (if (instance? clojure.lang.LineNumberingPushbackReader *in*)
       (.readLine #^clojure.lang.LineNumberingPushbackReader *in*)
       (.readLine #^java.io.BufferedReader *in*)))
  ([input]
     (if (instance? clojure.lang.LineNumberingPushbackReader input)
       (.readLine #^clojure.lang.LineNumberingPushbackReader input)
       (.readLine #^java.io.BufferedReader *in*))))


(defmacro defun [name arg & body]
  `(defn ~name ~arg
     (letfn [~@(map (fn [exp] (rest exp))
		    (filter #(= (first %) 'define) body))]
       ~@(filter #(not= (first %) 'define) body))))


(defn envelop-letfn [body]
  (if (zero? (count (filter #(= (first %) 'define) body)))
    body
    `(letfn [~@(map (fn [exp] (rest exp))
		    (filter #(= (first %) 'define) body))]
       ~@(filter #(not= (first %) 'define) body))))

(defmacro defun [name & fdecl]
  (let [m (if (string? (first fdecl))
	    {:doc (first fdecl)}
	    {})
	fdecl (if (string? (first fdecl))
		(next fdecl)
		fdecl)
	m (if (map? (first fdecl))
	    (conj m (first fdecl))
	    m)
	fdecl (if (map? (first fdecl))
		(next fdecl)
		fdecl)
	fdecl (if (vector? (first fdecl))
		(list fdecl)
		fdecl)
	m (if (map? (last fdecl))
	    (conj m (last fdecl))
	    m)
	fdecl (if (map? (last fdecl))
		(butlast fdecl)
		fdecl)]
    `(defn ~name
       ~@(map #(list (first %)
		     (envelop-letfn (rest %))) fdecl)
       ~m)))

(defun outer [arg1 arg2]
  (define inner1 [iarg1 iarg2] (+ iarg1 iarg2))
  (define inner2 [iarg1 iarg2] (* iarg1 iarg2))
  (inner1 arg1 arg2)
  (inner2 arg1 arg2))
(clojure.core/defn outer
  ([arg1 arg2]
     (clojure.core/letfn
      [(inner1 [iarg1 iarg2] (+ iarg1 iarg2))
       (inner2 [iarg1 iarg2] (* iarg1 iarg2))]
      (inner1 arg1 arg2)
      (inner2 arg1 arg2)))
  {})

(defun outer
    ([arg1 arg2]
       (define inner1 [iarg1 iarg2] (+ iarg1 iarg2))
       (define inner2 [iarg1 iarg2] (* iarg1 iarg2))
       (inner1 arg1 arg2)
       (inner2 arg1 arg2))
  {:doc "expandtest" :test (fn [] (assert (= 0 0)))})

(clojure.core/defn outer
  ([arg1 arg2]
     (clojure.core/letfn
      [(inner1 [iarg1 iarg2] (+ iarg1 iarg2))
       (inner2 [iarg1 iarg2] (* iarg1 iarg2))]
      (inner1 arg1 arg2)
      (inner2 arg1 arg2)))
  {:test (fn [] (assert (= 0 0))), :doc "expandtest"})


