(ns impresario.core)

;; TOOD: implement a state as a record / protocol?
;; TOOD: implement a transitions to as a record / protocol?
;; TODO: implement the workflow as a record / protocol?


(defn spec-start-states [spec]
  (filter #(:start (get (:states spec) %))
          (keys (:states spec))))

;; TODO: fore transition predicates, make-workflow supports either
;; keywords (qualified with namespace:
;; impresario.core.test/close-door?) or symbols (unqualified, in which
;; case it looks up the namespace via the meta-data) - we should
;; ensure we can resolve all those as part of the validateion
(defn validate! [spec]
  (if (nil? spec)
    (throw (IllegalArgumentException. "Error: null specification")))
  (if (empty? (keys spec))
    (throw (IllegalArgumentException. "Error: empty specifiction")))
  ;; must have 1 and only 1 start state
  (let [start-nodes (filter #(get %1 :start) (vals (:states spec)))]
    (if-not (= 1 (count start-nodes))
      (throw (IllegalArgumentException.
              (format "Error: zero or multiple start states: (%s) :: must have 1 and only 1 initial state."
                      (map :name start-nodes))))))
  true)

(defn spec-start-state [spec]
  (first (filter (fn [state-name]
                   (:start (get (:states spec)
                                state-name)))
                 (keys (:states spec)))))

(defn split-keyword-parts [kwd]
  (.split (.substring (str kwd) 1)
          "/"))

(defn fn-lookup-via-symbol [sym-name & args]
  (let [[ns-name-part sym-name-part] (split-keyword-parts sym-name)]
    (printf "fn-lookup-via-symbol: %s / %s\n" ns-name-part sym-name-part)
    (ns-resolve (symbol ns-name-part)
                (symbol sym-name-part))))

(defn fn-invoke-via-symbol [sym-name & args]
  (let [fn (fn-lookup-via-symbol sym-name)]
    (apply fn args)))

;; (fn-invoke-via-symbol :clojure.core/printf "foo:%s %s%s" "bar" "qux" "!")

(defn construct-internal-store [spec]
  (let [internal-store (:internal-store spec)]
    (cond
      (keyword? internal-store)
      (fn-invoke-via-symbol internal-store)

      (map? internal-store)
      internal-store

      :else
      (throw (IllegalArgumentException.
              (format
               "Error, spec's internal-store type (%s) not recognized, expected map or keyword in: %s"
               (class internal-store)
               spec))))))

(defn symbol-to-combined-keyword [spec-ns thing]
  ;; convert whatever thing is into a keyword that contains both the
  ;; package and namespace so that we can serialize, reify and lookup
  ;; and call it by that name in the future...
  (printf "symbol-to-combined-keyword thing=%s\n" thing)
  (cond
    (keyword? thing)
    (keyword (str (.substring (str spec-ns) 1) "/" (name thing)))

    (symbol? thing)
    (let [thing-var (resolve thing)
          thing-meta (meta thing-var)]
      (keyword (str (str (:ns thing-meta))
                    "/"
                    (str (:name thing-meta)))))
    :else
    (throw (IllegalArgumentException. (format "Unable to resolve: %s of %s"
                                              thing (class thing))))))

(defn resolve-symbol-in-state [spec-ns state]
  (printf "resolve-symbol-in-state: spec-ns=%s state=%s\n" spec-ns state)
  (printf "resolve-symbol-in-state: transitions-to=%s\n" (:transitions-to state))
  (merge state
         {:transitions-to
          (vector
           (map (fn [transition]
                  (printf "transition=%s\n" transition)
                  (if (:if transition)
                    (merge transition {:if (symbol-to-combined-keyword spec-ns (:if transition))})
                    transition))
                (:transitions-to state)))}))

(defn resolve-symbols-in-states [spec-ns states]
  (printf "resolve-symbols-in-states: stats=%s; ns=%s\n" states spec-ns)
  (loop [m {}
         [k & ks] (keys states)]
    (cond (not k)
          m
          :else
          (recur (assoc m k (resolve-symbol-in-state spec-ns (get states k)))
                 ks))))

(defn resolve-symbols-in-spec [spec]
  (merge spec {:states (resolve-symbols-in-states
                        (:ns spec)
                        (:states spec))}))


;; on construction, keywordize the predicates so it's easier on the users for the namespacing...
(defn construct-workflow [spec]
  (validate! spec)
  {:name           (:name spec)
   :definition     (resolve-symbols-in-spec spec)
   :internal-store (construct-internal-store spec)
   :external-store {}
   :current-state  (spec-start-state spec)})

(defmacro make-workflow [spec]
  `(construct-workflow
    (merge {:ns (keyword (str ~'*ns*))
            :internal-store {}
            :external-store {}}
           ~spec)))

(defn current-state [worklfow]
  (:current-state worklfow))

(defn current-state-info [workflow]
  (get-in workflow [:definition :states (current-state workflow)]))

(defn possible-transitions-from [workflow state]
  (get-in workflow [:definition :states state :transitions-to]))

(defn resolve-predicate [pred-ns pred-name]
  (ns-resolve (symbol (name pred-ns)) (symbol (name pred-name))))

(defn can-transition? [workflow]
  (printf "can-transition?...\n")
  (let [curr-state             (current-state workflow)
        state-info             (current-state-info workflow)
        transitions            (possible-transitions-from workflow curr-state)
        viable-next-states     (filter (fn [transition]
                                         (printf "testing transition: curr-state=%s state-info=%s transition=%s\n" curr-state state-info transition)
                                         (let [pred-name (:if transition)
                                               pred      (resolve-predicate (:ns workflow) pred-name)]
                                           (printf "pred-name:%s pred=%s\n" pred-name pred)
                                           (pred workflow)))
                                       transitions)]
    ;; TODO: can-transition-to? should return [ name-of-state new-workflow ]
    viable-next-states))


