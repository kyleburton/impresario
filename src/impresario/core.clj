(ns impresario.core)

;; TODO: validate 1 and only 1 start state
;; TODO: add sentinal for :stop not just :start
;; TODO: support a registry of workflows?
;; TODO: default context keys?  :start-time :current-time :history (?)

;; TODO: how to support a 'trace'?  a seq of all the states that were transitioned through and each version of the context at the time (before/after)
;; TODO: external triggers: timers, 'waking up' - also support a 'wake' trigger?
;; TODO: an exception state that any state can transition to on error?
;; TODO: support [global] error handler when a transition was expected but did not occurr?
;; TODO: Support a [global] 'on-entry' for any state being entered?
;; TODO: be more accomodating in the definition of predicate symbols: see if we can create a macro that allows us to gleam the namespace the workflow was defined in...

(defn split-keyword-parts [kwd]
  (.split (.substring (str kwd) 1)
          "/"))

(defn fn-lookup-via-symbol [sym-name]
  (let [[ns-name-part sym-name-part] (split-keyword-parts sym-name)]
    (ns-resolve (symbol ns-name-part)
                (symbol sym-name-part))))

(defn resolve-keyword-to-fn [pred]
  (cond
    (keyword? pred)
    (fn-lookup-via-symbol pred)
    :else
    (throw (RuntimeException. (format "resolve-keyword-to-fn: Don't know how to resolve: '%s'" pred)))))

;; Can't have both a :if and an :unless
(defn- get-transition-predicate-fn [transition-info]
  (let [if-pred     (:if transition-info)
        unless-pred (:unless transition-info)]
    (printf "> get-transition-predicate-fn: %s\n" transition-info)
    (cond
      (and if-pred unless-pred)
      (throw (RuntimeException. (format "Error: transition:'%s' has both an :if and an :unless!" transition-info)))

      unless-pred
      (complement (resolve-keyword-to-fn unless-pred))

      if-pred
      (resolve-keyword-to-fn if-pred)

      :else
      (throw (RuntimeException. (format "Error: no :if or :else predicates in transition-info:%s" transition-info))))))


(defn can-transition-to? [workflow current-state transition-info context]
  (let [pred       (get-transition-predicate-fn transition-info)
        state-name (:state transition-info)]
    (printf "> can-transition-to? testing [%s->%s] pred:%s\n" current-state state-name pred)
    (if (pred workflow current-state context)
      state-name
      nil)))

(defn transition-once? [workflow current-state context]
  (let [transitions (:transitions (get (:states workflow)
                                       current-state))
        viable-next-states (vec
                            (filter (fn [transition-info]
                                      (can-transition-to?
                                       workflow
                                       current-state
                                       transition-info
                                       context))
                                    transitions))]
    (printf "> transition-once?: transitions:%s\n" (vec transitions))
    (printf "> transition-once?: viable-next-states:%s\n" viable-next-states)
    (printf "> transition-once?: viable-next-states:%s\n" (vec viable-next-states))
    (cond
      (= 1 (count viable-next-states))
      (:state (first viable-next-states))

      (zero? (count viable-next-states))
      nil

      :else
      (throw
       (RuntimeException.
        (format "Error: multiple target states! workflow:%s curr:%s viable-targets:%s"
                (:name workflow)
                current-state
                (vec viable-next-states)))))))

(defn transition? [workflow current-state context]
  (loop [prev-state current-state
         next-state (transition-once? workflow current-state context)]
    ;;(printf "transition? prev-state:%s next-state:%s\n" prev-state next-state)
    (if (nil? next-state)
      ;; we're done here
      prev-state
      (recur next-state
             (transition-once? workflow next-state context)))))

(defn seqize-triggers [triggers]
  (cond
    (nil? triggers)
    nil

    (or (vector? triggers) (seq? triggers))
    triggers

    :else
    [triggers]))

(defn on-exit-triggers [workflow state]
  (seqize-triggers (:on-exit (get (:states workflow) state))))

(defn on-entry-triggers [workflow state]
  (seqize-triggers (:on-entry (get (:states workflow) state))))

(defn get-transition-info [workflow current-state next-state]
  (filter (fn [state-info]
            (= next-state (:state state-info)))
          (:transitions (get (:states workflow)
                             current-state))))

(defn on-transition-triggers [workflow current-state next-state]
  (let [state-info (get-transition-info workflow current-state next-state)]
    ;;(printf "on-transition-triggers: %s to %s : %s" current-state next-state (vec state-info))
    (cond
      (empty? state-info)
      (throw
       (RuntimeException.
        (format "Error: unable to find transition from %s to %s?? While looking up transition triggers."
                current-state
                next-state)))

      (> 1 (count state-info))
      (RuntimeException.
       (format "Error: found more than 1 transition from %s to %s?? While looking up transition triggers."
               current-state
               next-state))

      :else
      (seqize-triggers (:on-transition (first state-info))))))

(defn execute-trigger [trigger workflow current-state next-state context]
  (let [f (resolve-keyword-to-fn trigger)]
    (if-not f
      (throw (RuntimeException. (format "Error: unable to resolve trigger (%s) to function!" trigger))))
    (printf "> executing trigger: %s/%s state:%s/%s\n" trigger f current-state next-state)
    (f workflow current-state next-state context)))

(defn execute-triggers [workflow current-state next-state context]
  (let [context (atom context)]
    (doseq [trigger (on-exit-triggers workflow current-state)]
      (reset! context (execute-trigger trigger workflow current-state next-state @context)))
    (doseq [trigger (on-transition-triggers workflow current-state next-state)]
      (reset! context (execute-trigger trigger workflow current-state next-state @context)))
    (doseq [trigger (on-entry-triggers workflow next-state)]
      (reset! context (execute-trigger trigger workflow current-state next-state @context)))
    @context))

(defn transition-once! [workflow current-state context]
  (let [next-state (transition-once? workflow current-state context)]
    (if-not next-state
      [current-state context]
      [next-state (execute-triggers workflow current-state next-state context)])))

(defn transition! [workflow current-state context & [max]]
  (if (nil? workflow)
    (throw (RuntimeException. "Error: invalid workflow (nil)!")))
  (loop [[prev-state prev-context] [current-state context]
         [next-state next-context] (transition-once! workflow current-state context)
         iterations (or max 100)]
    ;;(printf "transition! prev-state:%s next-state:%s\n" prev-state next-state)
    (cond
      ;; we're done here, didn't transition
      (nil? next-state)
      [prev-state prev-context]

      (zero? iterations)
      (throw (RuntimeException. (format "Error: maximum number of iterations [%s] exceeded, aborting flow." max)))

      ;; keep trying to transition
      :else
      (recur [next-state next-context]
             (transition-once! workflow next-state context)
             (dec iterations)))))

(defn workflow-to-dot [workflow current-state]
  (let [sb (StringBuilder. (format "digraph \"%s\" {\n" (name (:name workflow))))]
    (doseq [state (keys (:states workflow))]
      (let [shape (if (or (:start (get (:states workflow) state))
                          (empty? (:transitions (get (:states workflow) state))))
                    "ellipse"
                    "box")]
        (.append sb (format "  \"%s\" [shape=%s];\n" (name state)
                            shape)))
      (doseq [transition (:transitions (get (:states workflow) state))]
        ;; name the edges...
        (let [to-state (:state transition)
              transition-type
              (cond (:if transition)
                    ""
                    (:unless transition)
                    "!"
                    :else
                    (throw
                     (RuntimeException.
                      (format
                       "Error: no :if or :unless in transition: %s for %s"
                       transition
                       state))))
              pred-name (or (:if transition)
                            (:unless transition))]
          (.append sb (format "  \"%s\" -> \"%s\" [label=\" %s%s\"];\n"
                              (name state)
                              (name to-state)
                              transition-type
                              (name pred-name))))))
    (.append sb "}\n")
    (str sb)))

(defn get-start-state [workflow]
  (first (first (filter (fn [[k v]]
                          (:start v))
                        (:states workflow)))))

(defn initialize-workflow [workflow context]
  "Executes start-state triggers."
  (let [start-state (get-start-state workflow)
        state-info  (get (:states workflow) start-state)
        triggers    (seqize-triggers (:on-entry state-info))]
    (doseq [trigger triggers]
      (execute-trigger trigger workflow nil start-state context))))


(defonce *registered-workflows* (atom {}))

(defn register-workfow [name definition]
  (if-not (keyword? name)
    (throw (RuntimeException. (format "workflow name [%s] must be a keyword, it was: %s" name (class name)))))
  (swap! *registered-workflows*
         assoc
         name
         (assoc definition :name name)))

;; TODO: resolve/store off the *ns* predicates via the macro
;; TODO: support :unless predicates

(defmacro register-workfow! [name definition]
  ;; walk the tree, anywhere we have one of [:if :unless :on-entry
  ;; :on-exit, :on-transition] do the ns resolution
  `(register-workfow ~name ~definition))

(defn lookup-workflow [name]
  (get @*registered-workflows* name))