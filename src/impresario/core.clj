(ns impresario.core)


;; TODO: rename state-store / store to 'context'
;; TODO: triggers should return the new context
;; TODO: transition*? should return state-name
;; TODO: transition*! should return tuple [stat-name new-context]
;; TODO: add sentinal for :stop not just :start
;; TODO: how to support a 'trace'?  a seq of all the states that were transitioned through and each version of the context at the time (before/after)
;; TODO: validate 1 and only 1 start state
;; TODO: place a bound/limit on the # of times a cycle can be followed :: prevents infinite loops
;; TODO: external triggers: timers, 'waking up' - also support a 'wake' trigger?
;; TODO: an exception state that any state can transition to on error?
;; TODO: support [global] error handler when a transition was expected but did not occurr?
;; TODO: make sure we fire the :on-entry for start states when the workflow is created
;; TODO: support a registry of workflows?
;; TODO: default context keys?  :start-time :current-time :history (?)

;; state is represented as just a keyword
;; workflows are simply maps, the key is a state name
;; values are descriptions of the state, including outbound transitions
;; observers may be attached to the workflow

(defn split-keyword-parts [kwd]
  (.split (.substring (str kwd) 1)
          "/"))

(defn fn-lookup-via-symbol [sym-name]
  (let [[ns-name-part sym-name-part] (split-keyword-parts sym-name)]
    (ns-resolve (symbol ns-name-part)
                (symbol sym-name-part))))

;; ((fn-lookup-via-symbol :impresario.test.core/transition-every-time) nil nil nil)

(defn resolve-predicate [pred]
  (cond
    (keyword? pred)
    (fn-lookup-via-symbol pred)
    :else
    (throw (RuntimeException. (format "Don't know how to resolve: '%s'" pred)))))

(defn can-transition-to? [workflow current-state transition-info state-store]
  (let [pred       (resolve-predicate (:if transition-info))
        state-name (:state transition-info)]
    (if (pred workflow current-state state-store)
      state-name
      nil)))


(defn transition-once? [workflow current-state state-store]
  (let [transitions (:transitions (get (:states workflow)
                                       current-state))
        viable-next-states (filter (fn [transition-info]
                                     (can-transition-to?
                                      workflow
                                      current-state
                                      transition-info
                                      state-store))
                                   transitions)]
    (printf "transitions:%s\n" (vec transitions))
    (printf "viable-next-states:%s\n" (vec viable-next-states))
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

(defn transition? [workflow current-state state-store]
  (loop [prev-state current-state
         next-state (transition-once? workflow current-state state-store)]
    (printf "transition? prev-state:%s next-state:%s\n" prev-state next-state)
    (if (nil? next-state)
      ;; we're done here
      prev-state
      (recur next-state
             (transition-once? workflow next-state state-store)))))

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
    (printf "on-transition-triggers: %s to %s : %s" current-state next-state (vec state-info))
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

(defn execute-trigger [trigger workflow current-state next-state state-info]
  (let [f (resolve-predicate trigger)]
    (f workflow current-state next-state state-info)))

(defn execute-triggers [workflow current-state next-state state-store]
  (doseq [trigger (on-exit-triggers workflow current-state)]
    (execute-trigger trigger workflow current-state next-state state-store))
  (doseq [trigger (on-transition-triggers workflow current-state next-state)]
    (execute-trigger trigger workflow current-state next-state state-store))
  (doseq [trigger (on-entry-triggers workflow next-state)]
    (execute-trigger trigger workflow current-state next-state state-store)))

(defn transition-once! [workflow current-state state-store]
  (let [next-state (transition-once? workflow current-state state-store)]
    (if (nil? next-state)
      ;; no transition
      current-state
      (do
        (execute-triggers workflow current-state next-state state-store)
        next-state))))

(defn transition! [workflow current-state state-store]
  (loop [prev-state current-state
         next-state (transition-once! workflow current-state state-store)]
    (printf "transition! prev-state:%s next-state:%s\n" prev-state next-state)
    (if (nil? next-state)
      ;; we're done here, didn't transition
      prev-state
      ;; keep trying to transition
      (recur next-state
             (transition-once? workflow next-state state-store)))))

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
        (.append sb (format "  \"%s\" -> \"%s\" [label=\"%s\"];\n"
                            (name state)
                            (name (:state transition))
                            (name (:if transition))))))
    (.append sb "}\n")
    (str sb)))
