(ns impresario.core
  (:require [clojure.contrib.pprint :as pp]))

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

(defn get-workflow [wkflow]
  (cond
    (keyword? wkflow)
    (lookup-workflow wkflow)

    (map? wkflow)
    wkflow

    :else
    (throw (RuntimeException. (format "Error: can't get workflow via '%s' not a keyword (lookup) or a map (identity)" wkflow)))))


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
  (let [workflow (get-workflow workflow)
        transitions (:transitions (get (:states workflow)
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
  (let [workflow (get-workflow workflow)]
    (loop [prev-state current-state
           next-state (transition-once? workflow current-state context)]
      ;;(printf "transition? prev-state:%s next-state:%s\n" prev-state next-state)
      (if (nil? next-state)
        ;; we're done here
        prev-state
        (recur next-state
               (transition-once? workflow next-state context))))))

(defn seqize-triggers [triggers]
  (cond
    (nil? triggers)
    nil

    (or (vector? triggers) (seq? triggers))
    triggers

    :else
    [triggers]))

(defn on-exit-triggers [workflow state]
  (let [workflow (get-workflow workflow)]
    (seqize-triggers (:on-exit (get (:states workflow) state)))))

(defn on-entry-triggers [workflow state]
  (let [workflow (get-workflow workflow)]
    (seqize-triggers (:on-entry (get (:states workflow) state)))))

(defn get-transition-info [workflow current-state next-state]
  (let [workflow (get-workflow workflow)]
    (filter (fn [state-info]
              (= next-state (:state state-info)))
            (:transitions (get (:states workflow)
                               current-state)))))

(defn on-transition-triggers [workflow current-state next-state]
  (let [workflow (get-workflow workflow)
        state-info (get-transition-info workflow current-state next-state)]
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
  (let [workflow (get-workflow workflow)
        f (resolve-keyword-to-fn trigger)]
    (if-not f
      (throw (RuntimeException. (format "Error: unable to resolve trigger (%s) to function!" trigger))))
    (printf "> executing trigger: %s/%s state:%s/%s\n" trigger f current-state next-state)
    (f workflow current-state next-state context)))

(defn execute-triggers [workflow current-state next-state curr-context]
  (let [context-validator-fn (if (:context-validator-fn workflow)
                               (resolve-keyword-to-fn (:context-validator-fn workflow))
                               nil)
        context (atom curr-context)
        uuid    (:uuid curr-context)
        set-context!
        (fn [trigger-type trigger]
          (let [v (execute-trigger trigger workflow current-state next-state @context)]
            (printf "!! execute-triggers/set-context! updated from: %s to %s\n" @context v)
            (if context-validator-fn
              (context-validator-fn workflow current-state next-state trigger-type trigger curr-context v))
            (reset! context v)))]
    (doseq [trigger (on-exit-triggers workflow current-state)]
      (set-context! :on-exit       trigger))
    (doseq [trigger (on-transition-triggers workflow current-state next-state)]
      (set-context! :on-transition trigger))
    (doseq [trigger (on-entry-triggers workflow next-state)]
      (set-context! :on-entry      trigger))
    ;; UUID can't be lost from or overidden in the context
    (assoc
        @context
      :uuid uuid)))

(defn transition-once! [workflow current-state context]
  (let [workflow (get-workflow workflow)
        next-state (transition-once? workflow current-state context)]
    (if-not next-state
      [current-state context]
      [next-state (execute-triggers workflow current-state next-state context)])))

(defn is-final-state? [workflow state-name]
  (let [workflow (get-workflow workflow)]
    (get-in workflow [:states state-name :stop])))

(defn transition! [workflow current-state context & [max]]
  (if (nil? workflow)
    (throw (RuntimeException. "Error: invalid workflow (nil)!")))
  (let [workflow (get-workflow workflow)
        max (or max 100)]
   (loop [[prev-state prev-context] [current-state context]
          [next-state next-context] (transition-once! workflow current-state context)
          iterations max]
     ;;(printf "transition! prev-state:%s next-state:%s\n" prev-state next-state)
     (printf "!! transition! transitioned %s => %s\n" prev-state next-state)
     (pp/pprint next-context)
     (cond
       ;; we're done here, didn't transition
       (nil? next-state)
       [prev-state prev-context]

       (is-final-state? workflow next-state)
       [next-state next-context]

       (zero? iterations)
       (throw (RuntimeException. (format "Error: maximum number of iterations [%s] exceeded, aborting flow." max)))

       ;; keep trying to transition
       :else
       (recur [next-state next-context]
              (transition-once! workflow next-state next-context)
              (dec iterations))))))

(defn workflow-to-dot [workflow current-state]
  (let [workflow (get-workflow workflow)
        sb (StringBuilder. (format "digraph \"%s\" {\n" (name (:name workflow))))]
    (doseq [state (keys (:states workflow))]
      (let [shape (if (or (:start (get (:states workflow) state))
                          (empty? (:transitions (get (:states workflow) state))))
                    "ellipse"
                    "box")
            style (if (= current-state state)
                    "bold"
                    "")]

        (.append sb (format "  \"%s\" [shape=%s,style=%s];\n" (name state)
                            shape style)))
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
                        (:states (get-workflow workflow))))))

(defn initialize-workflow [workflow context]
  "Executes start-state triggers."
  (let [workflow (get-workflow workflow)
        start-state (get-start-state workflow)
        state-info  (get (:states workflow) start-state)
        triggers    (seqize-triggers (:on-entry state-info))]
    (loop [[trigger triggers] triggers
           context context]
      (printf "initialize-workflow: trigger=%s context=%s\n" trigger context)
      (if trigger
        (recur triggers
               (execute-trigger trigger workflow nil start-state context))
        (assoc
            context
          :uuid (str (java.util.UUID/randomUUID)))))))

