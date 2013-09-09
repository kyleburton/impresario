(ns impresario.dsl
  (:require
   [impresario.core :as wf]))


(defn- keywordize-fn [f]
  (if (nil? f)
    nil
    (keyword (str (:ns   (meta f)))
             (str (:name (meta f))))))

(defn- add-on-entry-trigger [state-name attrs]
  (let [on-entry-trigger    (symbol (format "on-enter-%s" (name state-name)))
        on-entry-trigger-fn (ns-resolve *ns* on-entry-trigger)]
    (if (not on-entry-trigger-fn)
      attrs
      (assoc attrs
        :on-entry (keywordize-fn on-entry-trigger-fn)))))

;; TODO: support global-on-transition
;; TODO: support global-on-entry
;; TODO: support global-on-exit
;; TODO: support on-exit-{state}

;; TODO: optionally look for a not-transition-from-X-to-Y?
(defn- add-transitions [state-name attrs]
  (reduce
   (fn [attrs to-state-name]
     (let [transition?-name (symbol
                             (format "transition-from-%s-to-%s?"
                                     (name state-name)
                                     (name to-state-name)))
           transition?-fn   (ns-resolve *ns* transition?-name)
           on-transition!-name
           (symbol
            (format "on-transition-from-%s-to-%s!"
                    (name state-name)
                    (name to-state-name)))
           on-transition!-fn   (ns-resolve *ns* on-transition!-name)]
       (if transition?-fn
         (assoc attrs :transitions
                (conj (:transitions attrs)
                      {:state to-state-name
                       :if    (keywordize-fn transition?-fn)
                       :on-transition (keywordize-fn on-transition!-fn)}))
         (throw (RuntimeException.
                 (format "Error: no transition predicate defined for: %s to %s named %s in ns %s.  Try:\n(defpredicate %s %s\n  false)"
                         state-name
                         to-state-name
                         transition?-name
                         *ns*
                         state-name
                         to-state-name))))))
   (assoc attrs :transitions [])
   (:transitions attrs)))

(defmacro state [state-name & [attrs]]
  (let [attrs (merge
               {:description (format "State: %s" state-name)
                :start       false
                :stop        false}
               (or attrs {}))]
    (reduce
     #(%2 %1)
     {:name        state-name
      :start       (:start attrs)
      :stop        (:stop attrs)
      :description (:description attrs)
      :on-entry    (:on-entry attrs)
      :transitions (:transitions attrs)}
     [#(add-on-entry-trigger state-name %1)
      #(add-transitions      state-name %1)])))

(defn- split-forms [pred forms]
  (let [res (reduce (fn [m form]
                      (update-in
                       m
                       [(pred form)]
                       conj
                       form))
                    {}
                    forms)]
    [(vec (get res true))
     (vec (get res false))]))

(comment
  (split-forms
   #(and (seq? %1)
         (= 'state (first %1)))
   '[(state asdf)
     (on-transition asdf)
     (state fdsa)])

  )

(defn- select-forms [sym forms]
  (split-forms #(and (seq? %1) (= sym (first %1)))
               forms))

(defn- get-global-on-transition []
  (keywordize-fn (ns-resolve *ns* 'on-transition-any-fn!)))

(defn- get-global-on-enter []
  (keywordize-fn (ns-resolve *ns* 'on-enter-any)))

(defn- get-global-on-exit []
  (keywordize-fn (ns-resolve *ns* 'on-exit-any)))

(defmacro defmachine [workflow-name & forms]
  (let [[states forms]   (select-forms 'state forms)
        const-name       (symbol (format "*%s*" (name workflow-name)))
        states           (if (map? (first states)) (next states) states)
        states-map       (reduce (fn [states-map state]
                                   (assoc states-map (second state)
                                          state))
                                 {}
                                 states)]
    (if (not (empty? forms))
      (throw (RuntimeException. (format "Error: unreconigzed forms: %s" forms))))
    `(def ~const-name
          {:name ~workflow-name
           :on-transition ~(get-global-on-transition)
           :states        ~states-map
           :on-enter      ~(get-global-on-enter)
           :on-exit       ~(get-global-on-exit)})))


(def *workflow* nil)
(def *current-state* nil)
(def *next-state* nil)
(def *context* nil)

(defmacro on-enter-any! [& body]
  `(defn ~'on-enter-any [workflow# current-state# next-state# context#]
     (binding [*workflow*      workflow#
               *current-state* current-state#
               *next-state*    next-state#
               *context*       context#]
       (let [res# (do ~@body)]
         (if-not (map? res#)
           (throw (RuntimeException. (format "Error: on-enter-any! trigger did not return a map! Got [%s] instead."
                                             res#))))
         res#))))

(defmacro on-exit-any! [& body]
  `(defn ~'on-exit-any [workflow# current-state# next-state# context#]
     (binding [*workflow*      workflow#
               *current-state* current-state#
               *next-state*    next-state#
               *context*       context#]
       (let [res# (do ~@body)]
         (if-not (map? res#)
           (throw (RuntimeException. (format "Error: on-exit-any! trigger did not return a map! Got [%s] instead."
                          res#))))
         res#))))


(defmacro on-enter! [state-name & body]
  (let [trigger-name (symbol (format "%s-%s" (name :on-enter) (name state-name)))]
    `(defn ~trigger-name [workflow# current-state# next-state# context#]
       (binding [*workflow*      workflow#
                 *current-state* current-state#
                 *next-state*    next-state#
                 *context*       context#]
         (let [res# (do ~@body)]
           (if-not (map? res#)
             (throw (RuntimeException. (format "Error: on-enter! trigger [%s] did not return a map! Got [%s] instead."
                            ~(str trigger-name) res#))))
           res#)))))


(defmacro on-transition! [from-state to-state & body]
  (let [trigger-name (symbol (format "%s-from-%s-to-%s!"
                                     (name :on-transition)
                                     (name from-state)
                                     (name to-state)))]
    `(defn ~trigger-name [workflow# current-state# next-state# context#]
       (binding [*workflow*      workflow#
                 *current-state* current-state#
                 *next-state*    next-state#
                 *context*       context#]
         ~@body))))

(defmacro on-transition-any! [& body]
  `(defn ~'on-transition-any-fn! [workflow# current-state# next-state# context#]
     (binding [*workflow*      workflow#
               *current-state* current-state#
               *next-state*    next-state#
               *context*       context#]
       ~@body)))

(defmacro register! [workflow-name]
  `(wf/register-workflow! ~workflow-name ~(symbol (format "*%s*" (name workflow-name)))))

(defn- transition-predicate-name [from-state to-state]
  (symbol (format "transition-from-%s-to-%s?" (name from-state) (name to-state))))

(defmacro defpredicate [from-state to-state & body]
  (let [pred-name (transition-predicate-name from-state to-state)]
    `(defn ~pred-name [workflow# current-state# context#]
       (binding [*workflow*      workflow#
                 *current-state* current-state#
                 *context*       context#]
         ~@body))))

(defmacro defpredicate-as [from-state to-state alias]
  (let [pred-name (transition-predicate-name from-state to-state)]
    `(def ~pred-name
          (fn [workflow# current-state# context#]
            (binding [*workflow*      workflow#
                      *current-state* current-state#
                      *context*       context#]
              (~alias))))))

