(ns impresario.dsl
  (:use
   [clj-etl-utils.lang-utils :only [raise]]))


(defn- keywordize-fn [f]
  (if (nil? f)
    nil
    (keyword (format "%s/%s" (:ns   (meta f)) (:name (meta f))))))

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
  (printf "add-transitions: state-name:%s attrs:%s\n"
          state-name attrs)
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
         (raise "Error: no transition predicate defined for: %s to %s named %s in ns %s.  Try:\n(defn %s [workflow current-state context]\n  false)"
                state-name
                to-state-name
                transition?-name
                *ns*
                transition?-name))))
   (assoc attrs :transitions [])
   (:transitions attrs)))

(defmacro state [state-name & [attrs]]
  (let [attrs (merge
               {:description (format "State: %s" state-name)
                :start       false}
               (or attrs {}))]
    (reduce
     #(%2 %1)
     {:name        state-name
      :start       (:start attrs)
      :description (:description attrs)
      :on-entry    (:on-entry attrs)
      :transitions (:transitions attrs)}
     [#(add-on-entry-trigger state-name %1)
      #(add-transitions      state-name %1)])))

(defmacro defmachine [conversation-name & states]
  (let [const-name (symbol (format "*%s*" (name conversation-name)))
        states-map (reduce (fn [states-map state]
                             (assoc states-map (second state)
                                    state))
                           {}
                           states)]
    `(def ~const-name
          {:name ~conversation-name
           :states ~states-map})))