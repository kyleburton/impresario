(ns impresario.core)

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
    (if (= 1 (count viable-next-states))
      (:state (first viable-next-states))
      nil)))

(defn transition? [workflow current-state state-store]
  (loop [prev-state current-state
         next-state (transition-once? workflow current-state state-store)]
    (printf "transition? prev-state:%s next-state:%s\n" prev-state next-state)
    (if (nil? next-state)
      prev-state
      (recur next-state
             (transition-once? workflow next-state state-store)))))