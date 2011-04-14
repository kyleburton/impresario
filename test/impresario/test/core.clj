(ns impresario.test.core
  (:require [clojure.contrib.pprint :as pp])
  (:use [impresario.core] :reload)
  (:use [clojure.test]))

(defn transition-every-time [workflow current-state context]
  ;;(printf "transition-every-time, returning true...\n")
  true)

(def simple-workflow
     {:name :simple-workflow
      :states
      {:first-state
       {:start true
        :transitions
        [{:state :next-state
          :if :impresario.test.core/transition-every-time}]}
       :next-state
       {:transitions
        [{:state :final-state
          :if :impresario.test.core/transition-every-time}]}
       :final-state
       {:transitions []}}})

(register-workflow! :simple-workflow simple-workflow)

;; (get-start-state simple-workflow)

(deftest test-would-transition-once?
  (is (= :next-state
         (transition-once?
          :simple-workflow
          :first-state
          {}))))

;; (test-would-transition-once?)

(deftest test-would-transition?
  (is (= :final-state
         (transition?
          :simple-workflow
          :first-state
          {}))))

;; (test-would-transition?)

;; can we step it 1 state transition at a time
;; start -> next-state -> final-state

(deftest test-error-on-multiple-vaible-states
  (let [workflow
        {:name :simple-workflow
         :states
         {:first-state   {:start true
                          :transitions
                          [{:state :second-state
                            :if :impresario.test.core/transition-every-time}
                           {:state :final-state
                            :if :impresario.test.core/transition-every-time}]}
          :second-state  {:transitions []}
          :final-state   {:transitions []}}}]
    (is (thrown?
         RuntimeException
         (transition-once? workflow :first-state {})))))

;; (test-error-on-multiple-vaible-states)

;; support triggers
(def *last-trigger* (atom nil))

(defn simple-workflow-entry-trigger [workflow curr-state next-state context]
  (reset! *last-trigger* next-state)
  (assoc context
    :times-entered (inc (:times-entered context 0))))

(defn simple-workflow-transition-trigger [workflow curr-state next-state context]
  (reset! *last-trigger* next-state)
  (assoc context
    :transitions (conj (:transitions context []) [curr-state next-state])
    :times-transitioned (inc (:times-transitioned context 0))))

(def simple-workflow-with-triggers
     {:name :simple-workflow-with-triggers
      :on-transition :impresario.core/path-tracing-trigger
      :states
      {:first-state
       {:start true
        :on-entry :impresario.test.core/simple-workflow-entry-trigger
        :transitions
        [{:state :next-state
          :if :impresario.test.core/transition-every-time
          :on-transition :impresario.test.core/simple-workflow-transition-trigger}]}
       :next-state
       {:on-entry :impresario.test.core/simple-workflow-entry-trigger
        :transitions
        [{:state :final-state
          :if :impresario.test.core/transition-every-time
          :on-transition :impresario.test.core/simple-workflow-transition-trigger}]}
       :final-state
       {:stop true
        :transitions []}}})

(register-workflow! :simple-workflow-with-triggers simple-workflow-with-triggers)

;; *last-trigger*
;; (on-exit-triggers simple-workflow-with-triggers :first-state)
;; (on-entry-triggers simple-workflow-with-triggers :first-state)
;; (on-transition-triggers simple-workflow-with-triggers :first-state :next-state)

(deftest test-initialize-workflow
  (initialize-workflow :simple-workflow-with-triggers {})
  (is (= :first-state @*last-trigger*)))

;; (test-initialize-workflow)

(deftest test-triggers-transition-once
  (let [[res-state new-context]
        (transition-once!
         :simple-workflow-with-triggers
         :first-state
         (initialize-workflow :simple-workflow-with-triggers {}))]
    (printf "new-context:\n")
    (pp/pprint new-context)
    (is (= :next-state res-state))
    (is (= :next-state @*last-trigger*))
    (let [[res-state new-context] (transition-once! :simple-workflow-with-triggers
                                                    res-state
                                                    new-context)]
      (is (= :final-state @*last-trigger*)))))

(comment
  (test-triggers-transition-once)

  )

(deftest test-uuid-is-carried-forward
  (let [initial-context (initialize-workflow :simple-workflow-with-triggers {})
        uuid            (:uuid initial-context)
        [res-state new-context]
        (transition!
         :simple-workflow-with-triggers
         :first-state
         initial-context)]
    (is (= uuid (:uuid new-context)))))


(deftest test-path-tracing-trigger
  (let [initial-context (initialize-workflow :simple-workflow-with-triggers {})
        [res-state new-context]
        (transition!
         :simple-workflow-with-triggers
         :first-state
         initial-context)]
    (is (not (empty? (:trace new-context))))
    (printf "THE FINAL CONTEXT:\n")
    (pp/pprint new-context)))


;; (test-triggers-transition-once)

                                        ;
                                        ; (run-all-tests)

;; (spit "examples/simple-workflow.dot" (workflow-to-dot simple-workflow-with-triggers :start))

(defn is-not-valid-context? [workflow current-state next-state trigger-type trigger curr-context v]
  (throw (RuntimeException. "INVALID")))

(deftest test-context-validator
  (is (thrown-with-msg? RuntimeException #"INVALID"
        (transition-once!
         {:name :simple-workflow-with-triggers
          :context-validator-fn :impresario.test.core/is-not-valid-context?
          :states
          {:first-state
           {:start true
            :on-entry :impresario.test.core/simple-workflow-entry-trigger
            :transitions
            [{:state :final-state
              :if :impresario.test.core/transition-every-time
              :on-transition :impresario.test.core/simple-workflow-transition-trigger}]}
           :final-state
           {:on-entry :impresario.test.core/simple-workflow-entry-trigger
            :transitions []}}}
         :first-state
         {}))))

(deftest test-very-limited-max-transitions
  (let [workflow {:name :simple-workflow-with-triggers
           :max-transitions 0
           :states
           {:first-state
            {:start true
             :on-entry :impresario.test.core/simple-workflow-entry-trigger
             :transitions
             [{:state :final-state
               :if :impresario.test.core/transition-every-time
               :on-transition :impresario.test.core/simple-workflow-transition-trigger}]}
            :final-state
            {:on-entry :impresario.test.core/simple-workflow-entry-trigger
             :transitions []}}}
        context (initialize-workflow workflow {})]
   (is (thrown-with-msg? RuntimeException #"Error: maximum number of iterations"
         (transition!
          workflow
          :first-state
          context)))))

;; (test-very-limited-max-transitions)
;; (test-context-validator)

(comment
  (spit "examples/simple-workflow2.dot"
        (workflow-to-dot
         {:name :simple-workflow-with-triggers
          :states
          {:first-state
           {:start true
            :on-entry :impresario.test.core/simple-workflow-trigger
            :transitions
            [{:state :next-state
              :if :impresario.test.core/transition-every-time
              :on-transition :impresario.test.core/simple-workflow-trigger}
             {:state :intermediate-state
              :if :impresario.test.core/transition-every-time
              :on-transition :impresario.test.core/simple-workflow-trigger}]}
           :intermediate-state
           {:on-entry :impresario.test.core/simple-workflow-trigger
            :transitions
            [{:state :final-state
              :if :impresario.test.core/transition-every-time
              :on-transition :impresario.test.core/simple-workflow-trigger}]}
           :next-state
           {:on-entry :impresario.test.core/simple-workflow-trigger
            :transitions
            [{:state :final-state
              :if :impresario.test.core/transition-every-time
              :on-transition :impresario.test.core/simple-workflow-trigger}]}
           :final-state
           {:transitions []}}}
         :first-state))
  )

(comment

  (run-tests)

  )


(deftest test-state-entry-count-tracking
  (let [workflow {:name :simple-counting-workflow
                  :max-transitions 0
                  :states
                  {:first-state
                   {:start true
                    :transitions
                    [{:state :final-state
                      :if :impresario.test.core/transition-every-time}]}
                   :final-state
                   {:stop true
                    :transitions []}}}
        context (initialize-workflow workflow {})]
    (is (= 1 (get-in context [:state-tracking :first-state])))
    (is (= 0 (get-in context [:state-tracking :final-state])))
    (let [[next-state context] (transition! workflow :first-state context)]
      (is (= 1 (get-in context [:state-tracking :final-state]))))))


(comment
  (test-state-entry-count-tracking)

)