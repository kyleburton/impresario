(ns impresario.test.core
  (:use [impresario.core] :reload)
  (:use [clojure.test]))

(defn transition-every-time [workflow current-state state-store]
  (printf "transition-every-time, returning true...\n")
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

(deftest test-would-transition-once?
  (is (= :next-state
         (transition-once?
          simple-workflow
          :first-state
          {}))))

;; (test-would-transition-once?)

(deftest test-would-transition?
  (is (= :final-state
         (transition?
          simple-workflow
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

(defn simple-workflow-trigger [workflow curr-state next-state store]
  (printf "simple-workflow-trigger: curr-state:%s store=%s\n" curr-state store)
  (reset! *last-trigger* next-state))

(def simple-workflow-with-triggers
  {:name :simple-workflow-with-triggers
   :states
   {:first-state
    {:start true
     :on-entry :impresario.test.core/simple-workflow-trigger
     :transitions
     [{:state :next-state
       :if :impresario.test.core/transition-every-time
       :on-transition :impresario.test.core/simple-workflow-trigger}]}
    :next-state
    {:on-entry :impresario.test.core/simple-workflow-trigger
     :transitions
     [{:state :final-state
       :if :impresario.test.core/transition-every-time
       :on-transition :impresario.test.core/simple-workflow-trigger}]}
    :final-state
    {:transitions []}}})

;; *last-trigger*
;; (on-exit-triggers simple-workflow-with-triggers :first-state)
;; (on-entry-triggers simple-workflow-with-triggers :first-state)
;; (on-transition-triggers simple-workflow-with-triggers :first-state :next-state)

(deftest test-triggers-transition-once
  (let [res-state (transition-once!
                   simple-workflow-with-triggers
                   :first-state
                   {})]
    (is (= :next-state res-state))
    (is (= :next-state @*last-trigger*))
    (transition-once! simple-workflow-with-triggers
                      :next-state
                      {})
    (is (= :final-state @*last-trigger*))))

;; (test-triggers-transition-once)


;; (run-all-tests)

;; (spit "examples/simple-workflow.dot" (workflow-to-dot simple-workflow-with-triggers :start))