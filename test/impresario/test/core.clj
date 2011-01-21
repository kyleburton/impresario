(ns impresario.test.core
  (:use [impresario.core] :reload)
  (:use [clojure.test]))

;; (def door-spec
;;   {:name "Front Door"
;;    ;; NB: what about constructor generate state?  Should we support a function that returns the initial state?
;;    ;; :internal-store :some.namespace/make-initial-state
;;    :internal-store {:locked? false
;;                     :key-id  "761098709817145"}
;;    ;; :triggers [{:type :timer {:timer-data-goes-here "1234 4312 abcd"}}
;;    ;;            {:type :on-wakeup :some-function]
;;    :states  {:open   {:start true
;;                       :transitions [{:state :closed :if :close-door?}]}
;;              :closed {:transitions [{:state :open   :if :can-open-door?}
;;                                        {:state :locked :if :lock-door?}]}
;;              :locked {:transitions [{:state :closed :if :unlock-door?}]}}})

;; TODO: assert that would-transition? does not fire triggers/observers
;; TODO: assert that transition-once! does fire triggers/observers
;; TODO: assert that transition! does fire triggers/observers

(defn transition-every-time [workflow current-state state-store]
  (printf "transition-every-time, returning true...\n")
  true)

(def simple-workflow
    {:states
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