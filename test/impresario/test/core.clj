(ns impresario.test.core
  (:use [impresario.core] :reload)
  (:use [clojure.test]))

(deftest test-validate!
  (is (thrown? IllegalArgumentException (validate! nil)))
  (is (thrown? IllegalArgumentException (validate! {})))
  (is (thrown? IllegalArgumentException (validate! {:states {:first {}}})))
  (is (thrown? IllegalArgumentException (validate! {:states
                                                    {:first  {:start true}
                                                     :second {:start true}}})))
  (is (validate! {:states {:first {:start true}}})))

;; (validate! nil)
;; (test-validate!)

(deftest test-spec-start-state
  (is (spec-start-state {:states {:first {:start true}}}))
  (is (= :first
         (spec-start-state {:states {:first {:start true}}}))))


;; (test-spec-start-state)

(def door-spec
  {:name "Front Door"
   ;; NB: what about constructor generate state?  Should we support a function that returns the initial state?
   ;; :internal-store :some.namespace/make-initial-state
   :internal-store {:locked? false
                    :key-id  "761098709817145"}
   ;; :triggers [{:type :timer {:timer-data-goes-here "1234 4312 abcd"}}
   ;;            {:type :on-wakeup :some-function]
   :states  {:open   {:start true
                      :transitions-to [{:state :closed :if :close-door?}]}
             :closed {:transitions-to [{:state :open   :if :can-open-door?}
                                       {:state :locked :if :lock-door?}]}
             :locked {:transitions-to [{:state :closed :if :unlock-door?}]}}})

;; (spec-start-states door-spec)

;; (resolve-symbols-in-spec door-spec)

(def door-workflow (make-workflow door-spec))

;; (clojure.pprint/pprint door-workflow)

(deftest test-start-state-door
  (is (= :open (current-state door-workflow))))

;; (possible-transitions-from door-workflow :open)
;; (get-in door-workflow [:definition :states :open :transitions-to])
;; door-workflow

;; (test-start-state-door)
;; (current-state-info door-workflow)

(defn close-door? [workflow]
  (printf "close-door? workflow=%s\n" workflow)
  false)

(deftest test-can-transition-door
  (is (not (can-transition? door-workflow)))
  (is (can-transition? (assoc-in door-workflow
                                 [:external-store :locked] false))))

;; (test-can-transition-door)

;; (symbol-to-combined-keyword close-door?)
;; (resolve close-door?)

;; TODO: assert that would-transition-once? does not fire triggers/observers

#_(def simple-workflow
  (make-workflow
   {:states
    {:first-state
     {:start true
      :transitions-to
      [{:state :next-state
        :if (fn [& args] true)}]}
     :next-state
     {:transitions-to
      [{:state :final-state
        :if (fn [& args] true)}]}
     :final-state}}))

#_(deftest test-would-transition-once?
  (is :next-state
      (would-transition-once?
       simple-workflow)))

  ;; TODO: assert that would-transition? does not fire triggers/observers
  ;; TODO: assert that transition-once! does fire triggers/observers
  ;; TODO: assert that transition! does fire triggers/observers