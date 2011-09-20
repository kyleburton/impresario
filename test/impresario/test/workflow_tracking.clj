(ns impresario.test.workflow-tracking
  (:require [clojure.contrib.pprint :as pp]
            impresario.test.door-workflow)
  (:use
   impresario.core
   [clojure.test]))


;; Fix for Issue: "State Tracking Bug: Serialized Contexts and Newly Added States"
(deftest state-tracking-is-robust-if-new-states-are-encountered
  (let [curr-state           (get-start-state :door-workflow)
        context              (initialize-workflow :door-workflow {})
        [curr-state context] (transition-once! :door-workflow curr-state context)
        _ (is (= :closed curr-state))

        context              (assoc context :lock-door true)
        [curr-state context] (transition-once! :door-workflow curr-state context)
        _ (is (= :locked curr-state))

        context              (assoc context :unlock-door true)
        [curr-state context] (transition-once! :door-workflow curr-state context)
        _ (is (= :closed curr-state))

        context              (assoc context :open-door true)
        [curr-state context] (transition-once! :door-workflow curr-state context)
        _ (is (= :open curr-state))

        ;; dissoc'ing :closed out of state tracking and then
        ;; transitioning to :closed will simulate :closed being added
        ;; (eg: via workflow redefinition)
        context              (update-in context [:state-tracking] dissoc :closed)
        context              (assoc context :close-door true)
        [curr-state context] (transition-once! :door-workflow curr-state context)
        _ (is (= :closed curr-state))

        ]
    [curr-state context]))

