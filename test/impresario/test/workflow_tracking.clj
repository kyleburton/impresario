(ns impresario.test.workflow-tracking
  (:require [clojure.contrib.pprint :as pp])
  (:use [impresario.core] :reload)
  (:use impresario.dsl)
  (:use [clojure.test]))

(on-enter! :start
  (assoc *context* :transitions []))

(defpredicate :start :closed
  true)

(defpredicate :open :closed
  (get *context* :close-door))

(defpredicate :closed :locked
  (get *context* :lock-door))

(on-enter! :locked
  (assoc *context* :locked? true))

(defpredicate :closed :open
  (and
   (not (get *context* :locked?))
   (not (get *context* :lock-door))))

(defpredicate :locked :closed
  (get *context* :locked?))

(on-transition! :locked :closed
  (dissoc *context* :locked?))

(on-transition-any!
  (update-in
   (dissoc *context* :close-door :lock-door :unlock-door)
   [:transitions]
   conj
   [*current-state* :=> *next-state*]))

(defmachine :door-workflow
  (state :start {:start true :transitions [:closed]})
  (state :closed {:transitions [:locked
                                :open]})
  (state :locked {:transitions [:closed]})
  (state :open   {:transitions [:closed]}))

(register-workflow :door-workflow *door-workflow*)

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

