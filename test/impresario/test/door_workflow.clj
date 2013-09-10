(ns impresario.test.door-workflow
  (:use
   impresario.dsl
   impresario.core))

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
