(ns impresario.test.validate-workflow
  (:use
   impresario.core
   impresario.dsl
   [clojure.test]))

(defpredicate :start :nowhere
  true)

(defmachine
  :undefined-states
  (state :start {:start true
                 :transitions [:nowhere]})
  (state :final {:stop true}))


(deftest raise-on-register-invalid-flow
  (is (thrown-with-msg? RuntimeException #"Error: :start declares a transition to :nowhere"
        (register! :undefined-states)))
  (is (thrown-with-msg? RuntimeException #"the state :final can not be reached"
        (register! :undefined-states))))

(comment

  (keys @impresario.core/*registered-workflows*)

  )