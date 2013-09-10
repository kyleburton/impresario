(ns impresario.test.dsl
  (:use [impresario.core] :reload)
  (:use
   clojure.test
   impresario.dsl))

(on-enter-any!
  (update-in *context*
             [:on-enter]
             inc))


(on-exit-any!
  (update-in *context*
             [:on-exit]
             inc))

(defpredicate :start :done
  true)

(defmachine
  :dsl-test
  (state :start {:start true
                 :transitions [:done]})
  (state :done {:stop true}))

(register-workflow :dsl-test *dsl-test*)

(deftest test-global-on-enter-and-exit
  (let [context (initialize-workflow :dsl-test  {:on-enter 0 :on-exit 0})
        [next-state context]
        (transition! :dsl-test :start context)]
    (is (= 2 (:on-enter context)))
    (is (= 1 (:on-exit context)))))

(comment
  (run-all-tests)

  )
