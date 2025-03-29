(defsystem "aoc-coalton"
  :depends-on ("coalton" "named-readtables")
  :components ((:file "07"))
  :in-order-to ((test-op (test-op "aoc-coalton/test"))))

(defsystem :aoc-coalton/test
  :depends-on (:aoc-coalton
               :coalton/testing 
               :fiasco)
  :pathname "test/"
  :components ((:file "test"))
  :perform (test-op (o s)
                    (symbol-call '#:aoc-coalton/test '#:run-tests)))