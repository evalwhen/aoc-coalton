(in-package :cl-user)
(defpackage :aoc-coalton-asd
  (:use :cl :asdf))
(in-package :aoc-coalton-asd)

(asdf:defsystem "aoc-coalton"
  :depends-on (:coalton :named-readtables)
  :components ((:file "parsec")
               (:file "07")
               )
  :in-order-to ((test-op (test-op "aoc-coalton/test"))))

(asdf:defsystem :aoc-coalton/test
  :depends-on (:aoc-coalton
               :coalton/testing 
               :fiasco)
  :pathname "test/"
  :components ((:file "test"))
  :perform (test-op (o s)
                    (symbol-call '#:aoc-coalton/test '#:run-tests)))
