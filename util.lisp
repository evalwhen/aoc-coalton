(defpackage :util
  (:documentation "some util functions")
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (#:v #:coalton-library/vector)
   (#:s #:coalton-library/string)
   )
  (:export
   #:concatVec
   #:concatStrs
   #:split_str)
  )

(in-package :util)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare concatVec ((List (v:Vector :a)) -> (v:Vector :a)))
  (define (concatVec vs)
    (fold v:append (v:new) vs))

  (declare concatStrs ((List String) -> String))
  (define (concatStrs strs)
    (fold s:concat "" strs))

  (declare split_str (String -> String -> (List String)))
  (define (split_str str sep)
    (lisp (List String) (str sep)
      (uiop:split-string str :separator sep)))

  )
