(defpackage :grid
  (:documentation "a bounded grid type")
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (#:v #:coalton-library/vector)
   (#:s #:coalton-library/string)
   )
  (:export
   #:from-string
   #:to-list)
  )

(in-package :grid)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-type Pos
    (Pos UFix UFix))

  (define-struct (Grid)
    (width UFix)
    (height UFix)
    (data String))
  (declare empty-grid (Grid))
  (define empty-grid (Grid 0 0 ""))

  (declare from-string (String -> (Result String Grid)))
  (define (from-string str)
    (match (util:split_str str "\n")
      ((Nil) (Ok empty-grid))
      ((Cons row xs) (let ((width (s:length row)))
                       (rec go ((rows (make-list row))
                                (xs xs))
                         (match xs
                           ((Nil) (Ok (Grid width
                                            (length rows)
                                            (util:concatStrs (reverse rows)))))
                           ((Cons row xs) (let ((width2 (s:length row)))
                                            ;; (print (length rows))
                                            (cond
                                              ((== width2 0) (go rows nil))
                                              ((/= width2 width) (Err "row length mismatch"))
                                              (True (go (Cons row rows) xs)))
                                            ))
                           ))))))


  (declare lookup (Pos -> Grid -> (Optional Char)))
  (define (lookup (Pos x y) g)
    (match g
      ((Grid width height data) (s:ref data (+ x (* y width))))))

  (declare index (Pos -> Grid -> Char))
  (define (index p g)
    (match (lookup p g)
      ((Some v) v)
      ((None) (error "grid index out of bound"))))

  (declare to-list (Grid -> (List (Tuple Pos Char))))
  (define (to-list g)
    (do
     (let (Grid width height data) = g)
     (y <- (range 0 (1- height)))
      (x <- (range 0 (1- width)))
      (pure (Tuple  (Pos x y) (index (Pos x y) g)))))

  )
