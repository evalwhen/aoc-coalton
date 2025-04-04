(defpackage :aoc-coalton-2024-07
  (:documentation "aoc 2024 07")
  (:shadow #:take #:char)
  (:use
   #:coalton
   #:coalton-prelude
   #:parsec
   )
  (:local-nicknames
   (#:string #:coalton-library/string)
   (#:file #:coalton-library/file)
   ;; (#:parsec #:simple-parsec)
   )
  (:import-from
   #:coalton-library/functions
   #:asum)
  (:export
   #:run-equation-parser
   #:main
   ))

(cl:in-package :aoc-coalton-2024-07)
(named-readtables:in-readtable coalton:coalton)

;; 2024 aoc 07 day
(coalton-toplevel
  (define-type Equation
    (Equation Integer (List Integer)))
  
  (declare parse-equation (Parser Equation))
  (define parse-equation
    (map3 (fn (a b_ c) (Equation a c))
          natural
          (parsec:char #\:)
          (many1 (map2 (fn (_ x) x)
                       (parsec:char #\Space)
                       natural))))
  
  (declare parse-equations (Parser (List Equation)))
  (define parse-equations
    (many1
     (map2 (fn (a _) a) 
           parse-equation
           (parsec:char #\linefeed))))
  ;; Now, we can expose the functionality to the world
  (define (run-equation-parser str)
    (run-parser parse-equations (make-string-view str)))

  (declare evalation ((List (Integer -> Integer -> Integer)) -> (Integer -> Boolean) -> (List Integer) -> (List Integer)))
  (define (evalation ops stop ints)
    (rec go ((xs ints))
      (match xs
        ((Cons x xs) (if (stop x)
                         (make-list)
                         (match xs
                           ((Nil) (make-list x))
                           ((Cons y zs) (>>= ops (fn (op) (go (Cons (op x y)  zs)))))))))))

  (declare check ((List (Integer -> Integer -> Integer)) -> (List Equation) -> (List Integer)))
  (define (check ops eqs)
    (do
     (e <- eqs)
     (let (Equation a xs) = e)
      (if (any (== a)  (evalation ops (< a) xs))
          (pure a)
          (make-list))
      ))

  (declare solve (String -> Integer))
  (define (solve str)
    (let ((res (run-equation-parser str)))
      (match res
        ((Ok es) (sum (check (make-list * +) es)))
        ((Err err) (error err)))))

  (declare run (Unit -> Integer))
  (define (run)
    (match (file:read-file-to-string "input.txt")
      ((Ok str) (solve str))
      ((Err err) (error err))))
  ) 

(cl:defun main ()
  (uiop:println "part1:")
  (uiop:println (run Unit))
  (uiop:println "part2:")
  (uiop:println "TODO")
  ;; (coalton Unit)
  )
