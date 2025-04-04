(defpackage :aoc-coalton-2024-07
  (:documentation "aoc 2024 07 https://adventofcode.com/2024/day/7")
  (:shadow #:take)
  (:use
   #:coalton
   #:coalton-prelude
   #:parsec
   )
  (:local-nicknames
   (#:string #:coalton-library/string)
   (#:file #:coalton-library/file)
   )
  (:shadowing-import-from :parsec #:char)
  (:import-from
   #:coalton-library/math/integral
   #:div)
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
          (char #\:)
          (many1 (map2 (fn (_ x) x)
                       (char #\Space)
                       natural))))
  
  (declare parse-equations (Parser (List Equation)))
  (define parse-equations
    (many1
     (map2 (fn (a _) a) 
           parse-equation
           (char #\linefeed))))
  ;; Now, we can expose the functionality to the world
  (define (run-equation-parser str)
    (run-parser parse-equations (make-string-view str)))

  (define-type-alias Ops (List (Integer -> Integer -> Integer)))

  (declare evalation (Ops -> (Integer -> Boolean) -> (List Integer) -> (List Integer)))
  (define (evalation ops stop ints)
    (rec go ((xs ints))
      (match xs
        ((Cons x xs) (if (stop x)
                         (make-list)
                         (match xs
                           ((Nil) (make-list x))
                           ((Cons y zs) (>>= ops (fn (op) (go (Cons (op x y)  zs)))))))))))

  (declare check (Ops -> (List Equation) -> (List Integer)))
  (define (check ops eqs)
    (do
     (e <- eqs)
     (let (Equation a xs) = e)
      (if (any (== a)  (evalation ops (< a) xs))
          (pure a)
          (make-list))
      ))

  (declare solve (Ops -> String -> Integer))
  (define (solve ops str)
    (let ((res (run-equation-parser str)))
      (match res
        ((Ok es) (sum (check ops es)))
        ((Err err) (error err)))))

  (declare run (Ops -> Integer))
  (define (run ops)
    (match (file:read-file-to-string "input.txt")
      ((Ok str) (solve ops str))
      ((Err err) (error err))))

  (define (digits-count n)
    (rec go ((n n)
             (acc 1))
      (cond
        ((< n 10) acc)
        (True (go (div n 10) (1+ acc))))))


  ;; The concatenation operator (||) combines the digits from its left
  ;; and right inputs into a single number. For example, 12 || 345
  ;; would become 12345. All operators are still evaluated
  ;; left-to-right.
  (define (op3 x y)
    (+ (* x (^ 10 (digits-count y)))
       y))
  ) 

(cl:defun main ()
  (uiop:println "part1:")
  (uiop:println (coalton (run (make-list * +))))
  (uiop:println "part2:")
  (uiop:println (coalton (run (make-list * + op3))))
  ;; (coalton Unit)
  )
