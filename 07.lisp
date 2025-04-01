(defpackage :aoc-coalton-2024-07
  (:documentation "aoc 2024 07")
  (:shadow #:take #:char)
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:string #:coalton-library/string))
  (:import-from
   #:coalton-library/functions
   #:asum)
  (:export
   #:always-returns-zero
   #:one-element-list))

(cl:in-package :aoc-coalton-2024-07)

(named-readtables:in-readtable coalton:coalton)

;; Opaque type for string views
(coalton-toplevel
  (repr :native cl:string)
  (define-type StringView)

  (declare make-string-view (String -> StringView))
  (define (make-string-view str)
    (lisp StringView (str)
      (cl:make-array (cl:length (cl:the (cl:vector cl:character) str))
                     :element-type 'cl:character
                     :displaced-to str
                     :displaced-index-offset 0)))

  (declare next-char (StringView -> (Optional (Tuple coalton:Char StringView))))
  (define (next-char str)
    (lisp (Optional (Tuple coalton:Char StringView)) (str)
      (cl:let* ((arr str))
        (cl:declare (cl:type (cl:vector cl:character) arr)
                    ;; Muffle sbcl wanting to optimize aref. This cannot be optimized.
                    #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
        (cl:multiple-value-bind
              (displaced-to displaced-index-offset)
            (cl:array-displacement arr)
          (cl:if (cl:= 0 (cl:length arr))
                 None
                 (Some (Tuple (cl:aref arr 0)
                              (cl:if displaced-to
                                     (cl:make-array (cl:1- (cl:length arr))
                                                    :element-type 'cl:character
                                                    :displaced-to displaced-to
                                                    :displaced-index-offset (cl:1+ displaced-index-offset))
                                     (cl:make-array (cl:length arr)
                                                    :element-type 'cl:character
                                                    :displaced-to arr
                                                    :displaced-index-offset (cl:1+ displaced-index-offset))))))))))

  (declare string-view-get (StringView -> String))
  (define (string-view-get str)
    (lisp String (str) str))

  (declare string-view-empty-p (StringView -> Boolean))
  (define (string-view-empty-p str)
    (lisp Boolean (str)
      (cl:let* ((arr str))
        (cl:declare (cl:type (cl:vector cl:character) arr))
        (cl:if (cl:= 0 (cl:length arr))
               True
               False))))

  )

;; Parser Type and basic parsers
(coalton-toplevel
  (define-type ParseError
    (ParseError String)
    (Context String ParseError))

  (define parse-error-eof (ParseError "EOF"))

  (declare incomplete-parse-error (String -> ParseError))
  (define (incomplete-parse-error str)
    (ParseError (lisp String (str) (cl:format cl:nil "Parser did not complete: ~A" str))))

  (define-type (Parser :a)
    (Parser (StringView -> (Result ParseError (Tuple :a StringView)))))

  (declare get-parser ((Parser :a) -> StringView -> (Result ParseError (Tuple :a StringView))))
  (define (get-parser p)
    (match p
      ((Parser x) x)))

  (declare run-parser ((Parser :a) -> StringView -> (Result ParseError :a)))
  (define (run-parser p_ str)
    ;; Unwrap Parser function
    (let ((p (get-parser p_)))
      (match (p str)
        ((Err e) (Err e))
        ((Ok (Tuple a str))
         (if (string-view-empty-p str)
             (Ok a)
             (Err (incomplete-parse-error (string-view-get str))))))))

  ;;
  ;; Error tracking
  ;;
  
  (declare with-context (String -> (Parser :a) -> (Parser :a)))
  (define (with-context s p)
    (map-error (Context s) p))

  (declare map-error ((ParseError -> ParseError) -> (Parser :a) -> (Parser :a)))
  (define (map-error f p_)
    (let ((p (get-parser p_)))
      (Parser
       (fn (str)
         (match (p str)
           ((Err e) (Err (f e)))
           ((Ok (Tuple a str))
            (Ok (Tuple a str))))))))

  ;;
  ;; Some basic parsers
  ;;

  (declare const-value (:a -> (Parser :a)))
  (define (const-value x)
    (Parser
     (fn (str)
       (Ok (Tuple x str)))))
  ;;
  ;; Define some basic value parsers
  ;;

  (declare eof (Parser Unit))
  (define eof
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some (Tuple read-char _)) (Err (ParseError (lisp String (read-char)
                                                        (cl:format cl:nil "Unexpected character '~A' expected EOF" read-char)))))
         ((None) (Ok (Tuple Unit str)))))))

  (declare take (Parser coalton:Char))
  (define take
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (Ok t_))
         ((None) (Err parse-error-eof))))))

  (declare char (coalton:Char -> (Parser coalton:Char)))
  (define (char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Ok t_)
                (Err (ParseError (lisp String (read-char c) (cl:format cl:nil "Unexpected character '~A' expected '~A'" read-char c)))))))
         ((None) (Err parse-error-eof))))))

  (declare not-char (coalton:Char -> (Parser coalton:Char)))
  (define (not-char c)
    (Parser
     (fn (str)
       (match (next-char str)
         ((Some t_)
          (let ((read-char (fst t_)))
            (if (== c read-char)
                (Err (ParseError (lisp String (read-char c) (cl:format cl:nil "Unexpected character '~A' expected not '~A'" read-char c))))
                (Ok t_))))
         ((None) (Err parse-error-eof))))))

  (declare parse-string (StringView -> (Parser StringView)))
  (define (parse-string str)
    (let ((f (fn (s)
               (match (next-char s)
                 ((Some (Tuple c s))
                  (>>= (char c) (fn (_) (f s))))
                 ((None) (const-value str))))))
      (f str)))

  (declare whitespace (Parser Unit))
  (define whitespace
    (map (fn (_) Unit)
         (alt (char #\Space)
              (char #\Return))))

  (declare digit (Parser coalton:Char))
  (define digit
    (map-error
     (fn (_) (ParseError "Invalid digit"))
     (verify
      (fn (x) (and (>= x #\0)
                   (<= x #\9)))
      take)))

  (declare lowercase (Parser coalton:Char))
  (define lowercase
    (map-error
     (fn (_) (ParseError "Invalid lowercase character"))
     (verify
      (fn (x) (and (>= x #\a)
                   (<= x #\z)))
      take)))

  (declare uppercase (Parser coalton:Char))
  (define uppercase
    (map-error
     (fn (_) (ParseError "Invalid uppercase character"))
     (verify
      (fn (x) (and (>= x #\A)
                   (<= x #\Z)))
      take)))

  (declare alpha (Parser coalton:Char))
  (define alpha (alt lowercase uppercase))

  (declare alphanumeric (Parser coalton:Char))
  (define alphanumeric (alt alpha digit))

  (declare natural (Parser Integer))
  (define natural
    (with-context "While parsing natural number"
      (>>= (map string:parse-int (map into (many1 digit)))
           (fn (i)
             (match i
               ((Some a) (const-value a))
               ((None) (fail "Invalid integer")))))))

  ;;
  ;; Parser combinators
  ;;
  (declare many0 ((Parser :a) -> (Parser (List :a))))
  (define (many0 p_)
    (let ((p (get-parser p_))
          (f (fn (str)
               (match (p str)
                 ((Err _) (Tuple Nil str))
                 ((Ok (Tuple a str))
                  (match (f str)
                    ((Tuple b str) (Tuple (Cons a b) str))))))))
      (Parser
       (fn (str) (Ok (f str))))))

  (declare many1 ((Parser :a) -> (Parser (List :a))))
  (define (many1 p)
    (>>= p (fn (a) (map (Cons a) (many0 p)))))

  (declare option ((Parser :a) -> (Parser (Optional :a))))
  (define (option p_)
    (let ((p (get-parser p_)))
      (Parser
       (fn (str)
         (match (p str)
           ((Ok (Tuple a str)) (Ok (Tuple (Some a) str)))
           ((Err _) (Ok (Tuple None str))))))))

  (declare verify ((:a -> Boolean) -> ((Parser :a) -> (Parser :a))))
  (define (verify f p)
    (>>= p
         (fn (x)
           (match (f x)
             ((True) (const-value x))
             ((False) (fail "Validation failed"))))))

  (define-instance (Functor Parser)
    (define (map f p_)
      (let ((p (get-parser p_)))
        (Parser
         (fn (str)
           (match (p str)
             ((Err e) (Err e))
             ((Ok (Tuple a str))
              (Ok (Tuple (f a) str)))))))))

  (define-instance (Applicative Parser)
    (define pure const-value) 
    (define (liftA2 f a b)
      (do
          (a <- a)
          (b <- b)
        (pure (f a b)))))

  (define-instance (Monad Parser)
    (define (>>= p_ f)
      (let ((p (get-parser p_)))
        (Parser
         (fn (str)
           (match (p str)
             ((Err e) (Err e))
             ((Ok (Tuple a str))
              ((get-parser (f a)) str))))))))

  (define-instance (MonadFail Parser)
    (define (fail s)
      (Parser
       (fn (str)
         (Err (ParseError s))))))

  (define-instance (Alternative Parser)
    (define (alt p1_ p2_)
      (let ((p1 (get-parser p1_))
            (p2 (get-parser p2_)))
        (Parser
         (fn (str)
           (match (p1 str)
             ((Ok (Tuple a str))
              (Ok (Tuple a str)))
             ((Err _)
              (match (p2 str)
                ((Err e) (Err e))
                ((Ok (Tuple b str))
                 (Ok (Tuple b str))))))))))
    (define empty (fail "alt")))

  (declare map2 ((:a -> (:b -> :c)) -> ((Parser :a) -> ((Parser :b) -> (Parser :c)))))
  (define map2 liftA2)

  (declare map3 ((:a -> (:b -> (:c -> :d))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> (Parser :d))))))
  (define (map3 f p1_ p2_ p3_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (Ok (Tuple (f a b c) str))))))))))))

  (declare map4 ((:a -> (:b -> (:c -> (:d -> :e)))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> ((Parser :d) -> (Parser :e)))))))
  (define (map4 f p1_ p2_ p3_ p4_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_))
          (p4 (get-parser p4_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (match (p4 str)
                    ((Err e) (Err e))
                    ((Ok (Tuple d str))
                     (Ok (Tuple (f a b c d) str))))))))))))))

  (declare map5 ((:a -> (:b -> (:c -> (:d -> (:e -> :f))))) -> ((Parser :a) -> ((Parser :b) -> ((Parser :c) -> ((Parser :d) -> ((Parser :e) -> (Parser :f))))))))
  (define (map5 f p1_ p2_ p3_ p4_ p5_)
    (let ((p1 (get-parser p1_))
          (p2 (get-parser p2_))
          (p3 (get-parser p3_))
          (p4 (get-parser p4_))
          (p5 (get-parser p5_)))
      (Parser
       (fn (str)
         (match (p1 str)
           ((Err e) (Err e))
           ((Ok (Tuple a str))
            (match (p2 str)
              ((Err e) (Err e))
              ((Ok (Tuple b str))
               (match (p3 str)
                 ((Err e) (Err e))
                 ((Ok (Tuple c str))
                  (match (p4 str)
                    ((Err e) (Err e))
                    ((Ok (Tuple d str))
                     (match (p5 str)
                       ((Err e) (Err e))
                       ((Ok (Tuple e str))
                        (Ok (Tuple (f a b c d e) str))))))))))))))))
  )

(coalton-toplevel
  )
 
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
           (char #\Newline))))

  )