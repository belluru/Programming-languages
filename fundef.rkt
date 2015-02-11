#lang plai
(define-type F1WAEE
  (num (n number?))
  (add (lhs F1WAEE?) (rhs F1WAEE?))
  (with (id symbol?) (value F1WAEE?) (body F1WAEE?))
  (id (name symbol?))
  (app (id symbol?) (arg F1WAEE?)))

(define-type FunDef
  (fundef (name symbol?)
          (param symbol?)
          (body F1WAEE?)))

(define fundefs
  (list
        (fundef 'double 'x (add (id'x) (id'x)))
        (fundef 'doublecons 'x (add (id'x) (num 5)))))

(define (lookup-fundef fun-name fundefs)
(cond
[(empty? fundefs) (error fun-name "function not found")]
[else (if (symbol=? fun-name (fundef-name (first fundefs)))
(first fundefs)
(lookup-fundef fun-name (rest fundefs)))]))

(lookup-fundef 'doublecons fundefs)
