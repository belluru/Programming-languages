#lang plai

(define-type F1WAEE
  (num (n number?))
  (add (lhs F1WAEE?) (rhs F1WAEE?))
  (with (id symbol?) (value F1WAEE?) (body F1WAEE?))
  (id (name symbol?))
  )


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
  
(define-type DefrdSub
  (mtsub)
  (asub (name symbol?)
        (value number?)
        (ds DefrdSub?))
  )

(define lookup-asub
  (lambda (sub-id ds)
    (type-case DefrdSub ds
    [mtsub() (error 'lookup "var not found in asub")]
    [asub(name value ds) 
         (if(symbol=? sub-id name)
            value
            (lookup-asub sub-id (rest ds)))]
      )
    )
  )
    

(define interp-f1waee
  (lambda(expr fundefs ds)
    (type-case F1WAEE expr
      (num(n) n)
      (add(l r) (+ (interp-f1waee l fundefs ds) (interp-f1waee r fundefs ds)))
      (with(b-id b-value b-body) (interp-f1waee b-body fundefs (asub b-id (interp-f1waee b-value fundefs ds) ds)))
      (id(name) (lookup-asub name ds))
    )
  )
  )

(interp-f1waee (with 'x (num 9) (add (id 'x) (id 'x))) fundefs (mtsub))
(interp-f1waee (with 'x (num 9) (add (with 'x (num 4) (id 'x)) (id 'x))) fundefs (mtsub))
