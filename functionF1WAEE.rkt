#lang plai

(define-type F1WAEE
  (num (n number?))
  (add (lhs F1WAEE?) (rhs F1WAEE?))
  (with (id symbol?) (value F1WAEE?) (body F1WAEE?))
  (id (name symbol?))
  (app (fn-id symbol?) (fn-arg F1WAEE?)))


(define-type FunDef
  (fundef (name symbol?)
          (param symbol?)
          (body F1WAEE?)))

(define fundefs
  (list
        (fundef 'double 'x (id'n))
        (fundef 'doublecons 'x (add (id'x) (num 5)))))

(define (lookup-fundef fun-name fundefs)
(cond
[(empty? fundefs) (error fun-name "function not found")]
[else (if (symbol=? fun-name (fundef-name (first fundefs)))
(first fundefs)
(lookup-fundef fun-name (rest fundefs)))]))
  

(define interp-f1waee
  (lambda(expr fundefs)
    (type-case F1WAEE expr
      (num(n) n)
      (add(l r) (+ (interp-f1waee l fundefs) (interp-f1waee r fundefs)))
      (with(b-id b-value b-body) (interp-f1waee (subst b-id (num (interp-f1waee b-value)) b-body) fundefs))
      (id(name) (error name interp-f1waee "Free instance of an identifier found"))
      [app (fun-name arg-expr)
(local ([define the-fun-def (lookup-fundef fun-name fundefs)])
(interp-f1waee (subst 
(fundef-param the-fun-def )
(num (interp-f1waee arg-expr fundefs))
(fundef-body the-fun-def ))
fundefs))]
    )
  )
  )

(define subst
  (lambda (idc value expr)
    (type-case F1WAEE expr
      (num(n) expr)
      (add(l r) (add (subst idc value l) (subst idc value r)))
      (with (bound-id bound-value bound-body)
            (with bound-id (subst idc value bound-value)
                  (if(symbol=? bound-id idc)
                  bound-body
                  (subst idc value bound-body))))
      (id(name) (if(symbol=? name idc) value expr))
      [app (fun-name arg-expr)
(app fun-name (subst idc value arg-expr))]
      )
    )
  )

;;(subst 'double (num 2) (add (id 'x) (num 5)))
(interp-f1waee (with 'n (num 5) (app 'double (num 2))) fundefs)
