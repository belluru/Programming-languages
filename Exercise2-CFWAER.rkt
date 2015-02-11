#lang plai

;;; Define an AST type for CFAE constructs.  Standard expression elements plus functions and if0.
(define-type CFAE
  (num (n number?))
  (add (lhs CFAE?) (rhs CFAE?))
  (sub (lhs CFAE?) (rhs CFAE?))
  (mul (lhs CFAE?) (rhs CFAE?))
  (div (lhs CFAE?) (rhs CFAE?))
  (id (name symbol?))
  (if0 (cond CFAE?) (tarm CFAE?) (farm CFAE?))
  (fun (arg-name symbol?) (body CFAE?))
  (app (fun-expr CFAE?)(arg CFAE?))
  (rec (name symbol?) (named-expr CFAE?) (body CFAE?)))

;;; Define a parser for CFAE constructs.  This parser does no error checking at all. Simply converts
;;; concrete syntax to AST.
(define parse-cfae
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (sub (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((+) (add (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((*) (mul (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((/) (div (parse-cfae (cadr expr)) (parse-cfae (caddr expr))))
             ((if0) (if0 (parse-cfae (cadr expr)) (parse-cfae (caddr expr))
                         (parse-cfae (cadddr expr))))
             ((fun) (fun (cadr expr) (parse-cfae (caddr expr))))
             (else (app (parse-cfae (car expr)) (parse-cfae (cadr expr))))))
          (else 'parse-cfae "Unexpected token"))))

(define add-num
  (lambda (x y)
    (cond
      [(and (numV?  x) (numV? y))
    (numV (+ (numV-n x) (numV-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define sub-num
  (lambda (x y)
     (cond
      [(and (numV?  x) (numV? y))
    (numV (- (numV-n x) (numV-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define div-num
  (lambda (x y)
    (cond
      [(and (numV?  x) (numV? y))
    (numV (/ (numV-n x) (numV-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define mul-num
  (lambda (x y)
    (cond
      [(and (numV?  x) (numV? y))
    (numV (* (numV-n x) (numV-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
;;define type for deferred substitution list


(define-type CFAER-value
  (numV (n number?))
  (closureV (param symbol?)
            (body CFAE?)
            (ds DefrdSub?)))

(define (boxed-CFAER-value? v)
(and (box? v)
(CFAER-value? (unbox v))))

(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?)
        (value CFAER-value?)
        (ds DefrdSub?))
  [aRecSub (name symbol?)
(value boxed-CFAER-value?)
(env DefrdSub?)]
  )

;;lookup used to look for a value in deferred substitution list
(define lookup-asub
  (lambda (sub-id ds)
    (type-case DefrdSub ds
    [mtsub () (error 'lookup "var not found in asub")]
    [aSub(name value ds1) 
         (if(symbol=? sub-id name)
            value
            (lookup-asub sub-id ds1))]
    [aRecSub (bound-name boxed-bound-value rest-env)
(if (symbol=? bound-name sub-id)
(unbox boxed-bound-value)
(lookup-asub sub-id rest-env))]  
      )
    )
  )
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 3}})
(numV 6))

(define (cyclically-bind-and-interp bound-id named-expr env)
(local ([define value-holder (box (numV 1729))]
[define new-env (aRecSub bound-id value-holder env)]
[define named-expr-val (interp-cfae named-expr new-env)])
(begin
(set-box! value-holder named-expr-val)
new-env)))

(define (interp-cfae expr ds)
(type-case CFAE expr
[num (n) (numV n)]
[add (l r) (add-num (interp-cfae l ds) (interp-cfae r ds))]
[sub (l r) (sub-num (interp-cfae l ds) (interp-cfae r ds))]
[mul (l r) (mul-num (interp-cfae l ds) (interp-cfae r ds))]
[div (l r) (div-num (interp-cfae l ds) (interp-cfae r ds))]
[id (v) (lookup-asub v ds)]
[fun (bound-id bound-body)
     (closureV bound-id bound-body ds)]
[app (fun-expr arg-expr)
(local ([define the-closure (interp-cfae fun-expr ds)])
(interp-cfae (closureV-body the-closure)
(aSub (closureV-param the-closure)
      (interp-cfae arg-expr ds)
(closureV-ds the-closure))))]
[rec (bound-id named-expr bound-body)
(interp-cfae bound-body
(cyclically-bind-and-interp bound-id
named-expr
ds))]
[if0(c t e) (local [(define cv (interp-cfae c ds))] 
    [cond 
      [(not (numV? cv)) (error "test expression in if must be a number")]
      [else (if (equal? (interp-cfae c ds) (numV 0)) (interp-cfae t ds) (interp-cfae e ds))]]
  )]))

;;; Define an AST type for CFWAE constructs
(define-type CFWAER
  (numw (n number?))
  (addw (lhs CFWAER?) (rhs CFWAER?))
  (subw (lhs CFWAER?) (rhs CFWAER?))
  (mulw (lhs CFWAER?) (rhs CFWAER?))
  (divw (lhs CFWAER?) (rhs CFWAER?))
  (withw (name symbol?) (named-expr CFWAER?) (body CFWAER?))
  (condw (arms list?) (default CFWAER?))
  (idw (name symbol?))
  (if0w (cond CFWAER?) (tarm CFWAER?) (farm CFWAER?))
  (funw (arg-name symbol?) (body CFWAER?))
  (appw (fun-expr CFWAER?)(arg CFWAER?))
  (recw (name symbol?) (named-expr CFWAER?) (body CFWAER?)))

;;; Define a parser for CFWAE constructs
(define parse-cfwaer
  (lambda (expr)
    (cond ((symbol? expr) (idw expr))
          ((number? expr) (numw expr))
          ((list? expr)
           (case (car expr)
             ((-) (subw (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((+) (addw (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((*) (mulw (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((/) (divw (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr))))
             ((if0) (if0w (parse-cfwaer (cadr expr)) (parse-cfwaer (caddr expr)) 
                          (parse-cfwaer (cadddr expr))))
             ((fun) (funw (cadr expr) (parse-cfwaer (caddr expr))))
             ((with) (withw (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((rec) (recw (car (cadr expr)) 
                            (parse-cfwaer (cadr (cadr expr))) 
                            (parse-cfwaer (caddr expr))))
             ((cond0) (condw (parse-arms (cdr (drop-right expr 1))) (parse-cfwaer (last expr))))
             (else (appw (parse-cfwaer (car expr)) (parse-cfwaer (cadr expr))))))
          (else 'parse-cfwaer "Unexpected token"))))

;;; Utility function to parse the arms of a cond0
(define parse-arms
  (lambda (arms)
    (cond ((empty? arms) '())
          (else (cons (list (parse-cfwaer (car (car arms))) (parse-cfwaer (cadr (car arms))))
                       (parse-arms (cdr arms)))))))

(define prelude
  (aSub 'pi (numV 3.141592653589793) 
        (aSub 'inc (closureV 'x (add (id 'x) (num 1)) (mtsub))
             (aSub 'area (closureV 'r (mul (num 3.141592653589793) (mul (id 'r) (id 'r))) (mtsub)) (mtsub)))))


(define elab-cfwaer
  (lambda(expr)
    (type-case CFWAER expr
      [numw(n) (num n)]
      [addw(l r) (add (elab-cfwaer l) (elab-cfwaer r))]
      [subw(l r) (sub (elab-cfwaer l) (elab-cfwaer r))]
      [mulw(l r) (mul (elab-cfwaer l) (elab-cfwaer r))]
      [divw(l r) (div (elab-cfwaer l) (elab-cfwaer r))]
      [withw(b-id b-value b-body) (app (fun b-id (elab-cfwaer b-body)) (elab-cfwaer b-value))]
      [recw(b-id b-value b-body) (rec b-id  (elab-cfwaer b-value) (elab-cfwaer b-body))]
      [if0w(c t e) (if0 (elab-cfwaer c) (elab-cfwaer t) (elab-cfwaer e))]
      [funw(arg-name body) (fun arg-name (elab-cfwaer body))]
      [appw(f v) (app (elab-cfwaer f) (elab-cfwaer v))]
      [idw(name) (id name)]
      [condw(a def) (elab-condw a (elab-cfwaer def))] 
      )
    )
  )

(define elab-condw 
  (lambda (a def)
    [cond 
                        [(empty? a) def]
                        [else (if0 (elab-cfwaer (car (car a))) (elab-cfwaer (cadr (car a))) (elab-condw (cdr a) def))]]
    )
  )
  
(define eval-cfwaer
  (lambda(expr)
    (interp-cfae (elab-cfwaer (parse-cfwaer expr)) prelude)))


(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{with {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 0}})
(numV 1))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 3}})
(numV 6))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 5}})
(numV 120))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 1} 1}})
(numV 3))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 2} 2}})
(numV 7))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 3}})
(numV 61))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 0} 3}})
(numV 4))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 0}})
(numV 5))
(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {rec {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {rec {f {fun x {+ y x}}} {rec {y 100} {f 3}}}}) (numV 4))