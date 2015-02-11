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
  (app (fun-expr CFAE?)(arg CFAE?)))

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
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?)
        (value CFAE-value?)
        (ds DefrdSub?))
  )

(define-type CFAE-value
  (numV (n number?))
  (closureV (param symbol?)
            (body CFAE?)
            (ds DefrdSub?)))

;;lookup used to look for a value in deferred substitution list
(define lookup-asub
  (lambda (sub-id ds)
    (type-case DefrdSub ds
    [mtsub () (error 'lookup "var not found in asub")]
    [aSub(name value ds1) 
         (if(symbol=? sub-id name)
            value
            (lookup-asub sub-id ds1))]
      )
    )
  )

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
[if0(c t e) (local [(define cv (interp-cfae c ds))] 
    [cond 
      [(not (numV? cv)) (error "test expression in if must be a number")]
      [else (if (equal? (interp-cfae c ds) (numV 0)) (interp-cfae t ds) (interp-cfae e ds))]]
  )]))

;;; Define an AST type for CFWAE constructs
(define-type CFWAE
  (numw (n number?))
  (addw (lhs CFWAE?) (rhs CFWAE?))
  (subw (lhs CFWAE?) (rhs CFWAE?))
  (mulw (lhs CFWAE?) (rhs CFWAE?))
  (divw (lhs CFWAE?) (rhs CFWAE?))
  (withw (name symbol?) (named-expr CFWAE?) (body CFWAE?))
  (condw (arms list?) (default CFWAE?))
  (idw (name symbol?))
  (if0w (cond CFWAE?) (tarm CFWAE?) (farm CFWAE?))
  (funw (arg-name symbol?) (body CFWAE?))
  (appw (fun-expr CFWAE?)(arg CFWAE?)))

;;; Define a parser for CFWAE constructs
(define parse-cfwae
  (lambda (expr)
    (cond ((symbol? expr) (idw expr))
          ((number? expr) (numw expr))
          ((list? expr)
           (case (car expr)
             ((-) (subw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((+) (addw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((*) (mulw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((/) (divw (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr))))
             ((if0) (if0w (parse-cfwae (cadr expr)) (parse-cfwae (caddr expr)) 
                          (parse-cfwae (cadddr expr))))
             ((fun) (funw (cadr expr) (parse-cfwae (caddr expr))))
             ((with) (withw (car (cadr expr)) 
                            (parse-cfwae (cadr (cadr expr))) 
                            (parse-cfwae (caddr expr))))
             ((cond0) (condw (parse-arms (cdr (drop-right expr 1))) (parse-cfwae (last expr))))
             (else (appw (parse-cfwae (car expr)) (parse-cfwae (cadr expr))))))
          (else 'parse-cfwae "Unexpected token"))))

;;; Utility function to parse the arms of a cond0
(define parse-arms
  (lambda (arms)
    (cond ((empty? arms) '())
          (else (cons (list (parse-cfwae (car (car arms))) (parse-cfwae (cadr (car arms))))
                       (parse-arms (cdr arms)))))))

(define prelude
  (aSub 'pi (numV 3.141592653589793) 
        (aSub 'inc (closureV 'x (add (id 'x) (num 1)) (mtsub))
             (aSub 'area (closureV 'r (mul (num 3.141592653589793) (mul (id 'r) (id 'r))) (mtsub)) (mtsub)))))


(define elab-cfwae
  (lambda(expr)
    (type-case CFWAE expr
      [numw(n) (num n)]
      [addw(l r) (add (elab-cfwae l) (elab-cfwae r))]
      [subw(l r) (sub (elab-cfwae l) (elab-cfwae r))]
      [mulw(l r) (mul (elab-cfwae l) (elab-cfwae r))]
      [divw(l r) (div (elab-cfwae l) (elab-cfwae r))]
      [withw(b-id b-value b-body) (app (fun b-id (elab-cfwae b-body)) (elab-cfwae b-value))]
      [if0w(c t e) (if0 (elab-cfwae c) (elab-cfwae t) (elab-cfwae e))]
      [funw(arg-name body) (fun arg-name (elab-cfwae body))]
      [appw(f v) (app (elab-cfwae f) (elab-cfwae v))]
      [idw(name) (id name)]
      [condw(a def) (elab-condw a (elab-cfwae def))] 
      )
    )
  )

(define elab-condw 
  (lambda (a def)
    [cond 
                        [(empty? a) def]
                        [else (if0 (elab-cfwae (car (car a))) (elab-cfwae (cadr (car a))) (elab-condw (cdr a) def))]]
    )
  )
  
(define eval-cfwae
  (lambda(expr)
    (interp-cfae (elab-cfwae (parse-cfwae expr)) prelude)))

