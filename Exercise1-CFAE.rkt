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
      [(and (num?  x) (num? y))
    (num (+ (num-n x) (num-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define sub-num
  (lambda (x y)
     (cond
      [(and (num?  x) (num? y))
    (num (- (num-n x) (num-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define div-num
  (lambda (x y)
    (cond
      [(and (num?  x) (num? y))
    (num (/ (num-n x) (num-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
(define mul-num
  (lambda (x y)
    (cond
      [(and (num?  x) (num? y))
    (num (* (num-n x) (num-n y)))]
      [else (error "both arguments for arithmetic operation must be numbers")])))
;;define type for deferred substitution list
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?)
        (value CFAE?)
        (ds DefrdSub?))
  )

;;lookup used to look for a value in deferred substitution list
(define lookup-asub
  (lambda (sub-id ds)
    (type-case DefrdSub ds
    [mtsub() (error 'lookup "var not found in asub")]
    [aSub(name value ds) 
         (if(symbol=? sub-id name)
            value
            (lookup-asub sub-id ds))]
      )
    )
  )

(define (interp-cfae expr ds)
(type-case CFAE expr
[num (n) expr]
[add (l r) (add-num (interp-cfae l ds) (interp-cfae r ds))]
[sub (l r) (sub-num (interp-cfae l ds) (interp-cfae r ds))]
[mul (l r) (mul-num (interp-cfae l ds) (interp-cfae r ds))]
[div (l r) (div-num (interp-cfae l ds) (interp-cfae r ds))]
[id (v) (lookup-asub v ds)]
[fun (bound-id bound-body)
expr]
[app (fun-expr arg-expr)
(local ([define fun-val (interp-cfae fun-expr ds)])
(interp-cfae (fun-body fun-val)
(aSub (fun-arg-name fun-val)
      (interp-cfae arg-expr ds)
ds)))]
[if0(c t e) (local [(define cv (interp-cfae c ds))] 
    [cond 
      [(not (num? cv)) (error "test expression in if must be a number")]
      [else (if (equal? (interp-cfae c ds) (num 0)) (interp-cfae t ds) (interp-cfae e ds))]]
  )]))

(define eval-cfae
  (lambda(expr)
    (interp-cfae (parse-cfae expr) (mtsub))))
(parse-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0})
(test (eval-cfae '{+ 1 2}) (num 3))
(test (eval-cfae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfae '{{fun x x} 3}) (num 3))
(test (eval-cfae '{{fun x {+ x 1} } 1}) (num 2))
(test (eval-cfae '{if0 0 1 2}) (num 1))
(test (eval-cfae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))

(if 6 1 2)