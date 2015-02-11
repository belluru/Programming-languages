#lang plai

;;; Define an AST type for CFWAES constructs.  Standard expression elements plus functions and if0 and sequence,assign.
(define-type CFWAES
  (num (n number?))
  (add (lhs CFWAES?) (rhs CFWAES?))
  (sub (lhs CFWAES?) (rhs CFWAES?))
  (mul (lhs CFWAES?) (rhs CFWAES?))
  (div (lhs CFWAES?) (rhs CFWAES?))
  (with (id symbol?) (value CFWAES?) (body CFWAES?))
  (id (name symbol?))
  (if0 (cond CFWAES?) (tarm CFWAES?) (farm CFWAES?))
  (fun (arg-name symbol?) (body CFWAES?))
  (app (fun-expr CFWAES?)(arg CFWAES?))
  (seq (e0 CFWAES?) (e1 CFWAES?))
  (assign (id symbol?) (expr CFWAES?)))


(define parse-cfwaes
  (lambda (expr)
    (cond ((symbol? expr) (id expr))
          ((number? expr) (num expr))
          ((list? expr)
           (case (car expr)
             ((-) (sub (parse-cfwaes (cadr expr)) (parse-cfwaes (caddr expr))))
             ((+) (add (parse-cfwaes (cadr expr)) (parse-cfwaes (caddr expr))))
             ((*) (mul (parse-cfwaes (cadr expr)) (parse-cfwaes (caddr expr))))
             ((/) (div (parse-cfwaes (cadr expr)) (parse-cfwaes (caddr expr))))
             ((if0) (if0 (parse-cfwaes (cadr expr)) (parse-cfwaes (caddr expr))
                         (parse-cfwaes (cadddr expr))))
             ((with) (with (car (cadr expr)) 
                            (parse-cfwaes (cadr (cadr expr))) 
                            (parse-cfwaes (caddr expr))))
             ((fun) (fun (cadr expr) (parse-cfwaes (caddr expr))))
             ((seq) (seq (parse-cfwaes (cadr expr))
                        (parse-cfwaes (caddr expr))))
           ((assign) (assign (cadr expr) (parse-cfwaes (caddr expr))))
             (else (app (parse-cfwaes (car expr)) (parse-cfwaes (cadr expr))))))
          (else parse-cfwaes "Unexpected token"))))

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
(define-type Env
  (mtsub)
  (aSub (name symbol?)
        (location number?)
        (env Env?))
  )

(define-type CFWAES-value
  (numV (n number?))
  (closureV (param symbol?)
            (body CFWAES?)
            (env Env?)))

;;lookup used to look for a value in deferred substitution list
(define lookup-asub
  (lambda (sub-id env)
    (type-case Env env
    [mtsub () (error 'lookup "var not found in asub")]
    [aSub(name loc e) 
         (if(symbol=? sub-id name)
            loc
            (lookup-asub sub-id e))]
      )
    )
  )

(define-type Store
  (mtSto)
  (aSto (location number?)
        (value CFWAES-value?)
        (sto Store?))
  )

(define lookup-store
  (lambda (loc sto)
    (type-case Store sto
    [mtSto () (error 'lookup-store "var not found at the location in store")]
    [aSto(l v s) 
         (if(= l loc)
            v
            (lookup-store loc s))]
      )
    )
  )

(define-type Value*Store
  (v*s (value CFWAES-value?) (store Store?)))

(define next-location
  (local ([define last-loc (box -1)])
          (lambda (store)
            (begin
              (set-box! last-loc (+ 1 (unbox last-loc)))
              (unbox last-loc)))))

(define (interp-cfwaes expr env sto)
(type-case CFWAES expr
[num (n) (v*s (numV n) sto)]
[add (l r) 
     (type-case Value*Store (interp-cfwaes l env sto)
       (v*s (l-val l-sto)
     (type-case Value*Store (interp-cfwaes r env l-sto)
       (v*s (r-val r-sto)
            (v*s (add-num l-val r-val) r-sto)))))]
[sub (l r) 
     (type-case Value*Store (interp-cfwaes l env sto)
       (v*s (l-val l-sto)
     (type-case Value*Store (interp-cfwaes r env l-sto)
       (v*s (r-val r-sto)
            (v*s (sub-num l-val r-val) r-sto)))))]
[mul (l r) 
     (type-case Value*Store (interp-cfwaes l env sto)
       (v*s (l-val l-sto)
     (type-case Value*Store (interp-cfwaes r env l-sto)
       (v*s (r-val r-sto)
            (v*s (mul-num l-val r-val) r-sto)))))]
[div (l r) 
     (type-case Value*Store (interp-cfwaes l env sto)
       (v*s (l-val l-sto)
     (type-case Value*Store (interp-cfwaes r env l-sto)
       (v*s (r-val r-sto)
            (v*s (div-num l-val r-val) r-sto)))))]
[id (v) (v*s (lookup-store (lookup-asub v env) sto) sto)]
[fun (bound-id bound-body)
     (v*s (closureV bound-id bound-body env) sto)]
[app (fun-expr arg-expr)
     (type-case Value*Store (interp-cfwaes fun-expr env sto)
       (v*s (funexpr-val funexpr-store)
            (type-case Value*Store (interp-cfwaes arg-expr env funexpr-store)
              (v*s (argexpr-val argexpr-store)
                  (local ((define new-loc (next-location argexpr-store)))
                    (interp-cfwaes (closureV-body funexpr-val)
                       (aSub (closureV-param funexpr-val)
                             new-loc
                             (closureV-env funexpr-val))
                       (aSto new-loc
                             argexpr-val
                             argexpr-store)))))))]
[with (name named-expr body) 
          (type-case Value*Store (interp-cfwaes named-expr env sto)
            (v*s (named-expr-value named-expr-store)
                 (local ((define new-loc (next-location named-expr-store)))
                   (interp-cfwaes body (aSub name new-loc env)
                                  (aSto new-loc named-expr-value named-expr-store)))))]
[if0(c t e) 
    (type-case Value*Store (interp-cfwaes c env sto)
           (v*s (c-value c-store)
                (if (zero? (numV-n c-value))
                    (interp-cfwaes t env c-store)
                    (interp-cfwaes e env c-store))))]
[seq (first-seq second-seq)
          (type-case Value*Store (interp-cfwaes first-seq env sto)
            (v*s (first-seq-value first-seq-store)
                 (interp-cfwaes second-seq env first-seq-store)))]
[assign (id-name value)
             (type-case Value*Store (interp-cfwaes value env sto)
               (v*s (value-value value-store)
                    (local ((define l (lookup-asub id-name env)))
                      (v*s value-value (aSto l value-value value-store)))))]
  ))

(define (eval-cfwaes expr)
  (type-case Value*Store (interp-cfwaes (parse-cfwaes expr) (mtsub) (mtSto))
    (v*s (v s)
         v)))


(test (eval-cfwaes '{with {y 0}
                       {with {inc {fun x {+ x 1}}}
                         {seq {seq {assign y {inc y}}
                                   {assign y {inc y}}}
                              {seq {assign y {inc y}}
                                   {assign y {inc y}}}}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {inc 3}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {seq {assign y 2} {inc 3}}}}) (numV 5))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {seq {assign y 2} {fun x {+ x y}}}}
                         {inc 3}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq x {assign x {+ x 1}}}}) (numV 4))

(test (eval-cfwaes '{with {x 3}
                       {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq
                        {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}
                        {assign x {+ x 1}}}}) (numV 6))