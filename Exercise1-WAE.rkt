#lang plai
;; Project work for project1/Exercise1 by Bharath Chandra Ellur(Student id: 2789552)
(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (id symbol?) (value WAE?) (body WAE?))
  (id (name symbol?)))

(define parse-wae
  (lambda(expr)
    (cond [(number? expr) (num expr)]
          [(list? expr)
              (cond [(symbol=? (car expr) `+) (add (parse-wae(cadr expr)) (parse-wae (caddr expr)))]
                    [(symbol=? (car expr) `-) (sub (parse-wae(cadr expr)) (parse-wae (caddr expr)))]
                    [(equal? (car expr) `with) (with (car(cadr expr)) (parse-wae(cadr(cadr expr))) (parse-wae(caddr expr)))]
                    [else (error `parse-wae "invalid expression")])]
          [(symbol? expr) (id expr)]
          [else (error `parse-wae "invalid expression")])))


(define subst
  (lambda (idc value expr)
    (type-case WAE expr
      (num(n) expr)
      (add(l r) (add (subst idc value l) (subst idc value r)))
      (sub(l r) (sub (subst idc value l) (subst idc value r)))
      (with (bound-id bound-value bound-body)
            (with bound-id (subst idc value bound-value)
                  (if(symbol=? bound-id idc)
                  bound-body
                  (subst idc value bound-body))))
      (id(name) (if(symbol=? name idc) value expr)))))

(define interp-wae
  (lambda(expr)
    (type-case WAE expr
      (num(n) n)
      (add(l r) (+ (interp-wae l) (interp-wae r)))
      (sub(l r) (- (interp-wae l) (interp-wae r)))
      (with(b-id b-value b-body) (interp-wae(subst b-id (num (interp-wae b-value)) b-body)))
      (id(name) (error name 'interp-wae "Free instance of an identifier found")))))

(define eval-wae
  (lambda(x)
    (interp-wae(parse-wae x))))

;;sample test cases
(interp-wae (add (num 3) (num 4)))
(define test-wae
  (lambda (f)
    (begin
      (test (f '1) 1)
      (test (f '{+ 1 1}) 2)
      (test (f '{- 1 1}) 0)
      (test (f '{with {x 3} {+ x x}}) 6)
      (test (f '{with {x 3} {with {y 4} {+ x y}}}) 7)
      (test (f '{with {x 3} {with {y {+ x x}} {+ x y}}}) 9)
      (test (f '{with {x 3} {with {y {+ x x}} {with {x 1} {+ x y}}}}) 7))))

(test-wae eval-wae)
                 
