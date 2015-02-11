#lang plai

(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (id symbol?) (value WAE?) (body WAE?))
  (idc(name symbol?)))


(define parse-wae
  (lambda(expr)
    (cond [(number? expr) (num expr)]
          [(list? expr)
              (cond [(symbol=? (car expr) `+) (add (parse-wae(cadr expr)) (parse-wae (caddr expr)))]
                    [(symbol=? (car expr) `-) (sub (parse-wae(cadr expr)) (parse-wae (caddr expr)))]
                    [(equal? (car expr) `with) (with (car(cadr expr)) (parse-wae(cadr(cadr expr))) (parse-wae(caddr(expr))))]
                    [else (error "invalid expression")])]
          [(symbol? expr) (idc expr)]
          [else (error "invalid expression")])))

(parse-wae '(with (x 3) (+ x x)))
