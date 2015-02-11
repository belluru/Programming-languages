#lang plai
(define-type AE
  (num (n number?))
  (add (lhs AE?) (rhs AE?))
  (sub (lhs AE?) (rhs AE?))
  (mult (lhs AE?) (rhs AE?))
  (div (lhs AE?) (rhs AE?)))

;;(define parse
  ;;(lambda (x)
    ;;(cond ((number? x) (num x))
      ;;    ((list? x)
        ;;   (cond ((equal? (car x) '+) (add (parse (cadr x)) (parse (caddr x))))
          ;;       ((equal? (car x) '-) (sub (parse (cadr x)) (parse (caddr x)))))))))

(define interp
  (lambda (expr)
    (type-case AE expr
       (num (n) n)
       (add (l r) (+ (interp l) (interp r)))
       (sub (l r) (- (interp l) (interp r)))
       (mult(l r) (* (interp l) (interp r)))
       (div (l r) (/ (interp l) (interp r))))))
(interp (num 1))
(interp  (add (num 1) (num 5)))
(interp  (sub (num 1) (num 5)))
(interp (div (num 2) (num 7)))
(interp (add (div (num 6) (num 2)) (div (num 6) (num 4))))
(not #f)