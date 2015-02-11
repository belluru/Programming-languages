#lang plai
;;defining type for list of binding such as ((x 2) (y 4))
(define-type Binding
  (bind (id symbol?) (value WAEE?)))

(define-type WAEE
  (num (n number?))
  (binop (op symbol?) (lhs WAEE?) (rhs WAEE?))
  (with (listbinding (listof Binding?)) (body WAEE?))
  (id (name symbol?)))
  
(define-type binop-recognizer
  (binop-rec (name symbol?) (op procedure?)))

(define binop-table
  (list
   (binop-rec 'add +)
   (binop-rec 'sub -)
   (binop-rec 'mul *)
   (binop-rec 'div /)))

(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (binop-rec-name (car op-table)) op-name)
                     (binop-rec-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

(define parse-waee
  (lambda(expr)
    (cond [(number? expr) (num expr)]
          [(list? expr)
              (cond [(symbol=? (car expr) `+) (binop `add (parse-waee(cadr expr)) (parse-waee (caddr expr)))]
                    [(symbol=? (car expr) `-) (binop `sub (parse-waee(cadr expr)) (parse-waee (caddr expr)))]
                    [(symbol=? (car expr) `*) (binop `mul (parse-waee(cadr expr)) (parse-waee (caddr expr)))]
                    [(symbol=? (car expr) `/) (binop `div (parse-waee(cadr expr)) (parse-waee (caddr expr)))]
                    [(equal? (car expr) `with) (with (map create-listbinding(cadr expr)) (parse-waee(caddr expr)))]
                    [else (error `parse-waee "invalid expression")])]
          [(symbol? expr) (id expr)]
          [else (error `parse-waee "invalid expression")])))

(define create-listbinding
  (lambda(x)
    (match x
      [(list (? symbol? id) value) (bind id (parse-waee value))]
      [else (error 'create-listbinding "syntactical error in list of binding")])))


(define interp-waee
  (lambda(expr)
    (type-case WAEE expr
      (num(n) n)
      (binop(op l r) ((lookup op binop-table) (interp-waee l) (interp-waee r)))
      (with(b-list b-body) (interp-waee(substwith b-list b-body)))
      (id(name) (error name 'interp-waee "Free instance of an identifier found")))))

(define substwith
  (lambda(listbinding body)
         (cond
           [(empty? listbinding) body]
           [else (substwith (rest listbinding) (subst (bind-id(car listbinding))
                                                      (num (interp-waee(bind-value(car listbinding))))
                                                      body))]
           )
         )
  )

(define subst
  (lambda (idc value expr)
    (type-case WAEE expr
      (num(n) expr)
      (binop(op l r) (binop op (subst idc value l) (subst idc value r)))
      (with (listbinding body)
            (if(symbol=? (bind-id(car listbinding)) idc)
            (with (list(bind (bind-id(car listbinding))
                  (subst idc value (bind-value(car listbinding)))))
                  body)
             (with (list(bind (bind-id(car listbinding))
                  (subst idc value (bind-value(car listbinding)))))
                  (subst idc value body))))
      (id(name) (if(symbol=? name idc) value expr)))))


;;(interp-waee (parse-waee '(with ((x 2) (y 4)) (+ x y))))

;;(interp-waee (with (list (bind 'x (num 2))) (binop 'add (num 3) (num 1))))
;;(interp-waee (parse-waee '(with ((x 2)) (with ((x 3) (y 4)) (+ x y)))))
(substwith (list (bind 'x (num 2))) (with (list (bind 'x (num 3)) (bind 'y (num 4))) (binop 'add (id 'x) (id 'y))))
;;(substwith 
;;(interp-waee (parse-waee '(with ((x 2)) (with ((x 3) (y 4)) (+ x y)))))
(interp-waee (parse-waee '(with ((x 2) (y 3) (z 4)) (with ((x 5) (y 4)) (+ x y)))))

(define eval-waee
  (lambda(x)
    (interp-waee(parse-waee x))))
;;(eval-waee '(with ((x 2) (y 3) (z 4)) (with ((x 5) (y 4)) (+ x y))))