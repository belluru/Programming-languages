#lang plai
;;project work for Project1/Exercise2 by Bharath Chandra Elluru ( student id: 2789552) 
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
                    [else (error `parse-waee "invalid expression or operator not found")])]
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


;;Checking for assignment for same variable twice for with eg. {with {{x 2}{x 3}}}
(define (dup-assign? ids)
  (cond [(empty? ids) false]
        [else (or (member (car ids) (cdr ids)) (dup-assign? (cdr ids)))])
  )


(define substwith
  (lambda(listbinding body)
         (cond
           [(empty? listbinding) body]
           [(dup-assign? (map bind-id listbinding)) (error 'substwith "duplicate assignment for the same variable found")]
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
            (if(member idc (map bind-id listbinding))
            (with (map (subst-in-binding idc value) listbinding)
                  body)
             (with (map (subst-in-binding idc value) listbinding)
                  (subst idc value body))))
      (id(name) (if(symbol=? name idc) value expr)))))

(define (subst-in-binding subt-id val)
  (lambda (b)
    (type-case Binding b
      [bind (id rhs) (bind id (subst subt-id val rhs))])))

(define eval-waee
  (lambda(x)
    (interp-waee(parse-waee x))))

;;-------------------------------------------------------------------------------------------------------------------------------
;; Sample test cases
(parse-waee '{with {{x 13}{y 2}} {+ x y}})
(test (eval-waee '{with {{x 13}{y 2}} {+ x y}}) 15)
(test (eval-waee '{with {{x 3} {y 5}}{with {{x 10}} {+ x y}}}) 15)
(test (eval-waee '{with {{x 3} {y 5}}{with {{x 10} {y 10}} {+ x y}}}) 20)


(test (eval-waee '{with {{y 5}}{with {{x 10} {y 20} {z 30}} {+ x {+ y z}}}}) 60)
(test (eval-waee '{with {{x 3}}{with {{x 2} {y {+ x 5}}} {+ x y}}}) 10)

(test (eval-waee '{with {{x 3} {x 5}}{with {{x 10}} {+ x y}}}) 10)
(test (eval-waee '{with {{x 2} {y {+ x 5}}}{+ x y}}) 20)
(test (eval-waee '{with {{x 3}} {$ 3 x}}) 6)