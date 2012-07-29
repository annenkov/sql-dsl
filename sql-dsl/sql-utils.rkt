#lang racket
(require srfi/1
         srfi/13
         racket/dict)

(define (make-infix op k v)
  (format "(~a ~a ~a)" k op v))

(define (escape-value v)
  (format "'~a'" (regexp-replace* "(')" (format "~a" v) "\\\\&")))

(define (field? field entity field-in)
  (if (member (syntax->datum field) (hash-ref (eval-syntax entity) 'fields))
      (syntax->datum field)
      (raise-syntax-error field-in
                          (format "'~a' not a field of '~a'" 
                                  (syntax->datum field) 
                                  (hash-ref (eval-syntax entity) 'table)))))

(define (make-order-by field [order '()])
  (if (member order (list 'asc 'desc))
      (if order
          (format "~a ~a" field (string-upcase (symbol->string order)))
          (symbol->string field))
      (raise-syntax-error 'ORDER-BY
                          (format "Order can only be 'asc'(ascending) or 'desc' (descending), not '~a'." order))))

(define (to-sql-str tagged-clause)
  (let ([tag (car tagged-clause)]
        [clause (cdr tagged-clause)]) 
      (cond [(eq? tag 'where) (format "WHERE ~a" clause)]
        [(eq? tag 'order-by) (format "ORDER BY ~a" clause)]
        [else ""])))

(define (order-clauses tagged-clauses)
  (for/list ([tag (list 'where 'order-by)]
             #:when (dict-has-key? tagged-clauses tag))
    (cons tag (dict-ref tagged-clauses tag))))

(define (compose-select fields table . rest)
  (let ([base-form (list "SELECT" fields "FROM" table)]
        [clauses-order (list 'where 'order-by)]
        [clauses (make-hash rest)])
    (string-join (if (not (null? rest))
                     (append base-form
                             (map to-sql-str (order-clauses rest)))
                     base-form) " ")))

(define (compose-insert table fields values)
  (format "INSERT INTO ~a (~a) VALUES (~a)"
          table
          (string-join fields ", ")
          (string-join (map (lambda (x)(format "'~a'" x)) values) ", ")))

(provide (all-defined-out))