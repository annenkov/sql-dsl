#lang racket
(define (make-infix op k v)
  (format "(~a ~a ~a)" k op v))

(define (escape-value v)
  (format "'~a'" (regexp-replace* "(')" (format "~a" v) "\\\\&")))

(define (is-field? field entity)
  (if (member (syntax->datum field) (hash-ref (eval-syntax entity) 'fields))
      (syntax->datum field)
      (raise-syntax-error 'WHERE
                          (format "'~a' not a field of '~a'" 
                                  (syntax->datum field) 
                                  (hash-ref (eval-syntax entity) 'table)))))

(define (compose-where expr)
  (format "WHERE ~a" expr))

(define (to-sql-str tagged-clause)
  (if (eq? (car tagged-clause) 'where)
      (compose-where (cdr tagged-clause))
      ""))

(define (compose-select fields table . rest)
  (let ([base-form (list "SELECT" fields "FROM" table)])
    (string-join (if (not (null? rest))
                     (append base-form (map to-sql-str rest))
                     base-form) " ")))

(provide (all-defined-out))