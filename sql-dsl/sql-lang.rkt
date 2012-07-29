#lang racket
(require (for-template racket/base))
(require (planet untyped/unlib/syntax))
(require (for-template "sql-utils.rkt"))
(require "sql-utils.rkt") ; чтобы тесты работали


(define (expand-where-clause stx entity)
  (syntax-case* stx (and or like > < =) symbolic-identifier=?
    [(and a b) #`(make-infix 'and #,(expand-where-clause #'a entity) #,(expand-where-clause #'b entity))]
    [(or a b) #`(make-infix 'or #,(expand-where-clause #'a entity) #,(expand-where-clause #'b entity))]
    [(like k v) #`(make-infix 'and 'k (format "~s" v))]
    [(> k v) #`(make-infix '> '#,(field? #'k entity 'WHERE) (escape-value v))]
    [(< k v) #`(make-infix '< '#,(field? #'k entity 'WHERE) (escape-value v))]
    [(= k v) #`(make-infix '= '#,(field? #'k entity 'WHERE) (escape-value v))]))

(define (expand-clauses entity stx)
  (syntax-case* stx (where order-by) symbolic-identifier=?
    [(where expr) #`(cons 'where #,(expand-where-clause #'expr entity))]
    [(order-by field) #`(cons 'order-by '#,(field? #'field entity 'ORDER-BY))]
    [(order-by field order) #`(cons 'order-by (make-order-by '#,(field? #'field entity 'ORDER-BY) 'order))]))

(provide (all-defined-out))