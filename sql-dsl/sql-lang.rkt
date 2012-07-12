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
    [(> k v) #`(make-infix '> '#,(is-field? #'k entity) (escape-value v))]
    [(< k v) #`(make-infix '< '#,(is-field? #'k entity) (escape-value v))]
    [(= k v) #`(make-infix '= '#,(is-field? #'k entity) (escape-value v))]))

(define (expand-where entity stx)
  (syntax-case* stx (where) symbolic-identifier=?
    [(where expr) (expand-where-clause #'expr entity)]))

(provide (all-defined-out))