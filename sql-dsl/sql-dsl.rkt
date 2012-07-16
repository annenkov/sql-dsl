#lang racket
(require (planet ryanc/db:1:5))
(require (for-syntax (planet untyped/unlib/syntax)))
(require (planet ryanc/db:1:5/util/connect))
(require (for-syntax racket))
(require (for-syntax syntax/parse))
(require (for-syntax "sql-lang.rkt"))
(require "sql-utils.rkt")

; Формирует SQL-команду в виде строки
(define-for-syntax (select-str stx)
  (syntax-case stx ()
    [(from rest ...) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                                  [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(compose-select fields table #,@(map (curry expand-clauses #'from) (syntax->list #'(rest ...)))))]))

; Выполняет запрос, используя текущее соединение с БД current-conn
(define-syntax (select* stx)
  (syntax-case stx ()
    [(_ from rest ...) #`(query-rows #,(datum->syntax stx 'current-conn) #,(select-str #'(from rest ...)))]))

; строковое представление SQL для тестирования
(define-syntax (test-select stx)
  (syntax-case stx ()
    [(_ from rest ...) (select-str #'(from rest ...))]))

(define-syntax (define-entity stx)
  (syntax-case stx ()
    [(_ entity table* fields*) #'(define-for-syntax entity (hash 'table 'table*
                                                                 'fields 'fields*))]))
(define-syntax (define-entity-for-test stx)
  (syntax-case stx ()
    [(_ entity table* fields*) #'(define entity (hash 'table 'table*
                                                      'fields 'fields*))]))

(define-syntax (define-current-connection stx)
  (syntax-parse stx
    [(_ (~seq kw:keyword v:expr) ...)
     #`(define #,(datum->syntax stx 'current-conn) (virtual-connection 
                             (lambda ()(keyword-apply postgresql-connect '(kw ...) '(v ...) '()))))]))

(provide (all-defined-out))