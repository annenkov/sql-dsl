#lang racket
(require (planet ryanc/db:1:5))
(require (for-syntax (planet untyped/unlib/syntax)))
(require (planet ryanc/db:1:5/util/connect))
(require (for-syntax racket))
(require (for-syntax syntax/parse))
(require (for-syntax "sql-lang.rkt"))
(require "sql-utils.rkt")

; Вспомогательный макрос для отладки запросов. Разводачивается в функцию, compose-select
; которая возвращает SQL запрос в виде строки
(define-syntax (select-str stx)
  (syntax-case stx ()
    [(_ from where) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                                  [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(compose-select fields table #,(expand-where #'where #'from)))]
    [(_ from) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                            [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(compose-select fields table))]))
; Главный макрос. Формирует запрос и выполняет его используя текущее соединение с БД current-conn
(define-syntax (select stx)
  (syntax-case stx ()
    [(_ from where) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                                  [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(query-rows #,(datum->syntax stx 'current-conn) (compose-select fields table #,(expand-where #'where #'from))))]
    [(_ from) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                            [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(query-rows #,(datum->syntax stx 'current-conn) (compose-select fields table )))]))

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