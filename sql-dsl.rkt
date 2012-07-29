#lang racket
(require (planet ryanc/db:1:5))
(require (for-syntax (planet untyped/unlib/syntax)))
(require (planet ryanc/db:1:5/util/connect))
(require (for-syntax racket))
(require (for-syntax syntax/parse))
(require (for-syntax "sql-lang.rkt"))
(require "sql-utils.rkt")

; Формирует SQL-команду в виде строки
(define-for-syntax (select-command stx)
  (syntax-case stx ()
    [(from rest ...) (with-syntax ([table (symbol->string (hash-ref (eval-syntax #'from) 'table))]
                                  [fields (string-join (map symbol->string (hash-ref (eval-syntax #'from) 'fields)) ",")])
                      #`(compose-select fields table #,@(map (curry expand-clauses #'from) (syntax->list #'(rest ...)))))]))

; Выполняет запрос, используя текущее соединение с БД current-conn
(define-syntax (select stx)
  (syntax-case stx ()
    [(_ from rest ...) #`(rows->entity-list 
                          (query-rows #,(datum->syntax stx 'current-conn)
                                      #,(select-command #'(from rest ...)))
                          from)]))

; Строковое представление SQL для тестирования
(define-syntax (test-select stx)
  (syntax-case stx ()
    [(_ from rest ...) (select-command #'(from rest ...))]))

(define (rows->entity-list rows entity)
  (for/list ([row rows])
    (apply entity (vector->list row))))

(define-syntax (define-entity stx)
  (syntax-case stx ()
    [(_ entity table* fields*) #'(begin 
                                   (define-for-syntax entity (hash 'table 'table*
                                                                   'fields 'fields*))
                                   (struct entity 
                                     fields*))]))

(define-for-syntax (build-selector entity field-name)
  (string->symbol (format "~a-~a" (syntax->datum entity) field-name)))

(define-for-syntax (expand-values-for-insert entity-stx entity-obj)
  (with-syntax ([selectors (map (lambda (x) (build-selector entity-stx x)) (hash-ref (eval-syntax entity-stx) 'fields))])
    #`(map (lambda (x) (format "~a" ((eval x) #,entity-obj))) 'selectors)))


(define-for-syntax (insert-command entity entity-obj)
  (syntax-case entity ()
    [entity (with-syntax ([fields (hash-ref (eval-syntax #'entity) 'fields)]
                                         [table (hash-ref (eval-syntax #'entity) 'table)])
                             #`(compose-insert 'table 
                                               (map symbol->string 'fields) 
                                               #,(expand-values-for-insert #'entity entity-obj)))]))

(define-syntax (test-insert stx)
  (syntax-case stx ()
    [(_ entity entity-obj) (insert-command #'entity #'entity-obj)]))

(define-syntax (insert stx)
  (syntax-case stx ()
    [(_ entity entity-obj) #`(query-exec 
                              #,(datum->syntax stx 'current-conn) 
                              #,(insert-command #'entity #'entity-obj))]))

(define-syntax (define-current-connection stx)
  (syntax-parse stx
    [(_ (~seq kw:keyword v:expr) ...)
     #`(define #,(datum->syntax stx 'current-conn) (virtual-connection 
                             (lambda ()(keyword-apply postgresql-connect '(kw ...) '(v ...) '()))))]))

(provide (all-defined-out))