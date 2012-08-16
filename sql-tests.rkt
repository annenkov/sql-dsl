#lang racket
(require "sql-dsl.rkt")
(require "sql-lang.rkt")
(require "sql-utils.rkt")
(require rackunit)
(require rackunit/text-ui)
(require (for-syntax syntax/parse))

(define-entity user 
  user_table (id name)
  #:pk id)

; Строковое представление SQL для тестирования
(define-syntax (test-select stx)
  (syntax-case stx ()
    [(_ from rest ...) (select-command #'(from rest ...))]))

(define-syntax (test-insert stx)
  (syntax-case stx ()
    [(_ entity) (insert-command #'entity)]))

(define-syntax (test-expand-values stx)
  (syntax-parse stx 
    [(_ entity entity-obj (~optional (~seq #:for-update for-update) #:defaults ([for-update #'#f])))     
     (expand-values #'entity #'entity-obj #:for-update (eval-syntax #'for-update))]))

(define-syntax (test-update stx)
  (syntax-case stx ()
    [(_ entity) (update-command #'entity)]))


(define-test-suite sql-select-tests
  (check-equal? (test-select user)
                "SELECT id,name FROM user_table") 
  (check-equal? (test-select user [where (= id 1)])
                "SELECT id,name FROM user_table WHERE (id = '1')")
  (check-equal? (test-select user [where (and (> id 1) (< id 3))])
                "SELECT id,name FROM user_table WHERE ((id > '1') and (id < '3'))")
  (check-equal? (test-select user [order-by name])
                "SELECT id,name FROM user_table ORDER BY name")
  (check-equal? (test-select user [order-by name asc])
                "SELECT id,name FROM user_table ORDER BY name ASC")
  (check-equal? (test-select user [order-by name desc])
                "SELECT id,name FROM user_table ORDER BY name DESC")
  (check-equal? (test-select user [where (= id 1)] [order-by name])
                "SELECT id,name FROM user_table WHERE (id = '1') ORDER BY name")
  ; меняем местами where и order-by, результат не должен измениться
  (check-equal? (test-select user [order-by name] [where (= id 1)])
                "SELECT id,name FROM user_table WHERE (id = '1') ORDER BY name")
  
  ; не может раскрыться, т.к. id  не является выражением (должно быть (op field value), где 
  ; op - операция сравнения, field поле сущности, value значение поля)
  (check-exn exn:fail?
             (lambda () (expand #'(test-select user [where (and id 1)]))))
  
  ; id123 не является полем сущности user
  (check-exn exn:fail? 
             (lambda () (expand #'(test-select user [where (= id123 1)]))))
  (check-exn exn:fail? 
             (lambda () (expand #'(test-select user [order-by id123])))))

(define user-john
  (user 1 "John"))

(define-test-suite sql-insert-tests
  (check-equal? (test-expand-values user user-john)
                '(1 "John"))
  (check-equal? (test-insert user)
                "INSERT INTO user_table (id, name) VALUES ($1, $2)"))

(define-test-suite sql-update-tests  
  (check-equal? (test-expand-values user user-john #:for-update #t)
                '("John" 1))
  (check-equal? (test-update user)
                "UPDATE user_table SET name=$1 WHERE id=$2"))

(define (run-all)
  (begin (run-tests sql-select-tests)
         (run-tests sql-insert-tests)
         (run-tests sql-update-tests)))