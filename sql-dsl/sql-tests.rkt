#lang racket
(require "sql-dsl.rkt")
(require "sql-lang.rkt")
(require "sql-utils.rkt")
(require rackunit)
(require rackunit/text-ui)

; Для тестов WHERE
(define-entity-for-test user 
  user_table (id name))

; Для тестов SELECT
(define-entity user 
  user_table (id name))

(define-test-suite where-clause-tests
  (check-equal? (eval-syntax (expand-where #'(where (= id 1)) #'user))
                "(id = '1')")
  (check-equal? (eval-syntax (expand-where #'(where (and (> id 1) (< id 3))) #'user))
                "((id > '1') and (id < '3'))")
  
  ; не может раскрыться, т.к. id  не является выражением (должно быть (op field value), где 
  ; op - операция сравнения, field поле сущности, value значение поля)
  (check-exn exn:fail? 
             (lambda () (expand-where #'(where (and id 1)) #'user)))
  
  ; id123 не является полем сущности user
  (check-exn exn:fail? 
             (lambda () (eval-syntax (expand-where #'(where (= id123 1)) #'user)))))

(define-test-suite sql-select-tests
  (check-equal? (select-str user)
                "SELECT id,name FROM user_table"))

(run-tests where-clause-tests)
(run-tests sql-select-tests)