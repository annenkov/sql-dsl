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
  (check-equal? (expand-clauses #'user #'(where (= id 1)))
                "WHERE (id = '1')")
  (check-equal? (expand-clauses #'user #'(where (and (> id 1) (< id 3))))
                "WHERE ((id > '1') and (id < '3'))")
  
  ; не может раскрыться, т.к. id  не является выражением (должно быть (op field value), где 
  ; op - операция сравнения, field поле сущности, value значение поля)
  (check-exn exn:fail? 
             (lambda () (expand-clauses #'(where (and id 1)) #'user)))
  
  ; id123 не является полем сущности user
  (check-exn exn:fail? 
             (lambda () (eval-syntax (expand-clauses #'(where (= id123 1)) #'user)))))

(define-test-suite sql-select-tests
  (check-equal? (select-str user (where (= id 1)))
                "SELECT id,name FROM user_table WHERE (id = '1')"))

(run-tests where-clause-tests)
(run-tests sql-select-tests)