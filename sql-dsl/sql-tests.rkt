#lang racket
(require "sql-dsl.rkt")
(require "sql-lang.rkt")
(require "sql-utils.rkt")
(require rackunit)
(require rackunit/text-ui)

; Для тестов SELECT
(define-entity user 
  user_table (id name))


(define-test-suite sql-select-tests
  (check-equal? (test-select user)
                "SELECT id,name FROM user_table") 
  (check-equal? (test-select user [where (= id 1)])
                "SELECT id,name FROM user_table WHERE (id = '1')")
  (check-equal? (test-select user [where (and (> id 1) (< id 3))])
                "SELECT id,name FROM user_table WHERE ((id > '1') and (id < '3'))")
  
  ; не может раскрыться, т.к. id  не является выражением (должно быть (op field value), где 
  ; op - операция сравнения, field поле сущности, value значение поля)
  (check-exn exn:fail? 
             (lambda () (expand '(test-select user [where (and id 1)]))))
  
  ; id123 не является полем сущности user
  (check-exn exn:fail? 
             (lambda () (expand '(test-select user [where (= id123 1)])))))

(run-tests sql-select-tests)