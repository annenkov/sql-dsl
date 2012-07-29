#lang racket
(require "sql-dsl.rkt")
(require "sql-lang.rkt")
(require "sql-utils.rkt")
(require rackunit)
(require rackunit/text-ui)

(define-entity user 
  user_table (id name))


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
  (check-equal? (test-insert user user-john)
                "INSERT INTO user_table (id, name) VALUES ('1', 'John')"))

(define (run-all)
  (begin (run-tests sql-select-tests)
         (run-tests sql-insert-tests)))