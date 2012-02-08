#lang racket
(require "sql-dsl.rkt")
(define-current-connection
  #:database "test"
  #:password "test" 
  #:user "test")

(define-entity user
  user_table (id name))

(define (all-users)
  (select user)) ; SELECT id,name FROM user_table

(define (get-user-by-id user-id)
  (select user (where (= id user-id)))) ; SELECT id,name FROM user_table WHERE (id = <user-id>)

