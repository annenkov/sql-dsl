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

(define (print-users user-list)
  (for ([user user-list])
    (printf "Id: ~a, name: ~a\n" (user-id user) (user-name user)))) 

(define (get-user-by-id user-id)
  (select user [where (= id user-id)])) ; SELECT id,name FROM user_table WHERE (id = <user-id>)

(define (add-user id name)
  (insert user (user id name)))