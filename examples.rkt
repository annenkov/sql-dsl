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
  (let ([user-in-list (select user [where (= id user-id)])])  ; SELECT id,name FROM user_table WHERE (id = <user-id>)
    (if (not (empty? user-in-list))
        (first user-in-list)
        #f)))

(define (add-user id name)
  (insert user (user id name)))

(define (rename-user target-user new-name)
  (update user (struct-copy user target-user [name new-name])))
