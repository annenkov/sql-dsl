Overview
========
Simple embedded DSL for making SQL-like queries.
Now supports SELECT, INSERT, UPDATE and DELETE commands.

Working with DSL
================

Defining entities
-----------------
Define "user" entity with "id" and "name"
::
  (define-entity user
     user_table  ; DB table name
     (id name))  ; fields list

Defining connection
-------------------
Connection used implicitly to execute all commands
::
   (define-current-connection
     #:database "test"
     #:password "test" 
     #:user "test")

Making queries
--------------
Select queries has following format
::
   (select ENTITY [where CONDITIONS] [order-by FIELD asc/desc])

For example
::
   (select user))  ; SELECT id,name FROM user_table
   (select user [where (and (> id 1) (< id 3))])  ; SELECT id,name FROM user_table WHERE ((id > '1') and (id < '3'))
   (select user [order-by name asc])  ; SELECT id,name FROM user_table ORDER BY name ASC

Select returns list of user structs. User field can be accessed with standard struct selectors (user-id, user-name)
::
   (define (print-users user-list)
     (for ([user user-list])
       (printf "Id: ~a, name: ~a\n" (user-id user) (user-name user)))) 

Insert/Update
-------------
Creating user
::
   (insert user (user 1 "John Doe"))

Rename user
::
   (define (rename-user target-user new-name)
     (update user (struct-copy user target-user [name new-name])))

Delete
------
You can delete entity instance after selecting it.
::
   (define (delete-user-by-id id)
     (let ([selected-user (first (select user [where (= id id)]))])  ; assuming, that user with given id exists
       (delete user selected-user)))