#lang racket
(require (prefix-in bag:
          (planet "bag.ss" ("soegaard" "galore.plt" 4 2))))  ; utils to work with multisets
(require rackunit)
;;;;;; Problem 2.1 ;;;;;;
; not complete - only even item count allowed
(define (min-and-max xs)
  (define (max pair)
    (let ([a (car pair)]
          [b (cadr pair)])
      (if (< a b) b
          a)))
  (define (min pair)
    (let ([a (car pair)]
          [b (cadr pair)])
      (if (> a b) b
          a)))
  
  (define (min-and-max-iter xs maybe-min maybe-max)
    (if (null? xs) (list maybe-min maybe-max)
        (let ([head (take xs 2)]
              [tail (drop xs 2)])
          (min-and-max-iter tail 
                            (if (> maybe-min (min head))
                                (min head)
                                maybe-min) 
                            (if (< maybe-max (max head)) 
                                (max head)
                                maybe-max)))))
  
  (min-and-max-iter (drop xs 2) (min (take xs 2)) (max (take xs 2))))

;;;;;; Problem 4.1 ;;;;;;

; At first, find distances between head and other points
(define (distances-with-head xs)
  (define (distances-helper xs head)
    (if (null? (cdr xs))
        '()
        (cons (abs (- head (cadr xs))) 
              (distances-helper (cdr xs) head))))
  (distances-helper xs (car xs)))

; Find all distances using distances-with-head
(define (make-all-distances xs)
  (if (null? (cdr xs))
        '()
        (append (distances-with-head xs) (make-all-distances (cdr xs)))))

; alternative version using list comprehension
(define (make-all-distances-alt xs)
  (define (distance x y)
    (abs (- x y)))
  (if (null? (cdr xs))
      '()
      (let ([head (car xs)])
        (append
         (for/list ([x (cdr xs)])(distance x head))
         (make-all-distances-alt (cdr xs))))))

; tail-recursive version of make-all-distances-alt
(define (make-all-distances-alt-tail-recur xs) 
  (define (iter xs res)
    (if (null? (cdr xs))
      res
      (let ([head (car xs)])
        (iter
         (cdr xs)
         (append res (distances (cdr xs) head))))))
  (iter xs '()))

(define (distances xs point)
   (define (distance x y)
     (abs (- x y)))
  (for/list ([x xs])
    (distance x point)))

;;;;; Problem 4.2 ;;;;;
(define (partial-digest lengths-bag)
  (let ([max-length (apply max (bag:elements lengths-bag))])
    (place (bag:remove max-length lengths-bag)
           (list 0 max-length) max-length)))

(define (place lengths-bag res width)
  (if (bag:empty? lengths-bag)
        (sort res <)
        (letrec ([bag-as-list (bag:elements lengths-bag)]
           [max-length (apply max (bag:elements lengths-bag))]
           [y-distances (distances res max-length)]
           [width-y-distances (distances res (- width max-length))])
          (cond [(bag:subbag? (bag:list->eq y-distances) lengths-bag) 
                  (with-handlers ([symbol? (lambda (x) (place (apply bag:remove* lengths-bag width-y-distances) (append res (list (- width max-length))) width))])
                    (place (apply bag:remove* lengths-bag y-distances) (append res (list max-length)) width))]
                 [(bag:subbag? (bag:list->eq width-y-distances) lengths-bag)
                  (place (apply bag:remove* lengths-bag width-y-distances) (append res (list (- width max-length))) width)]
                 [else (raise 'failed)]))))

;(check-equal? (partial-digest (bag:list->eq '(1 3 2))) '(1 2 4))