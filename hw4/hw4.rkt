
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; Problem 1
(define (sequence low high stride)
  (if (< high low)
      '()
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond
    ((< n 0) (error "list-nth-mod: negative number"))
    ((null? xs) (error "list-nth-mod: empty list"))
    (else (car (list-tail xs (remainder n (length xs)))))))

;; Problem 4
 
(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; Problem 6
(define  dan-then-dog
  (letrec ([aux (lambda (x)
                  (if (equal? x "dan.jpg")
                      (cons "dog.jpg" (lambda () (aux "dog.jpg")))
                      (cons "dan.jpg" (lambda () (aux "dan.jpg")))))])
    (lambda () (aux "dog.jpg"))))

;; Problem 7
(define (stream-add-zero s)
  (letrec ([aux (lambda (x)
                  (cons (cons 0 (car (x))) (lambda () (aux (cdr (x))))))])
    (lambda () (aux s))))

;; Problem 8
(define (list->stream lst) 
  (letrec ([l (length lst)]
           [aux (lambda (x)
                  (cons (list-ref lst (remainder x l))
                        (lambda () (aux (+ x 1)))))])
    (lambda () (aux 0))))

(define (cycle-lists xs ys)
  (letrec ([x (list->stream xs)]
           [y (list->stream ys)]
           [aux (lambda (x y)
                  (cons (cons (car (x)) (car (y)))
                        (lambda () (aux (cdr (x)) (cdr (y))))))])
    (lambda () (aux x y))))

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [aux (lambda (n)
                  (cond
                    ((= n l) #f)
                    ((not (pair? (vector-ref vec n))) (aux (+ n 1)))
                    ((equal? v (car (vector-ref vec n))) (vector-ref vec n))
                    (else (aux (+ n 1)))))])
    (aux 0)))

;; Problem 10
(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (cond
        ((vector-assoc v vec) (vector-assoc v vec))
        ((assoc v xs) (begin (vector-set! pos (assoc v xs))
                             (set! pos (remainder (+ pos 1) n))
                             (assoc v xs)))
        (else #f)))))
