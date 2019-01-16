
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
    ([= n 0] (error "list-nth-mod: negative number"))
    ([null? xs] (error "list-nth-mod: empty list"))
    (true (car (list-tail xs (remainder (length xs) n))))))

;; Problem 4
(define nats
 (letrec ([f (lambda (x)
 (cons x (lambda () (f (+ x 1)))))])
 (lambda () (f 1))))
 
(define (stream-for-n-steps s n)
  (if (= n 0)
      '(lambda () '())
      (cons (car (s)) (cdr ((stream-for-n-steps s (- n 1)))))))
