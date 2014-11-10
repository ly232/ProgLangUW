
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream 
  (letrec ([f (lambda (x) (cons 
                           (if (= (remainder x 5) 0)
                               (- 0 x)
                               x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) (let ([nextstr (if (string=? s "dan.jpg") "dog.jpg" "dan.jpg")])
                           (cons s (lambda () (f nextstr)))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (ss) (cons (cons 0 (car (ss))) (lambda () (f (cdr (ss))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (lambda () (cons (cons (car xs) (car ys))
                   (cycle-lists
                    (append (cdr xs) (list (car xs)))
                    (append (cdr ys) (list (car ys)))))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
                (cond [(= (vector-length vec) i) #f]
                      [(not (pair? (vector-ref vec i))) (f (+ i 1))]
                      [(= (car (vector-ref vec i)) v) (vector-ref vec i)]
                      [#t (f (+ i 1))])
                )])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [victim 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([newans (assoc v xs)])
                        (if newans
                            (begin
                              (vector-set! cache victim newans)
                              (set! victim (remainder (+ victim 1) n))
                              newans)
                            #f)))))])
    f)
  )