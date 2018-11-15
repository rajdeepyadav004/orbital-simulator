#lang racket
(provide (all-defined-out))
(struct v(x y z) #:transparent)
(define vector%
  (class object%
    (init x-coordinate)
    (init y-coordinate)
    (init z-coordinate)
    (define x x-coordinate)
    (define y y-coordinate)
    (define z z-coordinate)
    (super-new)
    (define vec-list (list x y z))
    (define/public (show-vector)
      (v x y z))
    (define/public (show-vec-list)
      vec-list)
    (define/public (set-vector a b c)
      (cond [(and (number? a)
                  (number? b)
                  (number? c))
             (set! x a)
             (set! y b)
             (set! z c)]
            [(and (number? a)
                  (number? b))
             (set! x a)
             (set! y b)]
            [(and (number? a)
                  (number? c))
             (set! x a)
             (set! z c)]
            [(and (number? b)
                  (number? c))
             (set! y b)
             (set! z c)]
            [(number? a) (set! x a)]
            [(number? b) (set! y b)]
            [(number? c) (set! z c)]))
    (define/public (scalar-mult k)
      (make-object vector% (* k x) (* k y) (* k z)))
    (define/public (magnitude)
      (sqrt (+ (sqr x) (sqr y) (sqr z))))
    (define/public (make-unit)
      (if (equal? 0 (send this magnitude)) this
      (send this scalar-mult (/ 1 (send this magnitude)))))))


(define (vec?0 v)
  (equal? v (make-vector% 0 0 0)))
        
(define (add-vector vec-1 vec-2)
  (define lst-1 (send vec-1 show-vec-list))
  (define lst-2 (send vec-2 show-vec-list))
  (make-object vector% (+ (car lst-1) (car lst-2)) (+ (cadr lst-1) (cadr lst-2)) (+ (caddr lst-1) (caddr lst-2))))
(define (sub-vector vec-1 vec-2)
  (define lst-1 (send vec-1 show-vec-list))
  (define lst-2 (send vec-2 show-vec-list))
  (make-object vector% (- (car lst-1) (car lst-2)) (- (cadr lst-1) (cadr lst-2)) (- (caddr lst-1) (caddr lst-2))))
(define (distance vec-1 vec-2)
  (define v1 (send vec-1 show-vector))
  (define v2 (send vec-2 show-vector))
  (sqrt (+ (sqr (- (v-x v1) (v-x v2))) (sqr (- (v-y v1) (v-y v2))) (sqr (- (v-z v1) (v-z v2))))))
(define (make-vector% a b c)
  (make-object vector% a b c))
(define (dot-product vector-1 vector-2)
  (let* ([v1 (send (send vector-1 make-unit) show-vector)]
         [v2 (send (send vector-2 make-unit) show-vector)]
         [x1 (v-x v1)] [y1 (v-y v1)] [z1 (v-z v1)]
         [x2 (v-x v2)] [y2 (v-y v2)] [z2 (v-z v2)])
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))
         
(define (vector-projection vector1 vector2);projection of 1 on 2
  (send (send vector2 make-unit) scalar-mult  (dot-product vector1 vector2)))
(define (perp vector1 vector2);perpendicular component of 1 on 2
  (sub-vector vector1 (send (send vector2 make-unit) scalar-mult (dot-product vector1 vector2))))
  
                     