#lang racket
(require "vector-defination.rkt")
(provide (all-defined-out))
(define G  1000)
(define dt 0.01)
(define e 1)

(define planetary-object%
  (class object%
    (init-field mass)
    (init-field radius)
    (init positions)
    (init velocities)
    (init forces)
    (init-field color)
    (super-new)
    (define position positions)
    (define/public (show-position)
      position)
    (define/public (show-radius)
      radius)
    (define/public (coordinates)
      (send position show-vector))
    (define velocity velocities)
    (define/public (show-velocity)
      velocity)
    (define force forces)
    (define acceleration (send force scalar-mult (/ 1 mass)))
    (define/public (show-mass)
      mass)
    (define/public (set-velocity v)
      (set! velocity v))
    (define/public (cal-force list-of-objects);list-of-objects maintain all object
      (foldr (lambda(a b)
               (if (equal? this a) b
                   (begin (if (check-collision this a)
                              (collision this a) void)
                     (let* ([obj a]
                          [position-vector (sub-vector (send this show-position) (send obj show-position))]
                          [mass-obj (send obj show-mass)]
                          [R (send position-vector magnitude)]
                          [force-magnitude (* -1 G mass mass-obj (/ 1 (expt R 3)))])
                  (if (< (- R (+ radius (get-field radius a))) (* 0.01 R)) b
                       (add-vector (send position-vector scalar-mult force-magnitude) b))))))
                   (make-object vector% 0 0 0) list-of-objects))
;      (cond [(null? list-of-objects) (make-object vector% 0 0 0)]
;            [(equal? this (car list-of-objects)) ]
;            [else (let* ([obj (car list-of-objects)]
;                         [position-vector (sub-vector (send this show-position) (send obj show-position))]
;                         [mass-obj (send obj show-mass)]
;                         [R (send position-vector magnitude)]
;                         [force-magnitude (* -1 G mass mass-obj (/ 1 (expt R 3)))])
;                    (add-vector (send position-vector scalar-mult force-magnitude) (send this cal-force (cdr list-of-objects))))
;                  ]))
    (define/public (change-planet-state list-of-objects)
      (begin
        (set! force (send this cal-force list-of-objects))
        (set! acceleration (send force scalar-mult (/ 1 mass)))
        (set! velocity (add-vector velocity (send acceleration scalar-mult dt)))
        (set! position (add-vector position (send velocity scalar-mult dt)))))))
      

(define (check-collision planet1 planet2)
  (let* ([r1 (get-field radius planet1)]
         [r2 (get-field radius planet2)]
         [r (sub-vector (send planet1 show-position) (send planet2 show-position))]
         [R (send r magnitude)]
         [v (sub-vector (send planet1 show-velocity) (send planet2 show-velocity))]
         [con (dot-product r v)])
    (if (or (>= con 0) (< (- R (+ r1 r2)) (* 0.01 R))) #f #t)))
         
    
(define (collision planet1 planet2)
  (let* ([r1 (send planet1 show-position)]
         [r2 (send planet2 show-position)]
         [v1 (send planet1 show-velocity)]
         [v2 (send planet2 show-velocity)]
         [r (sub-vector r1 r2)]
         [v1-para (vector-projection v1 r)]
         [v2-para (vector-projection v2 r)]
         [v1-per (perp v1 r)]
         [v2-per (perp v2 r)]
         [m1 (get-field mass planet1)]
         [m2 (get-field mass planet1)]
         [m (+ m1 m2)]
         [a2 (send (sub-vector  v1-para (send v2-para scalar-mult e)) scalar-mult m1)]
         [b2 (send  (add-vector v2-para (send   v1-para scalar-mult e)) scalar-mult m2)]
         [v2-new (send (add-vector a2 b2) scalar-mult (/ 1 m))]
         [a1 (send (sub-vector  v2-para (send v1-para scalar-mult e)) scalar-mult m2)]
         [b1 (send  (add-vector v1-para (send   v2-para scalar-mult e)) scalar-mult m1)]
         [v1-new (send (add-vector a1 b1) scalar-mult (/ 1 m))])
    (begin (send planet1 set-velocity (add-vector v1-per v1-new))
           (send planet2 set-velocity (add-vector v2-per v2-new)))))