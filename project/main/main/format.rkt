#lang racket
(require "vector-defination.rkt" "planet-defination.rkt"
         "render.rkt" "backgrounds.rkt"
         2htdp/image 2htdp/universe)



(define (resize a b)
   (lambda (im)
     (define a1 (/ a (image-width im)))
     (define b1 (/ b (image-height im)))
     (scale/xy a1 b1 im)))
(define resize-1080x720 (resize 1080 720))
(define base-image1 (resize-1080x720 (bitmap/file "images/star.jpg")))

(define text1 (text "play:D" 35 "white"))

(define (make-rect text)
  (define h (image-height text))
  (define w (image-width text))
  (define rect (rectangle (+ w 5) (+ h 5) 30 "violet"))
  (overlay/align "middle" "middle"
                 text rect))
(define back-button (button "Close:C" 40 "White" "purple"))
(define background1
  (place-image text1 700 400 base-image1))
(define background2
  (place-image back-button 950 50 (empty-scene 1080 720 "black") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define state 'main-menu)
(define theta 180)
(define phi 0)
;;can be main-menu, play-mode, default simlulations, transitionA-B(A fades and B appear)
(define world%
  (class object%
    (super-new)
    (init-field list-of-planets)
    (init-field point-of-view) 
    (init-field normal) 
    (init-field distance-from-screen)
    
    (define/public (show-planets)
      list-of-planets)
    (define/public (add-planet x)
      (if (list? x) (set! list-of-planets (append x list-of-planets))
          (set! list-of-planets (cons x list-of-planets))))
    (define/public (show-normal)
      normal)
    (define/public (change-angle ang x)
      (if (equal? x 'theta) (set! theta (+ theta x))
          (set! phi (+ theta x))))
    (define/public (show-point-of-view)
      point-of-view)
    (define/public (set-point-of-view a b c)
      (send point-of-view set-vector a b c))
    (define/public (show-distance-from-screen)
      distance-from-screen)
    (define/public (zoom-in)
      (set! distance-from-screen (+ 40 distance-from-screen)))
    (define/public (zoom-out)
      (set! distance-from-screen (- 40 distance-from-screen)))))

(define (update w)
  (cond [(equal? state 'default) (begin
                                   (define lst (send w show-planets))
                                   (map (lambda (planet) (send planet change-planet-state lst)) lst))]) w)


(define (press w key)
  (cond [(or (equal? state 'default))
         (cond
           [(equal? key "c") (set! state 'close) w]
           [else w])]
        [(equal? state 'main-menu)
         (cond [(equal? key "d") (set! state 'default) w])]))

(define (render w)
  (cond [(equal? state 'main-menu) background1]
        [else
         (begin
           (define lst (send w show-planets))
           (define V0 (send w show-point-of-view))
           (define n (send w show-normal))
           (define D (send w show-distance-from-screen))
           (define (f planet image)
             (define ans (planet-projection planet V0 n D)); (displayln (project-x  ans)) (displayln (project-y  ans))
             (place-image
              
              (circle; (abs (project-r ans))
              (get-field radius planet)
              "solid" (get-field color planet ))
             ; (+ 320 (project-x  ans)) (+ (project-y  ans) 240)
              (+ 540  ;(begin (display (v-x (send planet coordinates)))
                             (v-x (send planet coordinates))) (+ 390 (v-y (send planet coordinates)))
              image))
           (foldr f background2 lst))]))

(define (main x)
  (big-bang x
    (on-tick update 0.01)
    (on-key press)
    (to-draw render)
    (stop-when (lambda (y) (equal? state 'close)))))

(define world (make-object world% '() (make-vector% 2000 0 0)
                (v (* (sin (/ (* pi theta) 180)) (cos (/ (* pi phi) 180))) (* (sin (/ (* pi theta) 180)) (sin (/ (* pi phi) 180))) (cos (/ (* pi theta) 180)))
                   60))
                
(define Sun (make-object planetary-object% 100 50 (make-object vector% 0 0 0) (make-object vector% 0 0 0) (make-object vector% 0 0 0) "blue"))
(define Earth (make-object planetary-object% 1 10 (make-object vector% 100 0 0) (make-object vector% 0 30 0) (make-object vector% 0 0 0) "red"))
(define lst (list Earth Sun))
(send world add-planet lst)
(main world)