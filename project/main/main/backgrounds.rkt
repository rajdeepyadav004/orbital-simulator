#lang racket
(require 2htdp/image)
(provide (all-defined-out))

(define (resize a b)
   (lambda (im)
     (define a1 (/ a (image-width im)))
     (define b1 (/ b (image-height im)))
     (scale/xy a1 b1 im)))
(define resize-1080x720 (resize 1080 720))

;(define base-image2 (resize-1080x720 (bitmap/file "images/base1.jpg")))
;;;;;;;;;;;

(define (button t size color1 color2)
  (define im1 (text t size color1))
  (define h (image-height im1))
  (define w (image-width im1))
  (define rect (rectangle (+ w 10) (+ h 10) 50 color2))
  (overlay/align "middle" "middle"
                 im1 rect))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define button1 (button "Defaults" 40 "white" "purple"))
(define button2 (button "Play" 40 "white" "purple"))
(define button3 (button "Sounds" 40 "white" "purple"))
(define button4 (button "About" 40 "White" "purple"))
;
(define back-button (button "Back" 40 "White" "purple"))
;
