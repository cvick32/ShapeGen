;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab12InlaidCircles) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; A Ball is a (make-ball Nat Mode Color [Nat -> Posn])
(define-struct ball (r mode color placement))
; - where r is the ball's radius
; - mode is the ball's mode
; - color is the ball's color
; - and placement is a function that, given the current time,
;   outputs a new coordinate for the ball to be drawn at

; A Mode is one of:
; - 'solid
; - 'outline

(define HEIGHT 800)
(define WIDTH  800)
(define BALL-1 (make-ball 5 'solid 'red (λ (t) (make-posn 20 (modulo t HEIGHT)))))
(define BALL-2 (make-ball 7 'outline 'blue (λ (t) (make-posn (modulo t WIDTH) 100))))
(define MTSCENE (empty-scene HEIGHT WIDTH))
; ball-temp : Ball -> ???
(define (ball-temp b)
  (... (ball-r b) ... (mode-temp (ball-mode b)) ...
       (ball-color b) ... (ball-placement b) ...))

; mode-temp : Mode -> ???
(define (mode-temp m)
  (... (cond [(symbol=? m 'solid) ...]
             [(symbol=? m 'outline) ...]) ...))

; A World is a (make-world Nat [List-of Ball])
(define-struct world (t balls radius))
; - where t is the amount of time that has passed
; - and balls is the balls of the world
; - determines the radius of the ball

(define WORLD-1 (make-world 0 '() 1000))
(define WORLD-2 (make-world 10 (list BALL-1 BALL-2) 1000))
(define init-list '())
; world-temp : World -> ???
(define (world-temp w)
  (... (world-t w) ... (ball-list-temp (world-balls w)) ...))

; ball-list-temp : [List-of Ball] -> ???
(define (ball-list-temp alob)
  (... (cond [(empty? alob) ...]
             [(cons? alob)
              ... (ball-temp (first alob)) ...
              ... (ball-list-temp (rest alob)) ...]) ...))

; main : [List-of Ball] -> World
; Run this game with this list of initial balls
(define (main init-list)
  (big-bang (make-world 0 init-list 800)
            [on-tick tick]
            [to-draw draw]
            [on-mouse add-ball]))
;-------------------------------------------------------------------------

;; World -> World
;; takes a World, and return one
;;  with the time incremented by one
(check-expect (tick (make-world 0 init-list 1000)) (make-world 1 init-list 1000))

(define (tick w)
  (make-world (+ 1 (world-t w)) (world-balls w) (world-radius w)))
;-------------------------------------------------------------------------

;; World -> Image
;;  draws the world

(define (draw wor)
  (foldr (make-drawer (world-t wor)) MTSCENE (world-balls wor)))
;-------------------------------------------------------------------------

;; Ball Posn Image -> Image
;; draws the ball at that point on that image
(check-expect (draw-ball BALL-1 (make-posn 2 2) empty-image)
              (place-image (circle 5 'solid 'red) 2 2 empty-image))

(define (draw-ball ba pos im)
  (place-image (square (ball-r ba) (ball-mode ba) (ball-color ba))
               (posn-x pos)
               (posn-y pos)
               im))
;-------------------------------------------------------------------------

;; Number -> [Ball Image -> Image]
;; given a time will create a function that
;;  takes a Ball and an Image and will draw it

(define (make-drawer t)
  (lambda (b i) (draw-ball b ((ball-placement b) t) i)))
;-------------------------------------------------------------------------
; A BallGenerator is a [Nat Nat Nat -> [Nat -> Posn]]
; Given the time, x-coordinate, and y-coordinate of when and where a
; ball is created, create a function that, given the current time of
; the world, will output a Posn


; move-horizontally : BallGenerator
(define (move-horizontally t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH) y0)))
(check-expect ((move-horizontally 3 5 8) 10) ; 7 seconds have passed
              (make-posn 12 8))
;-------------------------------------------------------------------------
; move-vertically : BallGenerator
(check-expect ((move-vertically 0 2 3) 10)
              (make-posn 2 13))
(define (move-vertically t0 x0 y0)
  (lambda (t) (make-posn x0 (+ y0 t))))
;-------------------------------------------------------------------------
(define (place-in-middle t0 x0 y0)
  (lambda (t) (make-posn (/ WIDTH 2) (/ HEIGHT 2))))
; World Nat Nat MouseEvent -> World
; will output a World with a Ball
;  added to it if the person clicked

(define (add-ball wor x y me)
  (cond
    [(string=? me "button-down")
     (make-world (world-t wor)
                 (cons
                  (make-ball
                   (world-radius wor)
                   'solid
                   (select-random blackwhite 2)
                   (place-in-middle (world-t wor) x y))
                  (world-balls wor)) (- (world-radius wor) 5))]
    [else wor]))
;-------------------------------------------------------------------------
(define (select-random lis n)
  (list-ref lis (random n)))
;-------------------------------------------------------------------------
(define GENERATORS (list move-horizontally move-vertically))
(define modes (list 'solid 'outline))
(define colors (list 'green 'red 'blue 'purple  (make-color 227 2 252)
                     'orange 'chartreuse 'cyan 'magenta
                     'yellow 'navy (make-color 247 19 156 255)
                     (make-color 252 2 73)))
(define number-of-colors 13)
(define blackwhite '(black white))


(main '())

