;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bezier) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))


(define (bezier a b c acc)
  (cond
    [(< (area a b c) 1) (add-line acc (posn-x a) (posn-y a) (posn-x c) (posn-y c) "red")]
    [else (bezier a (midpoint a b) (midpoint (midpoint a b) (midpoint b c))
                  (bezier (midpoint (midpoint a b) (midpoint b c)) (midpoint b c) c acc))]))

(define (area a b c)
  (abs (/ (+ (* (posn-x a) (- (posn-y b) (posn-y c))) (* (posn-x b) (- (posn-y c) (posn-y a)))
        (* (posn-x c) (- (posn-y a) (posn-y b)))) 2)))

(define (midpoint a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2) (/ (+ (posn-y a) (posn-y b)) 2)))




(bezier (make-posn 1000 100) (make-posn 500 1000) (make-posn 250 250) empty-image)
(time bezier)

