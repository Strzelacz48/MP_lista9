#lang racket
;Problem skoczka szachowego
; zwróci listę par z numerami pól tak aby odwiedzić każde pola 1 raz i zwiedzić całą
;szachownicę
(define (szachownica n i)
  (cond[(eq? n i) null]
       [#t (cons (szachownica2 n i 0) (szachownica n (+ 1 i)) )])
  )
(define (szachownica2 n i j)
  (cond[(eq? n j) null]
       [#t (cons (list i j #f) (szachownica2 n i (+ 1 j) ) )]
)
)
;działa
(define (get-place x y szachownica i j)
  (cond [(eq? x i)
         (get-place x y (car szachownica) (+ 1 i) j)]
        [(< x i) 
         (if (eq? y j)
             (cdr (cdr (car szachownica)))
             (get-place x y (cdr szachownica) i (+ 1 j)))]
        [#t (get-place x y (cdr szachownica) (+ 1 i) j)])
  )
;działa
(define (set-place x y szachownica val n i)
  (cond[(eq? i n) null]
       [(eq? x i)
         (cons (set-place2 y (car szachownica) val n 0)
          (cdr szachownica))]
       [(> i x) (cdr szachownica)]
       [#t (cons (car szachownica)(set-place x y (cdr szachownica) val n (+ i 1)))]
  ))
;;Działa
(define (set-place2 y szachownica val n i)
  (cond[(eq? n i)
        null]
       [(eq? y i)
        (cons
         (list (car (car szachownica)) (car (cdr (car szachownica))) val)
         (cdr szachownica))]
       [#t (cons (car szachownica) (set-place2 y (cdr szachownica) val n (+ 1 i)))]
  )
  )
;dziala
(define (can-move n x y i j)
  (if (or (>= (+ x i) n) (< (+ x i) 0) (>= (+ y j) n) (< (+ y j) 0)) #f #t)
  )

(define (skoczek n)
  (cond[(< 4 n) "Nie ma takiej drogi aby skoczek odwiedził wszystkie pola"]
       [#t (szachownica n 0)])
  )
(define (silnik nr-skoku szachownica x y n)
  (cond [(eq? nr-skoku (* n n)) (list x y)]
        [(and (can-move n x y 2 1)
              (not (get-place (+ x 2)(+ y 1) szachownica) 0 0))]
        [(and (can-move n x y 2 -1)
              () )]
        [(and (can-move n x y -2 1)
              (not (get-place (+ x -2)(+ y 1) szachownica) 0 0))]
        [(and (can-move n x y -2 -1)
              (not (get-place (+ x -2)(+ y -1) szachownica) 0 0))]
        [(and (can-move n x y 1 2)
              (not (get-place (+ x 1)(+ y 2) szachownica) 0 0))]
        [(and (can-move n x y 1 -2)
              (not (get-place (+ x 1)(+ y -2) szachownica) 0 0))]
        [(and (can-move n x y -1 2)
              (not (get-place (+ x -1)(+ y 2) szachownica) 0 0))]
        [(and (can-move n x y -1 -2)
              (not (get-place (+ x -1)(+ y -2) szachownica) 0 0))])
  )