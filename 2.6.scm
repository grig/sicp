(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (g) (lambda (y) (g ((\f\x.x g) y))))
;(\g\y (g ((\x.x) y)))
;(\g\y (g y))

(define one (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;(add-1 (\g\y.g y))
;(\f\x. (f (((\g\y.g y) f) x)))
;(\f\x. (f ((\y.f y) x)))
;(\f\x. (f (f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))

;(one one)
;(one (\f\x.f x))
;((\g\y.g y) (\f\x.f x))
;(\y.(\f\x.f x) y)
;(\y.\x.(y x))

;(one zero)
;((\f\x.f x) (\g\y.y))
;(\x.(\g\y y) x)
;(\x\y.y)
;
;(one two)
;((\f\x.f x) (\g\y.(g (g y))))
;(\x.(\g\y.(g (g y))) x)
;(\x.(\y.x (x y)))
;=> two
;
;(two two)
;((\f\x.(f (f x)) (\g\y.(g (g y)))))
;(\x.((\g\y.(g (g y))) ((\h\z.(h (h z))) x)))
;(\x.((\g\y.(g (g y))) (\z.(x (x z)))))
;(\x.(\y.((\z.(x (x z))) ((\t.(x (x t))) y))))
;(\x.(\y.((\z.(x (x z))) (x (x y)))))
;(\x(\y(x (x (x (x y))))))
;=> four

; (define (mul a b) (a b))

;(two two)

;(define (add a b)
;  (lambda (f) (lambda (x) )))


; sum: 

;\f\x.x + \f\x.(f x) => \f\x.(f x)
;\f\x.(f x) + \g\y.(g y) => \h\z.(h (h z))
; = \h\z((\f\x.(f x) h) (((\g\y.(g y)) h) z)) ; << right?
; = \h\z((\x.(h x)) ((\y.(h y)) z))
; = \h\z.((\x.(h x)) (h z))
; = \h\z.(h (h z))
  
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;(add one two)
;(add (\f\x.(f x)) (\g\y.(g (g y))))
;(\f\x.(((\g\y.(g y)) f) (((\h\z.(h (h z))) f) x)))
;(\f\x.((\y.(f y)) ((\z.(f (f z))) x)))
;(\f\x.((\y.(f y)) (f (f x))))
;(\f\x.(f (f (f x))))
;=> 3, as was expected
