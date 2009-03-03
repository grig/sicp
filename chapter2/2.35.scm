(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Exercise 2.35: Rede\ufb01ne count-leaves from section [2-2-2]
; as an accumulation:

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define nil '())

;(define (count-leaves t)
;  (accumulate ? ? (map ?? ??)))

(define (count-leaves tree)
  (accumulate + 0 
              (map (lambda (node) (if (leaf? node) 1 (count-leaves node))) tree)))

(define (leaf? node) (not (pair? node)))
