;Exercise 1.20: The process that a procedure generates is of course dependent
;on the rules used by the interpreter. As an example, consider the iterative
;gcd procedure given above. Suppose we were to interpret this procedure using
;normal-order evaluation, as discussed in section unde\ufb01ned [1-1-5], page un-
;de\ufb01ned . (The normal-order-evaluation rule for if is described in unde\ufb01ned
;[Exercise 1-5], page unde\ufb01ned .) Using the substitution method (for normal
;order), illustrate the process generated in evaluating (gcd 206 40) and indicate
;the remainder operations that are actually performed. How many remainder
;operations are actually performed in the normal-order evaluation of (gcd 206
;40)? In the applicative-order evaluation?

(define (remainder a b)
  (if (< a b)
      a
      (remainder (- a b) b)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; normal-order
(gcd 206 40)
(if (= 40 0) 40
    (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0) 40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; 1
(if (= 6 0) 40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remainder 206 40)))))
; 1 + 2 = 3
(if (= 4 0) (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) 
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 206 40)
                  (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))   
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))         
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

; 3 + 4 = 7
(if (= 2 0) 0
    (remainder 40 (remainder 206 40))   
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))         
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))         
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40))))))

(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40 (remainder 206 40)))))))

; 7 + 7 = 14
(if (= 0 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40))))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40 (remainder 206 40)))))))

(remainder (remainder 206 40)
           (remainder 40 (remainder 206 40)))
           
; 18
2

; applicative order
(gcd 206 40)
(gcd 40 6)  ; 1
(gcd 6 4)   ; 2
(gcd 4 2)   ; 3
(gcd 2 0)   ; 4
2