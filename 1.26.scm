;Exercise 1.26: Louis Reasoner is having great di\ufb03culty doing unde\ufb01ned [Ex-
;ercise 1-24], page unde\ufb01ned . His fast-prime? test seems to run more slowly
;than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they
;examine Louis\u2019s code, they \ufb01nd that he has rewritten the expmod procedure to
;use an explicit multiplication, rather than calling square:
;       (define (expmod base exp m)
;         (cond ((= exp 0) 1)
;                 ((even? exp)
;                  (remainder (* (expmod base (/ exp 2) m)
;                                     (expmod base (/ exp 2) m))
;                                 m))
;                 (else
;                  (remainder (* base (expmod base (- exp 1) m))
;                                 m))))
;\u201cI don\u2019t see what di\ufb00erence that could make,\u201d says Louis. \u201cI do.\u201d says Eva.
;\u201cBy writing the procedure like that, you have transformed the [theta] (log n )
;process into a [theta] (n ) process.\u201d Explain.

;; Solution
; expmod is evaluated twice on each step with even exp. That means that if exp == 2^k, a linear process degenerates into a full binary tree, which has 2^k nodes, and expmod is now 2^(log n) = theta(n).