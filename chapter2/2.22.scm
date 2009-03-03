;Exercise 2.22: Louis Reasoner tries to rewrite the \ufb01rst square-list procedure
;of unde\ufb01ned [Exercise 2-21], page unde\ufb01ned so that it evolves an iterative
;process:
;      (define (square-list items)
;         (define (iter things answer)
;           (if (null? things)
;                answer
;                (iter (cdr things)
;                       (cons (square (car things))
;                               answer))))
;         (iter items nil))
;Unfortunately, de\ufb01ning square-list this way produces the answer list in the
;reverse order of the one desired. Why?

;; Because the first element of the original list will be consed first with the result, thus becoming the last element of the result.

;Louis then tries to \ufb01x his bug by interchanging the arguments to cons:
;      (define (square-list items)
;         (define (iter things answer)
;           (if (null? things)
;                answer
;                (iter (cdr things)
;                       (cons answer
;                               (square (car things))))))
;         (iter items nil))
;This doesn\u2019t work either. Explain.

;; This does not produce a correct list at all. Instead, its result is a left-associative cons of squares, like (cons (cons a b) c)