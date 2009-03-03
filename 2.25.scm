;Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each
;of the following lists:
;(1 3 (5 7) 9)
;((7))
;(1 (2 (3 (4 (5 (6 7))))))

(equal? (cadr (caddr '(1 3 (5 7) 9))) 7)
(equal? (caar '((7))) 7)
(equal? (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) 7)
