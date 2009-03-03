;Exercise 2.43: Louis Reasoner is having a terrible time doing unde\ufb01ned [Ex-
;ercise 2-42], page unde\ufb01ned . His queens procedure seems to work, but it runs
;extremely slowly. (Louis never does manage to wait long enough for it to solve
;even the 6*6 case.) When Louis asks Eva Lu Ator for help, she points out that
;he has interchanged the order of the nested mappings in the flatmap, writing
;it as
;       (flatmap
;         (lambda (new-row)
;           (map (lambda (rest-of-queens)
;                    (adjoin-position new-row k rest-of-queens))
;                 (queen-cols (- k 1))))
;         (enumerate-interval 1 board-size))
;Explain why this interchange makes the program run slowly.

;; because  the same recursive calculation of queen-cols repeated for each row in k-th column, instead of being done only once.

;Estimate how long it will take Louis's program to solve the eight-queens puzzle, 
;assuming that the program in [Exercise 2-42] solves the puzzle in time T.

;; Using T' for run time of Louis's programs given board size x:
;; f(k) represents time spent doing the rest of work during one step of the recursion; T(n+1) = f(n+1)*T(n); T'(n+1) = f(n+1)*T'(n).
;; T'(1) = T(1)
;; T'(2) = 2*f(2)*T'(1) = 2*f(2)*T(1) = 2*T(2)
;; T'(3) = 3*f(3)*T'(2) = 3*2*f(3)*T(2)
;; ...
;; T'(k) = k!*T(k).