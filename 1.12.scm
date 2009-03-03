;Exercise 1.12: The following pattern of numbers is called Pascal's triangle .
;                1
;              1   1
;            1   2   1
;         1    3   3    1
;      1     4   6   4    1
;The numbers at the edge of the triangle are all 1, and each number inside the
;triangle is the sum of the two numbers above it.35 Write a procedure that
;computes elements of Pascal's triangle by means of a recursive process.

; (pascal 0 0) -> 1
; (pascal 1 0) -> 1
; (pascal 1 1) -> 1
; (pascal 2 0) -> 1
; (pascal 2 2) -> 1
; (pascal 2 1) -> (+ (pascal 1 0) (pascal 1 1))
; (pascal 3 2) -> (+ (pascal 2 1) (pascal 2 2))

(define (pascal row col)
  (if (or (= col 0) (= col row))
      1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))
