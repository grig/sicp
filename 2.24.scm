;Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3
;4))). Give the result printed by the interpreter, the corresponding box-and-
;pointer structure, and the interpretation of this as a tree (as in unde\ufb01ned
;[Figure 2-6], page unde\ufb01ned ).

;; interpreter result:
;; (1 (2 (3 4)))

;; box-and-pointer structure
; ---      ---
;|*|*| -> |*|/|
; ---      ---
; |        |
; v        v
; 1       ---      ---
;        |*|*| -> |*|/|
;         ---      ---
;         |        |
;         v        v
;         2       ---      ---
;                |*|*| -> |*|/|
;                 ---      ---
;                 |        |
;                 v        v
;                 3        4

;; as tree:
; (1 (2 (3 4)))
;  /     \
; 1    (2 (3 4))
;       /  \
;      2   (3 4)
;           /\
;          3  4
