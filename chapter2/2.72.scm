;Exercise 2.72. Consider the encoding procedure that you designed in exercise 2.68. What is the order of
;growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed
;to search the symbol list at each node encountered. To answer this question in general is difficult. Consider
;the special case where the relative frequencies of the n symbols are as described in exercise 2.71, and give
;the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least
;frequent symbols in the alphabet.

(define (encode-symbol symbol tree)
  (if (leaf? tree) '()
      (cond ((contains-symbol? symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
            ((contains-symbol? symbol (right-branch tree)) (cons 1 (encode-symbol symbol (right-branch tree)))))
            (else (error "encode-symbol: tree does not contain symbol" symbol))))

;; alphabet of n symbols has their relative frequences defined as 1, 2, .., 2^(n - 1).
;; number of steps s(n) is

;; for the most frequent symbol s_min(n) = const.
;; for the least frequent symbol
;; s_max(n) = c + k*n + s_max(n - 1) = c + kn + c + k(n - 1) + .. + c = c(n + 1) + kn * n/2
;; Order of growth is O(n^2).