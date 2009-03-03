;Exercise 2.55: Eva Lu Ator types to the interpreter the expression
(car ''abracadabra)
;To her surprise, the interpreter prints back quote. Explain.

;; ''abracadabra is equivalent to (quote (quote abracadabra)), which in turn evaluates to (quote abracadabra), car of which is 'quote.
