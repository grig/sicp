(define nil '())

(define (enumerate-integers a b)
  (if (> a b)
      nil
      (cons a (enumerate-integers (+ a 1) b))))

(define (flatmap proc items)
  (accumulate append nil (map proc items)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

;Exercise 2.41: Write a procedure to find all ordered triples of distinct positive
;integers i, j, and k less than or equal to a given integer n that sum to a given
;integer s.

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                    (map (lambda (k) (list i j k))
                         (enumerate-integers 1 (- j 1))))
                  (enumerate-integers 1 (- i 1))))
           (enumerate-integers 1 n)))

(define (triple-sums-to? triple s)
  (= (sum triple) s))

(define (sum seq) (accumulate + 0 seq))

(define (triples-with-sum n s)
  (filter (lambda (triple) (triple-sums-to? triple s))
          (ordered-triples n)))
