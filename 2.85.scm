;Exercise 2.85. This section mentioned a method for ``simplifying'' a data object by lowering it in the
;tower of types as far as possible. Design a procedure drop that accomplishes this for the tower described
;in exercise 2.83. The key is to decide, in some general way, whether an object can be lowered. For
;example, the complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i can be
;lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all. Here is a plan for
;determining whether an object can be lowered: Begin by defining a generic operation project that
;``pushes'' an object down in the tower. For example, projecting a complex number would involve throwing
;away the imaginary part. Then a number can be dropped if, when we project it and raise the result
;back to the type we started with, we end up with something equal to what we started with. Show how to
;implement this idea in detail, by writing a drop procedure that drops an object as far as possible. You will
;need to design the various projection operations53 and install project as a generic operation in the
;system. You will also need to make use of a generic equality predicate, such as described in exercise 2.79.
;Finally, use drop to rewrite apply-generic from exercise 2.84 so that it ``simplifies'' its answers.

(define (project x)
  ((get 'project (type-tag x)) (contents x)))

;; complex package
(put 'project 'complex (lambda (x) (make-real (real-part x))))

;; real package
(put 'project 'real (lambda (x) (make-integer (round x))))

(define (drop x)
  (let ((projection (project x)))
    (if (eqv? x (raise projection))
        (drop projection)
        x)))

(define (apply-generic op . args)
  (let ((coerced-args (raise-to-same-level args)))
    (let ((proc (get op (map type-tag coerced-args))))
      (if proc
          (simplify (apply proc (map contents coerced-args)))
          (error "no method for these types" (list op type-tags))))))

(define (simplify results)
  (map drop results))
