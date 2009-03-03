;Exercise 2.52. Make changes to the square limit of wave shown in figure 2.9 by working at each of
;the levels described above. In particular:
;a. Add some segments to the primitive wave painter of exercise 2.49 (to add a smile, for example).

;; nah, i'm too lazy for that

;b. Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).

(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter (up-split painter (- n 1)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (beside painter (right-split painter (- n 1)))))

;c. Modify the version of square-limit that uses square-of-four so as to assemble the
;corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)


(define (square-limit painter n)
  (let ((quarter (corner-split (flip-horiz painter) n))) ; flipped the original painter horizontally
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
