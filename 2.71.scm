;Exercise 2.71. Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative
;frequencies of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the tree for n=5; for n=10. 
;; ((A 1) (B 2) (C 4) (D 8) (E 16))

;;((A B C D E) 32)
;;(E 16) ((A B C D) 16)
;;       (D 8) ((A B C) 8)
;;             (C 4) ((A B) 4)
;;                   (B 2) (A 1)

;In such a tree (for general n) how may bits are required to encode the most frequent symbol? the least frequent symbol?

;; each tree level except the last one removes one element from the set, while the last has two elements. Total number of tree levels k must satisfy
;; n = 1 * 2 (bottom level) + 1 * 0 (top level) + (k - 2)*1 (middle levels) = k

;; Therefore, most frequent symbol requires 1 bit while the least frequent symbol requires (k - 1) = (n - 1) bits.