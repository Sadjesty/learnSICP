(define (tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree proc)
             (proc sub-tree)))
  tree))

(define (scale-tree tree factor)
  (tree-map tree (lambda (x) (* factor x))))
(define (square-tree-map tree)
  (tree-map tree square))
(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))