;; William E. Byrd   October 18, 2025
;;
;; Added example code from the miniAdapton paper.
;;
;; You can run this file from Chez Scheme by running:
;;
;; (load "paper-examples.scm")
;;
;; from within the same directory as this file.

(import (rnrs (6))
        (miniadapton)
        (memoization)
        (include))

(define-syntax test
  (syntax-rules ()
    [(_ name e1 e2)
     (let ((v1 e1)
           (v2 e2))
       (if (equal? v1 v2)
           (printf "test ~s passed\n" 'name)
           (error 'test
                  (format "unexpected value ~s produced by ~s in test ~s (value ~s expected)"
                          v1
                          'e1
                          'name
                          v2))))]))

(define-avar v1 2)

(define-avar v2 (+ (avar-get v1) 4))

(define-avar b
  (+ (avar-get v1) (avar-get v2)))

(test "avar-get b 8"
  (avar-get b)
  8)

(avar-set! v1 10)

(test "avar-get b 24"
  (avar-get b)
  24)

(define (remove-adapton v)
  (cond
    ((pair? v)
     (cons (remove-adapton (car v))
           (remove-adapton (cdr v))))
    ((adapton? v)
     (remove-adapton (adapton-force v)))
    (else v)))

(define-avar lucky 7)

(define-avar t1 (cons 1 2))

(define-avar t2 (cons 3 4))

(define-avar some-tree
  (cons (avar-get t1) (avar-get t2)))

(test "avar-get some-tree ((1 . 2) 3 . 4)"
  (avar-get some-tree)
  '((1 . 2) 3 . 4))

(define-amemo (max-tree t)
  (cond
    ((adapton? t)
     (max-tree (adapton-force t)))
    ((pair? t)
     (max (max-tree (car t))
          (max-tree (cdr t))))
    (else t)))

(define-amemo (max-tree-path t)
  (cond
    ((adapton? t)
     (max-tree-path (adapton-force t)))
    ((pair? t)
     (if (> (max-tree (car t))
            (max-tree (cdr t)))
         (cons 'left
               (max-tree-path (car t)))
         (cons 'right
               (max-tree-path (cdr t)))))
    (else '())))

(test "max-tree some-tree 4"
  (max-tree some-tree)
  4)

(test ""
  (max-tree-path some-tree)
  '(right right))

(avar-set! t2 5)

(test "avar-get some-tree ((1 . 2) . 5)"
  (avar-get some-tree)
  '((1 . 2) . 5))

(test "max-tree some-tree 5"
  (max-tree some-tree)
  5)

(test "max-tree-path some-tree (right)"
  (max-tree-path some-tree)
  '(right))

(test "(max-tree (cdr (avar-get some-tree))) 5"
  (max-tree (cdr (avar-get some-tree)))
  5)

(test "(max-tree-path (cdr (avar-get some-tree))) ()"
  (max-tree-path (cdr (avar-get some-tree)))
  '())

(avar-set! t2
  (cons 20 (* 3 (avar-get lucky))))

(test "(avar-get some-tree) ((1 . 2) 20 . 21)"
  (avar-get some-tree)
  '((1 . 2) 20 . 21))

(test "(max-tree some-tree) 21"
  (max-tree some-tree)
  21)

(test "(max-tree-path some-tree) (right right)"
  (max-tree-path some-tree)
  '(right right))

(avar-set! lucky 3)

(test "(avar-get some-tree) ((1 . 2) 20 . 9)"
  (avar-get some-tree)
  '((1 . 2) 20 . 9))

(test "(max-tree some-tree) 20"
  (max-tree some-tree)
  20)

(test "(max-tree-path some-tree) (right left)"
  (max-tree-path some-tree)
  '(right left))
