#lang sicp
; Quotations
; note, program contains abstractions + snippets from exercises, and duplicate namings, so will not run directly.

; Exercise 2.54

; checks top-level contents of a list, and if their identity is equal in memory
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
      ((eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))) 
      (else #f)))


; Exercise 2.56

; Extend basic differentiator to handle more kinds of expressions.

 (define (make-exponentiation base exponent)
   (list '** base exponent))

(define (base exponentiation)
  (cadr exponentation))

(define (exponent exponentiation)
  (caddr exponentation))

(define (exponentiation? x)
  (and (pair? x) (eq? '** (car x))))


(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp)
		   (if (same-variable? exp var) 1 0))
		  ((sum? exp)
		   (make-sum (deriv (addend exp) var)
				     (deriv (augend exp) var)))
		 ((product? exp)
	      (make-sum 
		   (make-product
		    (multiplier exp)
		    (deriv (multiplicand exp) var))
		   (make-product
			(deriv (multiplier exp) var)
			(multiplicand exp))))

                 ((exponentation? exp)
                  (let ((base (base exp))
                        (exponent (exponent exp)))
                    (cond ((= exponent 0) 0) ; derivative of 1 is 0
                          ((= exponent 1) (deriv base var))
                          (else 
                           (make-product
                            (make-product
                             exponent
                             (make-exponentation base  (- exponent 1)))
                            (deriv base var))))))
                 
		(else (error "unknown expression type: DERIV" exp))))


; Exercise 2.57
; Extend differentiation to handle sums and products of arbitrary numbers of two or more terms.
; (+ a b c)

(define (make-sum a1 a2) (list '+ a1 a2))
; this was the origfinal representation for a sum. since the inputs can include two or more terms then we have to change the
; selectors.

(define (addend s) (cadr s)) ; first term.
(define (augend s)
  (if (null? (cdddr s)) ; cdr of the cdr of the cdr. checks if there is more than two elements(not including the car which is +)
      (caddr s) ; if so, the augend is just the last term
      (cons '+ (cddr s)))) ; otherwise, we extract the list of remaining elmeents, and attaching the + symbol. 


; Exerice 2.58a
; Changing the deriv program to work with infix operators e.g., (x + (3 * (x + (y + 2))))

(define (=number? exp num)
	(and (number? exp) (= exp num)))

; sums
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2))
		    (+ a1 a2))
		  (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s)
  (caddr s)) ; (x + y) or (x + (3 + y)) car = x, cddr = (y) or ((3 + y)), eitherway we need car of cddr.

; products
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2))
               (* m1 m2))
              (else
               (list m1 '* m2))))


(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))



; Exercise 2.58b

; Standard algerbraic notation like (x + 3 * (x + y + 2)).
; Drops unnecessary parantheses, and assumes multiplication is done before addition.
; this is different to 2.58 because the top level list not 3 element package anymore; + could be anywhere, so we need to scan

; case 1 (x + 3 * y).
; addend is the left side (x) -> need to clean up single elements
; augend is the right side (3 * y)

; case 2 (x * 3 * 5 + 5)
; addend is (x * 3 * 5)
; augent is (5) -> need to clean up single elements


; sums
(define (split-sum exp)
  (define (iter remaining acc)
    (cond ((null? remaining) #f)
          ((eq? (car remaining) '+)
           (cons (reverse acc) (cdr remaining))))
          (else
           (iter (cdr remaining) (cons (car remaining) acc))))

  (iter exp nil))

(define (sum? x)
  (and (pair? x) (pair? (split-sum x))))

(define (clean-up exp)
  (if (and (list? exp) (= (length exp) 1))
      (car exp)
      exp))

(define (addend s) (clean-up (car (split-sum s))))
(define (augend s) (clean-up (cdr (split-sum s))))


; multiplication - not associative, so dont scan.
; deriv dispatcher checks sum? first, so we dont change product? as it owuld have intercepted an exp which has a top-level +

(define (multiplier p) (clean-up (car p)))
(define (multiplicand p) (clean-up (cddr p)))
           


; exercise 2.59
; to union two sets, we assume we already have the union of cdr set 1 and set 2.
; so only join to join the car1 with the result
; to stop the recursion, we check if set1 is null, if so, return set2.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))


; Exercise 2.60
; sets can now be internally represented as lists that can have duplicates
; {1, 2, 3} -> (2 3 2 1 3 2 2...)

; element-of-set? wouldn't change as we are still checking if a element is a member of a list. O(n).

; adjoin-set would change, as we don't need to worry about duplicates, which increases efficiency. This is O(1)
(define (adjoin-set x set)
  (cons x set))

; union-set wouldn't change as it is an abstraction that uses adjoin-set, and we already changed adjoin-set. O(n)

; intersection-set wouldn't change as it is an abstraction that uses element-of-set, and we already changed element-of-set. O(nm)



; Exercise 2.61
; Implementation of adjoin-set using the ordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (cons x nil)) ; if the set is null, return (x)
        ((= x (car set)) set) ; if set contains x, we just return original set
        ((< x (car set)) (cons x set)) ; if x < current element of set, then it should be placed before current element
        (else
         (cons (car set) (adjoin-set x (cdr set)))))) ; otherwise, assume the recursive call will place x in right spot.
         

; Exercise 2.62
; O(n) implementation of union-set for sets represented as ordered lists.

; to do this, compare the heads of the two sets as they are ordered.
; ruyns in O(n+m) time, which is O(n), as every pass removes one or two elements from the sets.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))


; Exericse 2.66

; A procedure to lookup a record for a given key, with a set of records structured as a binary tree, which is ordered by numericla value

; a tree is represented as : (current, left branch, right branch)

(define (lookup given-key tree-of-record)
  (if (null? tree-of-record)
      false
      (let ((current (entry tree-of-record))
         (lb (left-branch tree-of-record))
         (rb (right-branch tree-of-record)))
    (cond ((= given-key (key current)) current)
          ((< given-key (key current)) (lookup given-key lb))
          (else
           (lookup given-key rb))))))


; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      nil
      (append
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  (if (leaf? tree)
      nil
      (let ((is-left? (element-of-set? symbol (left-branch tree)))
             (is-right (element-of-set? symbol (right-branch tree)))
             (current-bit
              (cond (is-left? 0)
                    (is-right? 1))))
         (cons current-bit (encode-symbol symbol
                                          (cond (is-left? (left-branch tree))
                                                (is-right? (right-branch tree))
                                                (else (error "symbol not in tree"))))))))




; Exericse 2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))


(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((= (length leaf-set) 1) (car leaf-set)) ; leaf-set would look like ( (tree-node) ). 
        (else
         (successive-merge
         (adjoin-set ; merges into new sub-tree.
          (make-code-tree (car leaf-set) (cadr leaf-set)) ; ordered list, so smallest in front - local rule
          (cddr leaf-set))))))