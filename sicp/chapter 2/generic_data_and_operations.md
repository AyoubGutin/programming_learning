# Exercises

## Generic Data

### 2.73

A) The predicates number? and variable? cannot be assimilated as we decided to use the algebraic operator as the type, and the above predicates do not have a algebraic operator, unlike the multiplication, addition predicates. For example, if the expression is a sole number, like 5, the operator would give an error as it is not a list structure, so there is no car.

B)

```Scheme
; derivative of sum
(define (addend s) (car s))
(define (augend s) (cadr s))

(define (deriv-sum s var)
	(make-sum
		(deriv (addend s) var)
		(deriv (augend s) var)))

(put 'deriv '+ deriv-sum)

(define (mulitplier p) (car p))
(define (multiplicand p) (cadr p))

(define (deriv-product p var)
	(make-sum
		(make-product
			(multiplier p)
			(deriv (multiplicand p) var))
		(make-product
			(multiplicand p)
			(deriv (multiplier p) var))
	))


(put 'deriv '* deriv-product)
```

C)

```Scheme
#lang sicp
(define (base exponentiation)
  (car exponentiation))

(define (exponent exponentiation)
  (cadr exponentiation))

(define (deriv-exponentiation expr var)
  (let ((b (base expr))
        (e (exponent expr)))

    (cond ((constant? e var) ; u^constant
           (make-product
            (make-product
             e
             (make-exponentiation b (- e 1)))
            (deriv b var)))

          ((constant? b var)
           (make-product
            (make-product
             expr
             (make-log b))
            (deriv e var)))

          (else
           (error "USUPPORTED EXPONENTIATION:" expr)))))

(put 'deriv '** deriv-exponentiation)
```

No changes made to deriv main dispatch procedure -> scalable.

D)

If procedures are indexed reversed, meaning operator is the first parameter, and the 'deriv symbol is the second parameter, then, the installations would need to reflect that change and swap it around.

Simplest work around is just to swap the order of the put and get procedure

### 2.74

```Scheme
; picks what procedure of get-record to use as procedure depends upon structure. Then, calls that procedure with the name., and then applies a tag to the output.
(define (get-record name file)
  (attach-tag (type-tag file)
              ((get 'get-record (type-tag file)) name (contents file))))

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

```

### 2.75

```Scheme
(define (make-from-mag-ang r a)
	(define (dispatch op)
		(cond ((eq? op 'real-part) (* r (cos a)))
			  ((eq? op 'imag-part) (* r (sin a)))
			  (eq? op 'magnitude) r)
			  (eq? op 'angle) a)
			  (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
	dispatch)
```

## Generic Operations

### 2.77

```Scheme
(put 'magnitude '(complex) magnitude)
```

This installs the magnitude selector, for the index: ('magnitude,'complex).

Trace:

```Scheme
z = (complex rectangular 3.4)

(magnitude z)
(apply-generic 'magnitude z)
; type-tags = (complex)
; proc = (get 'magnitude '(complex)) => magnitude

(apply magnitude '((rectangular 3 . 4)))
(magnitude '(rectangular 3 . 4))

(apply-generic 'magnitude '(rectangular 3 . 4))

; type-tags = (rectangular)
; proc = (get 'magnitude '(retangular)) => rectangular internal magnitude
(apply magnitude-rectangular '((3 . 4)))
(magnitude-rectangular '(3 . 4))
(sqrt (+ (square 3) (square 4)))
5
```

This works as the outer tag for a number is a complex, in this example. The selectors were defined for rectangular / polar types, and that tag is nested so it cannot be accessed in the current system. By installing the selectors as a complex type, it can now be accessed by the complex types and subsequently run through tearing down of tags until the right procedure can be accessed.

Apply generic is called twice, the first call dispatches to the generic magnitude, the second one dispatches to rectangular package's magnitude.
