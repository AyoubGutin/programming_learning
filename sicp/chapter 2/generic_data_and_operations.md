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

### Exercise 2.78

```Scheme

; improved version, where it handles primtiive numbers

(define (attach-tag type-tag contents)
	(if (number? contents)
		contents
		(cons type-tag contents)))

(define (type-tag datum)
	(cond ((pair? datum) (car datum))
		  ((number? datum) 'scheme-number)
		  (else
			(error "Bad tagged datum:
				TYPE-TAG " datum))))


(define (contents datum)
	(cond ((pair? datum) (cdr datum))
		  ((number? datum) datum)
		  (else
			(error "Bad tagged datum:
				CONTENTS" datum))))


```

Numbers are not stored with the type-tag anymore. The `type-tag` procedure works out `'scheme-number` from Scheme's primitive predicate each time it dispatches, then discards it. Tag isn't stored in data, its computed when needed + storage is now cheaper.

### Exercise 2.79

(define (equ? x y)
(apply-generic 'equ? x y))

(put 'equ? '(scheme-number scheme-number) =)

(define (fractional-equ? x y)
(= (_ (numer x) (denom y)) (_ (numer y) (denom x)))); would be inside the rational package, not standalone

(define (complex-equ? x y)
(and (= (real x) (real y)) (= (imag x) (imag y)))) ; would be inside complex package, not standalone

(put 'equ? '(rational rational) fractional-equ?)
(put 'equ? '(complex complex) complex-equ?)

### Exercise 2.81

1.  What happens when procedures are put in the coercion table to coerce arguments of each type to their own type, and an operation isn't found.

```Scheme
(apply-generic 'missing-operation x y) ; x, y are two scheme-numbers

type-tags = (scheme-number scheme-number)
proc = #f

type1 = scheme-number
type2 = scheme-number
a1 = x
a2 = y

t1->t2 = (get-coercion scheme-number scheme-number) = scheme-number->scheme-number

t2->t1 = (get-coercion scheme-number scheme-number) = scheme-number->scheme-number

(apply-generic 'missing-operation (t1->t2 a1) a2)

...

; recursion ends up happening, as there will always be a 'raise' procedure, with no operation being found as it isnt being installed in the first place.
```

recursion ends up happening, as there will always be a 'raise' procedure, with no operation being found as it isn't being installed in the first place.

```Scheme
(define (exp x y)
	(apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
	(lambda (x y)
		(tag (expt x y))))
```

What will happen in the above snippet , when called with two complex numbers:

`apply-generic` will extract the type tags, which are all `complex`, and extract the required procedure, which it will not find as it would have only been defined for `scheme-number`.
So, it will attempt to find coercions for where the `type1` can be coerced into `type2`, which in this case is just the identity, so `complex->complex`. It will then do a recursive call, with the new object for the first argument, which in actuality is just the same value and representation. This will lead to another infinite loop, and will never reach the intended operation.

2.  Tracing `apply-generic` as is:

```Scheme
(exp x y)
(apply-generic 'exp x y)

type-tags = (complex complex)
proc = #f

type1 = complex
type2 = complex
a1 = x
a2 = y

t1->t2 = #f
t2->t1 = #f

error "No method for these types"
```

So, the program fails and gives an error, where there is no method for these types, which is right. However, there is no need to attempt to find the coercions of the same types, as the initial fact that the procedure was not found with arguments of the same type should be an indicator that the function has to stop.

3.

```Scheme
; modified apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))
		 (type1 (car type-tags))
		 (type2 (cadr type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq type1 type2)))
              (let  ((a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1
                                     type2))
                      (t2->t1
                       (get-coercion type2
                                     type1)))
                  (cond (t1->t2
                         (apply-generic
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic
                          op a1 (t2->t1 a2)))
                        (else
                         (error
                          "No method for
                           these types"
                          (list
                           op
                           type-tags))))))
              (error
               "No method for these types"
               (list op type-tags)))))))

```

### Exericse 2.82

Goal is to generalise `apply-generic`, so that it can handle multiple arguments.

One strategy is to coerce all arguments to type of the first argument, find a relevant operation. If not found, try second. And so on.

```Scheme
(define (list-n element n)
  (if (= n 0) nil
      (cons element (list-n element (- n 1)))))


(define (coerce-one arg t)
  (let ((t-arg (type-tag arg)))
    (let ((coerce-proc (get-coercion t-arg t)))
      (if (eq? t-arg t)
        arg
        (if coerce-proc
            (coerce-proc arg)
            #f)))))



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
        (define (try-coercion tags)
          (if (null? tags)
              (error "No method for the types" (list op type-tags))
              (let ((t (car tags)))
                (let ((coerced (map (lambda (x) (coerce-one x t)) args))
                      (proc (get op (list-n t (length args)))))
                  (if (and (not (memq #f coerced)) proc)
                      (apply proc (map contents coerced))
                      (try-coercion (cdr tags)))))))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (try-coercion type-tags)))))


```

However, if we call `apply-generic` with `n3 n1`, and the only valid operation for `op` is `(op (n1 n3)`. It will first attempt to look at the index `(op (n3 n1)`. It will not find it. Then, it will do `(op (n3 n3))`. It will not find it. Then, it will do `(op (n1 n1))`, provided it has a way of remembering the second argument's original type. It will not find it.

Therefore, every key, apart from the original, it builds is of a single type. So, a method keyed on mixed types can never be found.
Furthermore, it cannot look for different types not found in the original call, for example, if called on` (scheme-number rational)`, it cannot ever be a `complex` type

### Exercise 2.83

```Scheme
(define (raise datum)
  (apply-generic 'raise datum))

(define (integer->rational datum)
  (make-rational datum 1))
(put 'raise '(integer) integer->rational) ; lives inside integer package

(define (rational->real datum)
  (make-real (/ (numer datum) (denom datum))))
(put 'raise '(rational) rational->real) ; lives inside rational package

(define (real->complex datum)
  (make-complex-from-real-imag datum 0))
(put 'raise '(real) real->complex) ; lives inside real package
```

### Exercise 2.84

```Scheme
;1. abstract approach. assume we have a way of getting a types 'level' in the tower.
; this knows whether one type is above another instantly, but its an extra table/variable to update when a type is added

(define (is-type-higher arg-type t)
  (> (- (tower-level t) (tower-level arg-type)) 0)) ; tower level would handle non existent types. abstracted


(define (successive-raise arg t)
  (let ((arg-type (type-tag arg)))
    (cond ((eq? arg-type t) arg) ; base-case: is the argument type the target
          ((is-type-higher arg-type t) (successive-raise (raise arg) t)) ; if target type is higher, raise once.
          (else #f)))) ; otherwise, it is lower, or there cannot be any more raises.


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
        (define (try-coercion tags)
          (if (null? tags)
              (error "No method for the types" (list op type-tags))
              (let ((t (car tags)))
                (let ((coerced (map (lambda (x) (successive-raise x t)) args))
                      (proc (get op (list-n t (length args)))))
                  (if (and (not (memq #f coerced)) proc)
                      (apply proc (map contents coerced))
                      (try-coercion (cdr tags)))))))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (try-coercion type-tags)))))


; 2. less abstract, we do it in one pass, but relies on raising first, so it can potentially do more 'useless' work.

(define (successive-raise-2 arg t)
  (let ((arg-type (type-tag arg)))
    (let ((proc (get 'raise (list arg-type))))
      (cond ((eq? arg-type t) arg) ; base-case: is the argument type the target
            (proc (successive-raise-2 (raise arg) t))
            (else #f))))) ; otherwise, it is lower, or there cannot be any more raises.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
        (define (try-coercion tags)
          (if (null? tags)
              (error "No method for the types" (list op type-tags))
              (let ((t (car tags)))
                (let ((coerced (map (lambda (x) (successive-raise-2 x t)) args))
                      (proc (get op (list-n t (length args)))))
                  (if (and (not (memq #f coerced)) proc)
                      (apply proc (map contents coerced))
                      (try-coercion (cdr tags)))))))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (try-coercion type-tags)))))
```
