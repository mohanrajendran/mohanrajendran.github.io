---
layout: spiedpage
order: 16
title: Section 2.5 solutions
exercises: '2.77 - 2.91'
submenu:
  - { hook: "Exercise2_77", title: "Exercise 2.77" }
  - { hook: "Exercise2_78", title: "Exercise 2.78" }
  - { hook: "Exercise2_79", title: "Exercise 2.79" }
  - { hook: "Exercise2_80", title: "Exercise 2.80" }
  - { hook: "Exercise2_81", title: "Exercise 2.81" }
  - { hook: "Exercise2_82", title: "Exercise 2.82" }
  - { hook: "Exercise2_83", title: "Exercise 2.83" }
  - { hook: "Exercise2_84", title: "Exercise 2.84" }
  - { hook: "Exercise2_85", title: "Exercise 2.85" }
  - { hook: "Exercise2_86", title: "Exercise 2.86" }
  - { hook: "Exercise2_87", title: "Exercise 2.87" }
  - { hook: "Exercise2_88", title: "Exercise 2.88" }
  - { hook: "Exercise2_89", title: "Exercise 2.89" }
  - { hook: "Exercise2_90", title: "Exercise 2.90" }
  - { hook: "Exercise2_91", title: "Exercise 2.91" }
---

### Exercise 2.77<a id="Exercise2_77">&nbsp;</a>

In this exercise, Louis Reasoner tries to apply the `magnitude` function to a `complex` object given in the section. However, it fails and Alyssa tells him to install the following package to make things work:-

{% highlight scheme %}
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
{% endhighlight %}

We are asked to explain how this works. As an example, we trace `(magnitute z)`. `z` is `(complex rectangular 3 . 4)`. It is also assumed that we have installed `rectangular-package` and `polar-package` from the previous section to the dispatch table along with the generic selectors for the four functions. 

{% highlight scheme %}
(magnitude z)

(magnitude (complex rectangular 3 . 4))

(apply-generic 'magnitude (complex rectangular 3 . 4))

((get 'magnitude '(complex)) (rectangular 3 . 4))

(magnitude (rectangular 3 . 4))

(apply-generic 'magnitude (rectangular 3 . 4))

((get 'magnitude '(rectangular)) (3 . 4))

(sqrt (+ (square 3) (square 4)))

5
{% endhighlight %}

As can be seen, `apply-generic` is called twice. The first time it is applied, the additions to the `complex` package as suggested would cause the `'complex` tag to be peeled off and another `magnitude` function applied to the `rectangular` object inside. Since we had already defined the precedure for the `rectangular` object, that is evaluated and we get the right answer. The same will be done for a `polar` object wrapped inside.

### Exercise 2.78<a id="Exercise2_78">&nbsp;</a>

In this exercise, we are told to modify the definitions of `type-tag`, `contents` and `attach-tag` from the previous section so that we can simply store a `scheme-number` as a Lisp number without tags. We can do that using the following code:-

{% highlight scheme %}
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
                      TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
                      CONTENTS" datum))))
{% endhighlight %}

Thus, by using primitive predicate function `number?`, we do not need to tag numbers.

### Exercise 2.79<a id="Exercise2_79">&nbsp;</a>

In this exercise, we are tasked with implementing `equ?` which can be used as a generic function to test the equality of two numbers which can be ordinary numbers, rational numbers or complex numbers.

{% highlight scheme %}
(define (install-arithmetic-package-equ?)
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
       (lambda (x y) (= (* (numer x) (denom y))
                        (* (numer y) (denom x)))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  'done)

(define (equ? x y)
  (apply-generic 'equ? x y))
{% endhighlight %}

Installing the above code would allow `equ?` to compare the three types of numbers as follows:-

{% highlight scheme %}
(equ? (make-scheme-number 4)
      (make-scheme-number 2))
; #f
(equ? (make-scheme-number 4)
      (make-scheme-number 4))
; #t
(equ? (make-rational 4 8)
      (make-rational 2 4))
; #t
(equ? (make-complex-from-real-imag 4 3)
      (make-complex-from-real-imag 4 3))
; #t
(equ? (make-complex-from-real-imag 4 0)
      (make-complex-from-mag-ang 4 0))
; #t
(equ? (make-complex-from-real-imag 4 3)
      (make-complex-from-mag-ang 4 3))
; #f
{% endhighlight %}

### Exercise 2.80<a id="Exercise2_80">&nbsp;</a>

Similar to `equ?` in the previous exercise, we are told to implement `=zero?`, a generic function that checks if an ordinary number, a rational number or a complex number is zero.

{% highlight scheme %}
(define (install-arithmetic-package-=zero?)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))
{% endhighlight %}

Now, we can check if any type of number is zero as follows:-

{% highlight scheme %}
(=zero? (make-scheme-number 1))
; #f
(=zero? (make-scheme-number 0))
; #t
(=zero? (make-rational 1 2))
; #f
(=zero? (make-rational 0 2))
; #t
(=zero? (make-complex-from-real-imag 0 0))
; #t
(=zero? (make-complex-from-real-imag 1 0))
; #f
(=zero? (make-complex-from-mag-ang 0 1))
; #t
{% endhighlight %}

### Exercise 2.81<a id="Exercise2_81">&nbsp;</a>

This exercise serves to introduce the concept of coercion to us. Louis believes that we need to add functions to coerce arguments of each type to their own type ie. an identity function. With the identity functions installed, we are asked to explain its effects.

##### Part-1

In the first part of the question, Louis installs the following code in addition to the identity coercions:-

{% highlight scheme %}
(define (exp x y) 
  (apply-generic 'exp x y))

(put 'exp 
     '(scheme-number scheme-number)
     (lambda (x y) 
       (tag (expt x y)))) 
{% endhighlight %}

Now, we are asked what happens if we call `exp` on two complex numbers. Since we do not have a `exp` procedure defined for `'(complex complex)` type, I would expect it to enter an infinite loop. Let us see how this is:-

{% highlight scheme %}
(exp c1 c2)

(apply-generic 'exp c1 c2)

; (get 'exp '(complex complex)) evaluates to #f
; (get-coercion 'complex 'complex) returns identity
(apply-generic 'exp (complex->complex c1) c2)

(apply-generic 'exp c1 c2)

...
{% endhighlight %}

As can be seen, we enter an inifinite loop because coercing back to the original type brings us back to square one.

##### Part-2

In the second part, we are asked if Louis made the right choice in providing coercions for the arguments of the same type. My answer would be no. Louis made the wrong decision. As seen in the previous part, adding identity coercions cause loops because they add no new information to the program flow. If there already did not exist a function that takes the argument types, trying again without changing types would be foolish. By not adding these identity coercions, `apply-generic` function would work correctly by throwing an error for the missing function.

##### Part-3

In this part, we are told to modify `apply-generic` so that it does not try coercion if both arguments have the same type.

{% highlight scheme %}
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    ((let ((t1->t2
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
                                type-tags)))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
{% endhighlight %}

### Exercise 2.82<a id="Exercise2_82">&nbsp;</a>

In this exercise, we are tasked with generalizing `apply-generic` to handle coercion in the general case of multiple arguments. This can be done by finding a super-type amongst the given argument types and coercing them all to the proper form. To that end, let us rewrite `apply-generic` in a more composed form as follows:-

{% highlight scheme %}
(define (apply-generic op . args)
  (define (get-super-type types)
    (if (are-all-same? types)
        #f
        (super-type-iter types types)))
        
  (define (are-all-same? types)
    (define (same-iter target other-types)
      (if (null? other-types)
          #t
          (and (eq? target (car other-types))
               (same-iter target (cdr other-types)))))
    (same-iter (car types) (cdr types)))
    
  (define (super-type-iter types candidate-types)
    (if (null? candidate-types)
        #f
        (if (are-sub-types types (car candidate-types))
            (car candidate-types)
            (super-type types (cdr candidate-types)))))
            
  (define (are-sub-types types target)
    (if (null? types)
        #t
        (if (or (eq? (car types) target)
                (get-coercion (car types) target))
        (are-sub-types (cdr types) target)
        #f)))
        
  (define (coerce-to super-type)
    (lambda (arg)
      (let ((arg-type (type-tag arg)))
        (if (eq? arg-type super-type)
            arg
            (let ((coercion (get-coercion arg-type super-type)))
              (coercion arg))))))
              
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((super-type (get-super-type type-tags)))
            (if super-type
                (apply apply-generic (cons op (map (coerce-to super-type) args)))
                (error "No method for these types"
                       (list op type-tags))))))))
{% endhighlight %}

When we cannot find the correct function, we try coercing. First, we check if all the supplied types are the same. If they are, there is no use coercing and emits an error. Then, we try to find a super type amongst the given types. If we can't find such a type, we emit an error. Finally, given the super-type, we coerce all the arguments to that super-type and try again. Let us run some test cases:-

{% highlight scheme %}
(add (make-complex-from-real-imag 3 4) (make-scheme-number 5))
; (complex rectangular 8 . 4)
{% endhighlight %}

The original cases work. Let us define a function that takes three arguments and try again.

{% highlight scheme %}
(define (install-complex-package-add3)
  (define (tag x)
    (attach-tag 'complex x))
  (put 'add3 '(complex complex complex)
    (lambda (z1 z2 z3) (add (add (tag z1) (tag z2)) (tag z3))))
  'done)
; install-complex-package-add3
(install-complex-package-add3)
; done
(define (add3 x y z) (apply-generic 'add3 x y z))
; add3

(add3 (make-complex-from-real-imag 1 2)
      (make-complex-from-real-imag 3 4)
      (make-complex-from-real-imag 5 6))
; (complex rectangular 9 . 12)

(add3 (make-complex-from-real-imag 1 2)
      (make-scheme-number 3)
      (make-complex-from-real-imag 5 6))
; (complex rectangular 9 . 8)
{% endhighlight %}

The one pitfall to this method is that the super-type to be tested is always one that is amongst the arguments supplied. For example, let us look at the relations for geometric figures given in the book. If we are given a function called on `'(triangle quadrilateral)` type, we cannot solve it correctly because there is no coercion from `'triangle` to `'quadrilateral` or vice versa. However, there exists coercion for their combined super-type `'polygon`. This coercion will not be tested in our current algorithm.

### Exercise 2.83<a id="Exercise2_83">&nbsp;</a>

In this exercise, we are asked to design a procedure that raises the type of the data and install them. We use `scheme-number` to replace `real`.

{% highlight scheme %}
(define (install-arithmetic-package-raise)
  (put 'raise '(integer)
       (lambda (n) (make-rational n 1)))
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (exact->inexact (/ (numer r) (denom r))))))
  (put 'raise '(scheme-number)
       (lambda (n) (make-complex-from-real-imag n 0)))
  'done)

(define (raise x)
  (apply-generic 'raise x))
{% endhighlight %}

With this package in place, we can raise the data types. Let us test it:-

{% highlight scheme %}
(raise (make-scheme-number 5))
; (rectangular 5 . 0)
(raise (make-rational 3 4))
; (scheme-number . .75)
{% endhighlight %}

### Exercise 2.84<a id="Exercise2_84">&nbsp;</a>

This exercise tasks us with using the `raise` operation defined in the previous exercise to modify the `apply-generic` procedure to work on arguments of multiple type. We can do that by testing which of the provided types is higher up in the tower of type hierarchy and raising the other to that type.

Further, we are required to set-up code such that future modifications are easy. We assume that the types follow a tower hierarchy and not a more complex relationship like the polygons. Let us first define how we can define the type hierarchy

{% highlight scheme %}
(define (put-level type level)
  (hash-table/put! dispatch-table (list 'level type) level))

(define (get-level type)
  (hash-table/get dispatch-table (list 'level type) (error "Type tag not found" type)))

(define (install-arithmetic-package-hierarchy)
  ;; Tower definition and extraction
  (define tower '(integer rational scheme-number complex))
  (define (tower-level type)
    (define (get-level-iter types level)
      (if (null? types)
          (error "Type not found" type)
          (if (eq? type (car types))
              level
              (get-level-iter (cdr types) (+ level 1)))))
    (get-level-iter tower 0))

  ;; Level definition
  (put-level 'integer (tower-level 'integer))
  (put-level 'rational (tower-level 'rational))
  (put-level 'scheme-number (tower-level 'scheme-number))
  (put-level 'complex (tower-level 'complex))
  'done)
; install-arithmetic-package-hierarchy

(install-arithmetic-package-hierarchy)
; done

(get-level 'integer)
; 0
(get-level 'rational)
; 1
(get-level 'scheme-number)
; 2
(get-level 'complex)
; 3
{% endhighlight %}

Thus, when we add new types, we only need to modify the tower and add the `'level` definition to the dispatch table. Now, we can modify the `apply-generic` function.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
; apply-generic

(add (make-rational 1 3)
     (make-scheme-number 1))
; (scheme-number . 1.3333333333333333)
(add (make-rational 1 2)
     (make-complex-from-real-imag 5 6))
; (complex rectangular 5.5 . 6)
{% endhighlight %}

### Exercise 2.85<a id="Exercise2_85">&nbsp;</a>

In this exercise, analogous to the `raise` operation that we have worked with, we are tasked with implementing a `drop` operation that can be used to push the data to the simplest level in the hierarchy. For that purpose, we create a function called `project` that pushes the data down by throwing away data. When we raise it again, if it retains the same value before projecting, we can safely use the lower level for storage.

Let use first define the `project` procedure:-
{% highlight scheme %}
(define (install-arithmetic-package-project)
  (put 'project '(complex)
       (lambda (z) (make-scheme-number (real-part z))))
  (put 'project '(scheme-number)
       (lambda (n)
         (let ((r (rationalize (inexact->exact n) 1/10000)))
           (make-rational (numerator r) (denominator r)))))
  (put 'project '(rational)
       (lambda (r) (make-integer (/ (numer r) (denom r)))))
  'done)
; install-arithmetic-package-project
(define (project x)
  (apply-generic 'project x))
; project

(install-arithmetic-package-project)
; done

(project (make-complex-from-real-imag 5.5 6))
; (scheme-number . 5.5)
(project (make-scheme-number 5.5))
; (rational 11 . 2)
(project (make-rational 11 2))
; (integer . 6)
{% endhighlight %}

Now that we have successfully implemented project, let us implement the `drop` procedure.

{% highlight scheme %}
(define (drop x)
  (if (= (get-level (type-tag x)) 0)
      x
      (if (equ? (raise (project x)) x)
          (drop (project x))
          x)))
; drop

(drop (make-complex-from-real-imag 2 3))
; (complex rectangular 2 . 3)
(drop (make-complex-from-real-imag 1.5 0))
; (rational 3 . 2)
(drop (make-complex-from-real-imag 1 0))
; (integer . 1)
{% endhighlight %}

As can be seen, drop works perfectly. Now, let us integrate it into the `apply-generic` procedure.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (define (drop-type ans)
    (if (or (eq? op 'add)
            (eq? op 'sub)
            (eq? op 'mul)
            (eq? op 'div))
        (drop ans)
        ans))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-type (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
; apply-generic

(add (make-complex-from-real-imag 1.5 0)
     (make-rational 3 2))
; (integer . 3)
(add (make-complex-from-real-imag 1.5 2.3)
     (make-rational 3 2))
; (complex rectangular 3. . 2.3)
(add (make-complex-from-real-imag 1.5 2)
     (make-complex-from-real-imag 3 (- 2)))
;Value 90: (rational 9 . 2)
{% endhighlight %}

### Exercise 2.86<a id="Exercise2_86">&nbsp;</a>

In this exercise, we are told to modify complex numbers so that their components can be of any numerical type that we desire in the system. Looking at the code for computing these values, we have are required to implement these new functions for the components:- `sin`, `cos`, `atan`, `square` and `sqrt`. We implement them for the rational and real numbers and modify the packages to use generic functions.

{% highlight scheme %}
(define (install-arithmetic-package-sine)
  (put 'sine '(scheme-number)
    (lambda (s) (make-scheme-number (sin s))))
  (put 'sine '(rational)
    (lambda (r) (sine (raise (attach-tag 'rational r)))))
  (put 'sine '(integer)
    (lambda (i) (sine (raise (cons 'integer i)))))
  'done)
(define (sine x)
  (apply-generic 'sine x))

(define (install-arithmetic-package-cosine)
  (put 'cosine '(scheme-number)
    (lambda (s) (make-scheme-number (cos s))))
  (put 'cosine '(rational)
    (lambda (r) (cosine (raise (attach-tag 'rational r)))))
  (put 'cosine '(integer)
    (lambda (i) (cosine (raise (cons 'integer i)))))
  'done)
(define (cosine x)
  (apply-generic 'cosine x))

(define (install-arithmetic-package-arctan)
  (put 'arctan '(scheme-number scheme-number)
    (lambda (s1 s2) (make-scheme-number (atan s1 s2))))
  (put 'arctan '(rational)
    (lambda (r1 r2) (arctan (raise (attach-tag 'rational r1))
                            (raise (attach-tag 'rational r2)))))
  (put 'arctan '(integer)
    (lambda (i1 i2) (arctan (raise (cons 'integer i1))
		                    (raise (cons 'integer i2)))))
  'done)
(define (arctan x y)
  (apply-generic 'arctan x y))

(define (install-arithmetic-package-squared)
  (put 'squared '(scheme-number)
    (lambda (s) (make-scheme-number (square s))))
  (put 'squared '(rational)
    (lambda (r) (squared (raise (attach-tag 'rational r)))))
  (put 'squared '(integer)
    (lambda (i) (squared (raise (cons 'integer i)))))
  'done)
(define (squared x)
  (apply-generic 'squared x))

(define (install-arithmetic-package-squareroot)
  (put 'squareroot '(scheme-number)
    (lambda (s) (make-scheme-number (sqrt s))))
  (put 'squareroot '(rational)
    (lambda (r) (squareroot (raise (attach-tag 'rational r)))))
  (put 'squareroot '(integer)
    (lambda (i) (squareroot (raise (cons 'integer i)))))
  'done)
(define (squareroot x)
  (apply-generic 'squareroot x))

(install-arithmetic-package-sine)
(install-arithmetic-package-cosine)
(install-arithmetic-package-arctan)
(install-arithmetic-package-squared)
(install-arithmetic-package-squareroot)
{% endhighlight %}

Once we have defined these, we can rewrite the complex number packages. Note we have used different names so as not to overwrite default functions.

{% highlight scheme %}
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (squareroot (add (squared (real-part z))
                     (squared (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (squareroot (add (squared x) (squared y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)
{% endhighlight %}

We also modify `apply-generic` to drop type when these functions are called.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (define (drop-type ans)
    (if (or (eq? op 'add)
            (eq? op 'sub)
            (eq? op 'mul)
            (eq? op 'div)
            (eq? op 'sine)
            (eq? op 'cosine)
            (eq? op 'arctan)
            (eq? op 'squared)
            (eq? op 'squareroot))
        (drop ans)
        ans))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-type (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
{% endhighlight %}

Let us test the code:-

{% highlight scheme %}
(define y (make-complex-from-mag-ang (make-scheme-number 5.6) (make-rational 3 4)))
; y
y
; (complex polar (scheme-number . 5.6) rational 3 . 4)
(real-part y)
; (scheme-number . 4.097457665693397)
(imag-part y)
; (scheme-number . 3.817177056130671)

(define z (make-complex-from-real-imag (make-scheme-number 5.6) (make-rational 3 4)))
; z
z
; (complex rectangular (scheme-number . 5.6) rational 3 . 4)
(magnitude z)
; (scheme-number . 5.6499999999999995)
(angle z)
; (scheme-number . .13313632755164762)
{% endhighlight %}

We now get answers of the right type.

### Exercise 2.87<a id="Exercise2_87">&nbsp;</a>

In this exercise, we are tasked with implementing a `=zero?` function for polynomials that can be used to determine if a polynomial is zero. We can simply reuse the functions given in the book. Let us add that to the generic arithmetic package. 

{% highlight scheme %}
(define (variable p) (car p))
(define (term-list p) (cdr p))

(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))
(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-arithmetic-package-=zero?)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put '=zero? '(integer)
       (lambda (i) (= i 0)))

  (define (=zero?-termlist t)
    (if (empty-termlist? t)
        #t
        (and (=zero? (coeff (first-term t)))
             (=zero?-termlist (rest-terms t)))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-termlist (term-list p))))
  'done)

(define (=zero? x)
  (apply-generic '=zero? x))
{% endhighlight %}

Let us test this:-

{% highlight scheme %}
(=zero? (make-polynomial 'x '()))
; #t
(=zero? (make-polynomial 'x (list (list 0 0))))
; #t
(=zero? (make-polynomial 'x (list (list 1 0) (list 2 0))))
; #t
(=zero? (make-polynomial 'x (list (list 1 1))))
; #f
{% endhighlight %}

### Exercise 2.88<a id="Exercise2_88">&nbsp;</a>

In this exercise, we are tasked with implementing the subtraction of polynomials. Based on the hint given, we could simply negate the coefficients of the subtrahend and reuse the `add` function written for the polynomial. Let us first implement the negate function.

{% highlight scheme %}
(define (install-arithmetic-package-negate)
  (put 'negate '(scheme-number)
       (lambda (x) (* x (- 1))))
  (put 'negate '(rational)
       (lambda (r) (make-rational (- (numer r)) (denom r))))
  (put 'negate '(complex)
       (lambda (z) (make-complex-from-real-imag (- (real-part z))
                                                (- (imag-part z)))))
  (put 'negate '(integer)
       (lambda (i) (make-integer (* i (- 1)))))

  (define (negate-termlist t)
    (if (empty-termlist? t)
        ()
        (cons (make-term (order (car t)) (negate (coeff (car t))))
              (negate-termlist (cdr t)))))
  (put 'negate '(polynomial)
       (lambda (p) (make-polynomial (variable p)
                                    (negate-termlist (term-list p)))))
  'done)
; install-arithmetic-package-negate
(define (negate x)
  (apply-generic 'negate x))
; negate

(install-arithmetic-package-negate)
; install-arithmetic-package-negate

(negate 5)
; -5
(negate (make-rational 5 6))
; (rational -5 . 6)
(negate (make-complex-from-real-imag 3 4))
; (complex rectangular -3 . -4)
(negate (make-polynomial 'x (list (list 1 3) (list 2 (- 5)))))
; (polynomial x (1 -3) (2 5))
{% endhighlight %}

With the negation implemented and tested, we can extend the polynomial system to include subtraction of polynomials:-

{% highlight scheme %}
(define (install-polynomial-package-subtract)
  (define (tag p) (attach-tag 'polynomial p))
  (define (sub-poly p1 p2)
    (add (tag p1) (negate (tag p2))))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  'done)
; install-polynomial-package-subtract
(install-polynomial-package-subtract)
; done
(define (drop x)
  (if (or (eq? (type-tag x) 'polynomial )
          (= (get-level (type-tag x)) 0))
      x
      (if (equ? (raise (project x)) x)
          (drop (project x))
          x)))
; drop

(sub (make-polynomial 'x (list (list 1 3)
                               (list 4 5)))
     (make-polynomial 'x (list (list 1 2)
                               (list 2 6)
                               (list 3 (- 1)))))
; (polynomial x (1 1) (4 5) (2 -6) (3 1))
{% endhighlight %}

### Exercise 2.89<a id="Exercise2_89">&nbsp;</a>

In this exercise, we are tasked with implementing a dense representation of polynomial terms as opposed to the sparse representation provided in the book along with the required procedures. In the sparse representation, we just store the coefficients of the polynomial in the order that they appear in without the power terms.

The method proposed should maintain the same external interface as the dense representation that is used by the `polynomial` package to access the coefficients. We should also traverse the list while emitting the actual order the coefficients belong to. To do this, we need to add one more item that refers to the order of the first coefficient in the list.

{% highlight scheme %}
(define (make-term-list starting-order coefficients)
  (cons starting-order coefficients))
(define (starting-order term-list)
  (car term-list))
(define (coefficients term-list)
  (cdr term-list))
(define (strip-leading-zeros term-list)
  (if (or (empty-termlist? term-list)
          (not (=zero? (car (coefficients term-list)))))
	  term-list
	  (strip-leading-zeros
	    (make-term-list (- (starting-order term-list) 1)
		                (cdr (coefficients term-list))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
	  (if (empty-termlist? term-list)
	      (make-term-list (order term)
		                  (list (coeff term)))
	      (if (= (- (order term) 1)
	             (starting-order term-list))
		      (make-term-list (order term)
		                      (cons (coeff term)
						            (coefficients term-list)))
	          (adjoin-term term
		                   (make-term-list (+ (starting-order term-list) 1)
					                       (cons 0 (coefficients term-list))))))))
(define (the-empty-termlist)
  (make-term-list (- 1) ()))
(define (first-term term-list)
  (make-term (starting-order term-list)
             (car (coefficients term-list))))
(define (rest-terms term-list)
  (strip-leading-zeros
    (make-term-list (- (starting-order term-list) 1)
	                (cdr (coefficients term-list)))))
(define (empty-termlist? term-list)
  (null? (coefficients term-list)))
(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
{% endhighlight %}

We also place a new negation function for the dense term-list:-

{% highlight scheme %}
(define (negate-termlist t)
  (if (empty-termlist? t)
      t
      (make-term-list (starting-order t)
	                  (map negate (coefficients t)))))
(put 'negate '(polynomial)
     (lambda (p) (make-polynomial (variable p)
                                  (negate-termlist (term-list p)))))
{% endhighlight %}

Let us test it out:-

{% highlight scheme %}
(define x (the-empty-termlist))
; x
(adjoin-term (make-term 3 2) x)
; (3 2)
(adjoin-term (make-term 5 1) (adjoin-term (make-term 3 2) x))
; (5 1 0 2)
(first-term (adjoin-term (make-term 5 1) (adjoin-term (make-term 3 2) x)))
;(5 1)
(rest-terms (adjoin-term (make-term 5 1) (adjoin-term (make-term 3 2) x)))
; (3 2)

(negate (make-polynomial 'x (list 5 1 2 0 3 (- 2) (- 5))))
; (polynomial x 5 -1 -2 0 -3 2 5)
(add (make-polynomial 'x (list 5 1 2 0 3 (- 2) (- 5))) (make-polynomial 'x (list 4 1 2 3 4 5)))
; (polynomial x 5 1 (integer . 3) 2 (integer . 6) (integer . 2) 0)
{% endhighlight %}

### Exercise 2.90<a id="Exercise2_90">&nbsp;</a>

In this exercise, we are tasked with implementing a polynomial system that can work with both dense and sparse representation simultaneously. We can coerce between each representation as needed. The coefficients can either be a number or a polynomial.

Note:- Since our old number tower is beyond the scope of upcoming exercises, we stick with the simple default numbers provided in MIT-Scheme so as to keep things simple. There will be no `drop` or `raise` being done. Only direct coercion. Full capability can be achieved but required too much testing corner cases to be use for this exercise.

{% highlight scheme %}
;; Term representation. Common for dense and sparse
(define (install-terms-package)
  ;; Internal procedures
  (define (term-tag x)
    (attach-tag 'term x))
  (define (make-term order coeff) 
    (cons order coeff))
  (define (order term) (car term))
  (define (coeff term) (cdr term))
  ;; External interface
  (put 'order '(term)
       (lambda (t)
	     (order t)))
  (put 'coeff '(term)
       (lambda (t)
	     (coeff t)))
  (put 'make 'term
       (lambda (order coeff)
	     (term-tag (make-term order coeff))))
  'done)
(install-terms-package)
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))

;; Generic function definitions
(define (adjoin-term x y) (apply-generic 'adjoin-term x y))
(define (first-term x) (apply-generic 'first-term x))
(define (rest-terms x) (apply-generic 'rest-terms x))
(define (empty-termlist? x) (apply-generic 'empty-termlist? x))

(define (install-polynomial-dense-package)
  ;; Internal procedures
  (define (dense-tag x)
    (attach-tag 'dense x))
  (define (term-tag x)
    (attach-tag 'term x))
  (define (tcoeff term)
    (coeff (term-tag term)))
  (define (torder term)
    (order (term-tag term)))

  (define (dense-adjoin-term term term-list)
    (if (=zero? (tcoeff term))
        term-list
        (cons term term-list)))
  (define (dense-first-term term-list) (car term-list))
  (define (dense-rest-terms term-list) (cdr term-list))
  (define (dense-empty-termlist? term-list)
    (null? term-list))
	
  (define dense-empty-term-list (dense-tag '()))
  (define (dense-make-term-list list-of-terms)
	(if (null? list-of-terms)
	    dense-empty-term-list
		(adjoin-term (car list-of-terms)
		             (dense-make-term-list (cdr list-of-terms)))))

  (define (dense-term-list-negate term-list)
    (if (null? term-list)
	    ()
		(let ((ft (car term-list)))
		      (rt (cdr term-list)))
		  (cons
		    (contents (make-term (torder ft) (negate (tcoeff ft))))
		    (dense-term-list-negate rt)))))

  ;; External interface
  (put 'adjoin-term '(term dense)
       (lambda (t tl)
	     (dense-tag (dense-adjoin-term t tl))))
  (put 'first-term '(dense)
       (lambda (tl)
	     (term-tag (dense-first-term tl))))
  (put 'rest-terms '(dense)
       (lambda (tl)
	     (dense-tag (dense-rest-terms tl))))
  (put 'empty-termlist? '(dense)
       dense-empty-termlist?)
  (put '=zero? '(dense)
       dense-empty-termlist?)
  (put 'negate '(dense)
       (lambda (t1)
	     (dense-tag (dense-term-list-negate t1))))
  (put 'make 'dense
       (lambda (terms)
	      (dense-make-term-list terms)))
  'done)
(install-polynomial-dense-package)
(define (make-dense-term-list terms-list)
  ((get 'make 'dense) terms-list))

(define (install-polynomial-sparse-package)
  ;; Internal procedures
  (define (sparse-tag x)
    (attach-tag 'sparse x))
  (define (term-tag x)
    (attach-tag 'term x))
  (define (tcoeff term)
    (coeff (term-tag term)))
  (define (torder term)
    (order (term-tag term)))

  ;; Internal representation
  (define (make-term-list starting-order coefficients)
    (cons starting-order coefficients))
  (define (starting-order term-list)
    (car term-list))
  (define (coefficients term-list)
    (cdr term-list))

  (define (sparse-empty-termlist? term-list)
    (null? (coefficients term-list)))

  (define (strip-leading-zeros term-list)
    (if (or (sparse-empty-termlist? term-list)
            (not (=zero? (car (coefficients term-list)))))
  	    term-list
  	    (strip-leading-zeros
	      (make-term-list (- (starting-order term-list) 1)
		                  (cdr (coefficients term-list))))))
						  
  (define (sparse-adjoin-term term term-list)
  (if (=zero? (tcoeff term))
      term-list
	  (if (sparse-empty-termlist? term-list)
	      (make-term-list (torder term)
		                  (list (tcoeff term)))
	      (if (= (- (torder term) 1)
	             (starting-order term-list))
		      (make-term-list
			    (torder term)
		        (cons (tcoeff term)
				      (coefficients term-list)))
	          (adjoin-term
			    term
		        (make-term-list (+ (starting-order term-list) 1)
				                   (cons 0 (coefficients term-list))))))))

  (define sparse-empty-term-list
    (sparse-tag (make-term-list (- 1) ())))
  (define (sparse-make-term-list list-of-terms)
	(if (null? list-of-terms)
	    sparse-empty-term-list
		(adjoin-term (car list-of-terms)
		             (sparse-make-term-list (cdr list-of-terms)))))


  (define (sparse-first-term term-list)
    (make-term (starting-order term-list)
               (car (coefficients term-list))))
  (define (sparse-rest-terms term-list)
    (strip-leading-zeros
      (make-term-list (- (starting-order term-list) 1)
	                  (cdr (coefficients term-list)))))

  (define (sparse-term-list-negate t)
  (if (sparse-empty-termlist? t)
      t
      (make-term-list (starting-order t)
	                  (map negate (coefficients t)))))

  ;; External interface
  (put 'adjoin-term '(term sparse)
       (lambda (t tl)
	     (sparse-tag (sparse-adjoin-term t tl))))
  (put 'first-term '(sparse)
       sparse-first-term)
  (put 'rest-terms '(sparse)
       (lambda (tl)
	     (sparse-tag (sparse-rest-terms tl))))
  (put 'empty-termlist? '(sparse)
       sparse-empty-termlist?)
  (put '=zero? '(sparse)
       sparse-empty-termlist?)
  (put 'negate '(sparse)
       (lambda (t1)
	     (sparse-tag (sparse-term-list-negate t1))))
  (put 'make 'sparse
       (lambda (terms)
	      (sparse-make-term-list terms)))
  'done)
(install-polynomial-sparse-package)
(define (make-sparse-term-list terms-list)
  ((get 'make 'sparse) terms-list))

(define (install-polynomial-core-package)
  ;; Internal functions
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (negate (term-list p2))))
        (error "Polys not in same var: SUB-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
          (make-poly 
           (variable p1)
           (mul-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: MUL-POLY"
                 (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) 
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))

  ;; External interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
	      (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero? (term-list p))))
  (put 'negate '(polynomial)
       (lambda (p) (make-polynomial
	                 (variable p)
                     (negate (term-list p)))))
  'done)
(install-polynomial-core-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
{% endhighlight %}

Let us test it.

{% highlight scheme %}
(define p1 (make-polynomial 'x (make-dense-term-list (list (make-term 2 5) (make-term 1 4) (make-term 0 3)))))
; p1
(define p2 (make-polynomial 'x (make-sparse-term-list (list (make-term 2 5) (make-term 1 4) (make-term 0 3)))))
; p2

p1
; (polynomial x dense (2 . 5) (1 . 4) (0 . 3))
p2
; (polynomial x sparse 2 5 4 3)

(add p1 p2)
; (polynomial x sparse 2 10 8 6)
(add p2 p1)
; (polynomial x dense (2 . 10) (1 . 8) (0 . 6))

(mul p1 p2)
; (polynomial x sparse 4 25 40 46 24 9)
(mul p2 p1)
; (polynomial x dense (4 . 25) (3 . 40) (2 . 46) (1 . 24) (0 . 9))

(sub p1 p2)
; (polynomial x sparse -1)
(sub p2 p1)
; (polynomial x dense)
{% endhighlight %}

As can be seen, we can use both representations interchangably because of the similar interface they expose. Taking terms from them emits the same data structure, so coercion is not even needed. The result of mixing is of the type of the first argument.

### Exercise 2.91<a id="Exercise2_91">&nbsp;</a>

In this exercise, we are tasked with implemeting polynomial division. For that purpose, we are given a skeleton code that we should complete.

{% highlight scheme %}
(define (install-core-polynomial-package-division)
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1) (car result))
                (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var: DIV-POLY"
               (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list L1 L1)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (make-dense-term-list ()) L1)
              (let ((new-c (div (coeff t1) 
                                (coeff t2)))
                    (new-o (- (order t1) 
                              (order t2))))
                (let ((rest-of-result
                        (div-terms
                          (add-terms L1
                                     (negate
                                       (mul-term-by-all-terms
                                         (make-term new-o new-c)
                                         L2)))
                           L2)))
                  (list (add-terms (make-dense-term-list
                                     (list (make-term new-o new-c)))
                                   (car rest-of-result))
                        (cadr rest-of-result))))))))

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  'done)
{% endhighlight %}

By default, we have set things up to use the dense representation.

{% highlight scheme %}
(define p1
  (make-polynomial 'x
                   (make-dense-term-list (list (make-term 5 1)
                                               (make-term 0 (- 1))))))
; p1
(define p2
  (make-polynomial 'x
                   (make-dense-term-list (list (make-term 2 1)
                                               (make-term 0 (- 1))))))
; p2

(div p1 p2)
; ((polynomial x dense (3 . 1) (1 . 1)) (polynomial x dense (1 . 1) (0 . -1)))
{% endhighlight %}

We get the right result given in the book.
