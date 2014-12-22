---
layout: post
category: [SICP, Solutions]
post_no: 31
title: "SICP Section 2.5 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_81", title: "Exercise 2.81" }
   - { hook: "Exercise2_82", title: "Exercise 2.82" }
   - { hook: "Exercise2_83", title: "Exercise 2.83" }
   - { hook: "Exercise2_84", title: "Exercise 2.84" }
---
### Exercise 2.81<a name="Exercise2_81">&nbsp;</a>

This exercise serves to introduce the concept of coercion to us. Louis believes that we need to add functions to coerce arguments of each type to their own type ie. an identity function. With the identity functions installed, we are asked to explain its effects.

<!--excerpt-->

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
                           (list op type-tages))
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

### Exercise 2.82<a name="Exercise2_82">&nbsp;</a>

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

### Exercise 2.83<a name="Exercise2_83">&nbsp;</a>

In this exercise, we are asked to design a procedure that raises the type of the data and install them. We use `scheme-number` to replace `real`.

{% highlight scheme %}
(define (install-arithmetic-package-raise)
  (put 'raise '(integer)
       (lambda (n) (make-rational n 1)))
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (exact->inexact (/ (numer r) (denom r))))))
  (put 'raise '(scheme-number)
       (lambda (n) (make-from-real-imag n 0)))
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
