---
layout: post
category: [SICP, Solutions]
post_no: 30
title: "SICP Section 2.5 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise2_77", title: "Exercise 2.77" }
   - { hook: "Exercise2_78", title: "Exercise 2.78" }
   - { hook: "Exercise2_79", title: "Exercise 2.79" }
   - { hook: "Exercise2_80", title: "Exercise 2.80" }
---
### Exercise 2.77<a name="Exercise2_77">&nbsp;</a>

In this exercise, Louis Reasoner tries to apply the `magnitude` function to a `complex` object given in the section. However, it fails and Alyssa tells him to install the following package to make things work:-

<!--excerpt-->

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

### Exercise 2.78<a name="Exercise2_78">&nbsp;</a>

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
                      TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
                      CONTENTS" datum))))
{% endhighlight %}

Thus, by using primitive predicate function `number?`, we do not need to tag numbers.

### Exercise 2.79<a name="Exercise2_79">&nbsp;</a>

In this exercise, we are tasked with implementing `equ?` which can be used as a generic function to test the equality of two numbers which can be ordinary numbers, rational numbers or complex numbers.

{% highlight scheme %}
(define (install-arithmetic-package-equ?)
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
       (lambda (x y) (= (* (numer x) (denom y))
                        (* (numer y) (denom x)))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y))))))

(define (equ? x y)
  (apply-generic 'equ? x y))
{% endhighlight %}

Installing the above code would allow `equ?` to compare the three types of numbers.

### Exercise 2.80<a name="Exercise2_80">&nbsp;</a>

Similar to `equ?` in the previous exercise, we are told to implement `=zero?`, a generic function that checks if an ordinary number, a rational number or a complex number is zero.

{% highlight scheme %}
(define (install-arithmetic-package-=zero?)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0))))

(define (=zero? x)
  (apply-generic '=zero? x))
{% endhighlight %}

Now, we can check if any type of number is zero.
