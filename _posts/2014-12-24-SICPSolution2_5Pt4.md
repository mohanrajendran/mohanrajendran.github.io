---
layout: post
category: [SICP, Solutions]
post_no: 33
title: "SICP Section 2.5 Exercise Solutions - Part 4"
submenu:
   - { hook: "Exercise2_87", title: "Exercise 2.87" }
   - { hook: "Exercise2_88", title: "Exercise 2.88" }
---
### Exercise 2.87<a name="Exercise2_87">&nbsp;</a>

In this exercise, we are tasked with implementing a `=zero?` function for polynomials that can be used to determine if a polynomial is zero. We can simply reuse the functions given in the book. Let us add that to the generic arithmetic package. 

<!--excerpt-->

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

### Exercise 2.88<a name="Exercise2_88">&nbsp;</a>

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

