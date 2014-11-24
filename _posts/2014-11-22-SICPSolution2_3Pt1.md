---
layout: post
category: [SICP, Solutions]
post_no: 22
title: "SICP Section 2.3 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise2_53", title: "Exercise 2.53" }
   - { hook: "Exercise2_54", title: "Exercise 2.54" }
   - { hook: "Exercise2_55", title: "Exercise 2.55" }
   - { hook: "Exercise2_56", title: "Exercise 2.56" }
   - { hook: "Exercise2_57", title: "Exercise 2.57" }
---
### Exercise 2.53<a name="Exercise2_53">&nbsp;</a>

In this exercise, we are simply asked to determine the responses from the interpreter when given expressions are evauated. This is to understand the quotation mechanic.

<!--excerpt-->

1. When we evaluate `(list 'a 'b 'c)`, `a`, `b` and `c` are quoted directly, thus, we get `(a b c)` directly.

2. When we evaluate `(list (list 'george))`, `george` is quoted. Including nested lists, we expect `((george))`.

3. When we evaluate `(cdr '((x1 x2) (y1 y2)))`, we get the list with the first element removed. We expect `((y1 y2))`.

4. When we evaluate `(cadr '((x1 x2) (y1 y2)))`, we get the `car` of the previous result which is simple `(y1 y2)`.

5. When we evaluate `(pair? (car '(a short list)))`, we check if `a` is a pair which should result in `#f`.

6. When we evaluate `(memq 'red '((red shoes) (blue socks)))`, we should get `#f` since there is no member in the main list directly equal to `red`

7. When we evaluate `(memq 'red '(red shoes blue socks))`, we should get the whole list in return because the first element is the element we are looking for. We expect `(red shoes blue socks)`

Now, let us actually run it in the REPL:-

{% highlight scheme %}
(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)
{% endhighlight %}

### Exercise 2.54<a name="Exercise2_54">&nbsp;</a>

In this exercise, we are tasked with writing a procedure called `equal?` which can be used to recursively determine the equality of two lists of symbols. We are given `eq?` which evaluates to true when the two arguments are the same symbols.

{% highlight scheme %}
(define (equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
	          (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))
; equal?

(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f

(equal? '(this (is a) list) '(this (is a) list))
; #t
{% endhighlight %}

### Exercise 2.55<a name="Exercise2_55">&nbsp;</a>

When Eva Lu Ator typed `(car ''abracadabra)`, the REPL printed back `quote`. This exercise ask us to explain how.

When we type the original quoted expression in the REPL, we get the following:-

{% highlight scheme %}
''abracadabra
; (quote abracadabra)
{% endhighlight %}

Thus, by using two quotes, we also quote in the inner quote. The inner quote thus does not get evaluated and shows up as is. When we take the `car` of this list, we simply get the `quote` back.

### Exercise 2.56<a name="Exercise2_56">&nbsp;</a>

In this exercise, we are tasked with extending the `deriv` program given in the book to handle exponentiations of the form:-

$$\frac{d(u^n)}{dx} = nu^{n-1}\frac{du}{dx}$$

We can do that by first implementing the data structure for exponentiation.

{% highlight scheme %}
(define (exponent? exp)
  (and (pair? exp) (eq? (car exp) '**)))
; exponent?

(define (base exp)
  (cadr exp))
; base

(define (exponent exp)
  (caddr exp))
; exponent

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        ((number? exponent) (list '** base exponent))
		(else (error "cannot exponentiate by an expression"))))
; make-exponentiation
{% endhighlight %}

Let us now include this code in the `deriv` expression.

{% highlight scheme %}
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
        ((exponent? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))
{% endhighlight %}

Let us test this code:-

{% highlight scheme %}
(deriv '(** x 5) 'x)
; (* 5 (** x 4))

(deriv '(+ (** x 3) (** x 5)) 'x)
; (+ (* 3 (** x 2)) (* 5 (** x 4)))

(deriv '(* a (** x 5)) 'x)
; (* a (* 5 (** x 4)))
{% endhighlight %}

### Exercise 2.57<a name="Exercise2_57">&nbsp;</a>

In this exercise, we are tasked with extending the differentiation program to handle the sums and products of arbitrary number of terms.

We can do this by simply modifying the selector functions to return another sum/product expression if there are more than two terms.

{% highlight scheme %}
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
	  (cons '+ (cddr s))))
; augend

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
	  (cons '* (cddr p))))
; multiplicand
{% endhighlight %}

Let us test this:-

{% highlight scheme %}
(augend '(+ a b c))
; (+ b c)
(augend '(+ a b))
; b

(deriv '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
{% endhighlight %}

