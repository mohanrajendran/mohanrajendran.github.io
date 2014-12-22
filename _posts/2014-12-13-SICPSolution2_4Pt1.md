---
layout: post
category: [SICP, Solutions]
post_no: 27
title: "SICP Section 2.4 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise2_73", title: "Exercise 2.73" }
---
### Exercise 2.73<a name="Exercise2_73">&nbsp;</a>

In this exercise, we deal with the reimplementation of the `deriv` program that we had worked on before to use the the data-derected dispatch.
<!--excerpt-->

We are given the following code which does that:-

{% highlight scheme %}
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
{% endhighlight %}

##### Part-1
The first part of the question asks us to describe what is happening in the code above and why the predicates for `number?` and `variable?` cannot be combined into the data-direction dispatch described in this section.

In the code above, when given an expression, we first use the built-in function `number?` to check if the expression is a number and return 0. Then, when given a symbol, we check if it is the same as the variable with we are differentiating with respect to and returns 0 or 1. Finally, we user the `operator` function to obtain the operator of the expression which can be `'+`, or `'*` as we had seen before. We then use the `get` function to look up the exact `deriv` procedure define for the particular expression and apply them to the expression.

Now, we are also asked why the number and variable branches cannot be merged. The reason for that is because these expressions are the primitive parts which do not have a type tag. So there is no dispatching based on the operator type. They are therefore they are dispatched based on their data type.

##### Part-2
In the second part of the exercise, we are told to write the procedures for derivatives of sums and products along with the code to install them in the table to be used by the given code. For that purpose, let us implement a the dispatch table using the built-in `hashtable` which supports the `put` and `get` operations.

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

(define dispatch-table (make-hash-table equal?))
; dispatch-table

(define (put op type item)
  (hash-table/put! dispatch-table (list op type) item))
; put
(define (get op type)
  (hash-table/get dispatch-table (list op type) #f))
; get

(put 'hello 'world 5)
; Unspecified return value
(get 'hello 'world)
; 5
{% endhighlight %}

With the dispatch table in hand, let us tackle the next problem at hand.

{% highlight scheme %}
(define (install-deriv-package)
  ;; internal function
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  ;; internal procedures for sums
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  ;; internal procedures for products
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (prod-deriv operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  
  ;; interface to the rest of the system
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* prod-deriv))
; install-deriv-package

(define variable? symbol?)
; variable?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
; same-variable?
{% endhighlight %}

Using the above code, we initialize everything the `deriv` program needs. We can install the package by calling `install-deriv-package`. Let us see test it.

{% highlight scheme %}
(install-deriv-package)
; Unspecified return value

(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
{% endhighlight %}

##### Part-3
In the third part of the exercise, we are told to create an additional package such as one for differentiating exponents and install it into the system. Let us write the code which does that.

{% highlight scheme %}
(define (install-deriv-package-exponentiation)
  ;; internal function
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))

  ;; internal procedures for exponents
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent)) (expt base exponent))
          ((number? exponent) (list '** base exponent))
          (else (error "cannot exponentiate by an expression"))))
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (exp-deriv operands var)
    (make-product
      (make-product
        (exponent operands)
        (make-exponentiation (base operands) (- (exponent operands) 1)))
      (deriv (base operands) var)))

  ;; interface to the rest of the system
  (put 'deriv '** exp-deriv))
; install-deriv-package-exponentiation

(install-deriv-package-exponentiation)
;Unspecified return value

(deriv '(** x 5) 'x)
; (* 5 (** x 4))
{% endhighlight %}

By simply installing the new functions into the dispatch table, we extend the functionality of the `deriv` system. Note that we have retyped some internal functions such as `=number?` and `make-product`. The constructors need to be implemented globally so that they can be reused. Further, any upgrade to these procedures can be done in one location and the changes can propogate consistently.

##### Part-4
In the final part of the exercise, we ae asked how we can change the order of indexing in `get` by using the following code instead.

{% highlight scheme %}
((get (operator exp) 'deriv) 
 (operands exp) var)
{% endhighlight %}

We can utilize the function as is by changing the order in which the index to the hash-table is constructed in the `get` function.

{% highlight scheme %}
(define (get type op)
  (hash-table/get dispatch-table (list op type) #f))
{% endhighlight %}

By changing the order in the function signature, this can be implemented efficiently. Thus, this layer of abstraction lets us make modifications.

