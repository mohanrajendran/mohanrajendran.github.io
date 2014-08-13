---
layout: post
categories: [SICP, Solutions]
post_no: 2
title: SICP Section 1.1 Exercise Solutions
submenu:
  - { hook: "Exercise1_1", title: "Exercise 1.1" }
  - { hook: "Exercise1_2", title: "Exercise 1.2" }
  - { hook: "Exercise1_3", title: "Exercise 1.3" }
  - { hook: "Exercise1_4", title: "Exercise 1.4" }
  - { hook: "Exercise1_5", title: "Exercise 1.5" }
  - { hook: "Exercise1_6", title: "Exercise 1.6" }
  - { hook: "Exercise1_7", title: "Exercise 1.7" }
  - { hook: "Exercise1_8", title: "Exercise 1.8" }
---

### Exercise 1.1<a name="Exercise1_1">&nbsp;</a>

This exercise deals with evaluating some Scheme functions and reporting their output. This acts as an introduction to Scheme syntax.

{% highlight scheme %}
10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
; a
(define b (+ a 1))
; b
(+ a b (* a b))
; 19
(= a b)
; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a)
     b
     a))
; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16
{% endhighlight %}

<!--excerpt-->

### Exercise 1.2<a name="Exercise1_2">&nbsp;</a>

This exercise deals with the translation of an equation into the prefix form. The equation and expected answer are as follows:-

$$\frac{5+4+(2-(3-(6+\frac{4}{5})))}{3(6-2)(2-7)}=-\frac{37}{150}$$

The prefix form and evaluation in Scheme results in:-

{% highlight scheme %}
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
; -37/150
{% endhighlight %}

### Exercise 1.3<a name="Exercise1_3">&nbsp;</a>

We are tasked with defining a procedure which takes in 3 arguments and returns the square of the 2 largest arguments. We solve this by building the solution step by step

{% highlight scheme %}
(define (sq x) (* x x))
; sq
 
(define (sumsq x y) (+ (sq x) (sq y)))
; sumsq
 
(define (largest2sq a b c)
(cond ((and (<= a b) (<= a c)) (sumsq b c))
      ((and (<= b a) (<= b c)) (sumsq a c))
      (else (sumsq a b))))
; largest2sq
 
; Test cases:-
(largest2sq 1 2 3)
; 13
(largest2sq 1 3 2)
; 13
(largest2sq 1 1 2)
; 5
{% endhighlight %}

### Exercise 1.4<a name="Exercise1_4">&nbsp;</a>

This exercise tasks us with explaining the behavior of the following procedure.

{% highlight scheme %}
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
{% endhighlight %}

When the if function is evaluated, based on the condition, the operator + or - is applied on the operands a and b. If b > 0, + operator is used, evaluating to $$a+b$$. Otherwise, - operator is issued which yields $$a-b$$. Ultimately, we evaluate to $$a+\|b\|$$.

### Exercise 1.5<a name="Exercise1_5">&nbsp;</a>

This exercise deals with examining how Lisp would behave in appilcative-order evaluation and normal order evaluation.

{% highlight scheme %}
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
{% endhighlight %}

When we evaluate the above expression, one of the following occurs based on the evaluation order.

##### Applicative-order evaluation

{% highlight scheme %}
(test 0 (p))

(test 0 (p))

(test 0 (p))
...
{% endhighlight %}

In *applicative-order evaluation*, the expression fails to terminate. This is because each formal parameter is replaced by its corresponding argument first(operator and operands are both expanded). Thus, the interpreter tries to evaluate `(p)` first which itself evaluates to `(p)` and so on. Thus, the evaulation does not terminate.

##### Normal-order evaluation

{% highlight scheme %}
(test 0 (p))

(if (= 0 0 ) 0 (p))

(if #t 0 (p))

0
{% endhighlight %}

In *normal-order evaluation*, the operand is not evaluated until it is required to do so. Thus, the interpreted evaluates the operator *test* first. Since the condition evaluates to `#t` for the resulting if operation, only the first argument is evaluated which is 0.

Overall, MIT Scheme exhibits **applicative-order evaluation**. Thus, the evaluation never halts.

### Exercise 1.6<a name="Exercise1_6">&nbsp;</a>

This exercise deals again with the pitfall of not considering evaluation order while writing our code. For that purpose, we consider a case when the `if` function is rewritten using `cond` and running our square root finding algorithm.

{% highlight scheme %}
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
{% endhighlight %}

Rewriting the if function on our own reverts the evaluation to applicative-order. This means all the arguments are expanded and evaluated in the first run. Thus running the code produces the following:-

{% highlight scheme %}
(sqrt-iter 1.0 2)

(new-if (good-enough? 1.0 2)
        1.0
        (sqrt-iter (improve 1.0 2)
                   2))

(new-if (< (abs (- (square 1.0) 2)) 0.001)
	1.0
	(sqrt-iter (average 1.0 (/ 2 1.0))
		   2))

(new-if (< (abs (- 1.0 2)) 0.001)
 	1.0
	(sqrt-iter 1.5 2))

(new-if (< 1.0 0.001)
	1.0
	(new-if (good-enough? 1.5 2)
        	1.5
		(sqrt-iter (improve 1.5 2)
                   	   2)))

(new-if (< 1.0 0.001)
	1.0
	(new-if (< 0.25 0.001)
        	1.5
		(sqrt-iter 1.4167 2)))

...

{% endhighlight %}
Note:- `cond` is not expanded to for clarity.

Thus, in this case. The predicate, then-clause and else-clause are all evaluated first. Thus, irrespective of the condition, both clauses are evaluated. Because the else-clause contains another new-if statement, this means that the evaluation continues recursively down ad infinitum even if a terminal case is reached. Thus the evauation does not terminate.

### Exercise 1.7<a name="Exercise1_7">&nbsp;</a>

This exercise is used to teach us the short-coming of machine precision computation which can affect our computations even though the procedure is specified in a sound manner. We take a look at the square root function in this section.

The `good-enough?` function is not so effective when we use small numbers as arguments in the function.

For example, when we want to find the square root of 0.0001 which is 0.01,
{% highlight scheme %}
(sqrt 0.0001)

;.03230844833048122
{% endhighlight %}

The wrong answer is because the `good-enough?` function stops the refinement of the answer once it reaches within 0.001 of the actual answer. In this case, `(square 0.0323084) = 0.0010438` which is within 0.001 of the actual answer of 0.0001. Thus, the function evaluates to `#t`.

To test further, we try

{% highlight scheme %}
(sqrt 0.000001)

;.03135649010771716
{% endhighlight %}

which is also the wrong answer.

As for large numbers, we also see problems because of the floating point representation in the computers. As large numbers cannot be represented with the precision of 0.001 required by the `good-enough?` function, the function never returns a `#t`. For example, when we try evaluating `(sqrt 1e15)`, the system keeps evaluating without halting.

To fix these problems, we can redefine the `good-enough?` function to compare between subsequent guesses.

{% highlight scheme %}
(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) 
      		 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))
{% endhighlight %}

The `good-enough?` function above evaluates by only comparing the previous-guess with the current guess to check if there is any improvement in refinement of answer by doing any further iterations. For small numbers, it ensures that the iteration does not halt prematurely and for large numbers, it halts when it is realised that the iteration will not produce further refinement. To test, we re-run the previous tests.

{% highlight scheme %}
(sqrt 0.0001)
; 1.0000714038711746e-2

(sqrt 1e15)

; 31622776.601683795
{% endhighlight %}

### Exercise 1.8<a name="Exercise1_8">&nbsp;</a>

This exercise tasks us to use what we learnt in this section to write our own cube root function. In addition to that, we utilize the refinements we made in the previous exercise.

{% highlight scheme %}
(define (cbrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) 
      		 guess
                 x)))
; cbrt-iter

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
; improve

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))
; good-enough?

(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))
; cbrt

; Test cases:-
(cbrt 0.000001)
; 1.0039485733107844e-2
(cbrt 1e15)
; 10000.
{% endhighlight %}
