---
layout: post
category: [SICP, Solutions]
post_no: 9
title: "SICP Section 1.3 Exercise Solutions - Part 3"
submenu:
  - { hook: "Exercise1_40", title: "Exercise 1.40" }
  - { hook: "Exercise1_41", title: "Exercise 1.41" }
  - { hook: "Exercise1_42", title: "Exercise 1.42" }
  - { hook: "Exercise1_43", title: "Exercise 1.43" }
  - { hook: "Exercise1_44", title: "Exercise 1.44" }
  - { hook: "Exercise1_45", title: "Exercise 1.45" }
---

### Exercise 1.40<a name="Exercise1_40">&nbsp;</a>

This exercise tasks us with creating a function `cubic` which can be used in conjunction with `newtons-method` to determine the zeroes of the cubic $$x^3+ax^2+bx+c$$. This problem is fairly simple.

{% highlight scheme %}
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))
{% endhighlight %}

This can be used by Newton's method to determine the roots.

<!--excerpt-->

### Exercise 1.41<a name="Exercise1_41">&nbsp;</a>

In this exercise, we are required to write a procedure `double` which takes a function and applies it twice. It can be done easily too.

{% highlight scheme %}
(define (double f)
  (lambda (x) (f (f x))))
{% endhighlight %}

Now, when we perform the following procedure,

{% highlight scheme %}
(((double (double double)) inc) 5)

(((double double double double) inc) 5)
{% endhighlight %}

Doubling 4 times, means `inc` would be applied $$2^4=16$$ times. The expected answer would be 21. To verify,

{% highlight scheme %}
(((double (double double)) inc) 5)
; 21
{% endhighlight %}

### Exercise 1.42<a name="Exercise1_42">&nbsp;</a>

In this exercise, we are tasked with writing a `compose` function that takes two one-argument functions *f* and *g* and gives a compound function $$x\mapsto f(g(x))$$

{% highlight scheme %}
(define (compose f g)
  (lambda (x) (f (g x))))
{% endhighlight %}

To test it, we try the given example in Scheme REPL.

{% highlight scheme %}
((compose square inc) 6)
; 49
{% endhighlight %}

### Exercise 1.43<a name="Exercise1_43">&nbsp;</a>

In this exercise, we are tasked with the writing a function `repeated` which would take a single argument function *f* and a number *n* and return a compound function $$x\mapsto f(f(...(f(x))))$$ n times. The procedure for this can be written using the same pattern as the quick exponentiation function from the previous sections as well as the `compose` and `double` functions from the previous exercises.

{% highlight scheme %}
(define (compose f g)
  (lambda (x) (f (g x))))
; compose

(define (double f) (compose f f))
; double

(define (even? n) (= (remainder n 2) 0))
; even?

(define (repeated f n)
  (cond ((= n 1) f)
        ((even? n)
         (double (repeated f (/ n 2))))
        (else
         (compose f (repeated f (- n 1))))))
; repeated
{% endhighlight %}

To test it, we run through the example cases.

{% highlight scheme %}
((repeated square 2) 5)
; 625
((repeated inc 100) 5)
; 125
{% endhighlight %}

### Exercise 1.44<a name="Exercise1_44">&nbsp;</a>

In this exercise, we are required to create a way of *smoothing* functions repeatedly. Applying smoothing to a function *f* produces a new function *g* such that $$g(x) = [f(x-dx) + f(x) + f(x+dx)]/3$$. Smoothing can be performed repeatedly as well. We utilize the `repeated` function from the previous exercise to accomplish this. We keep a value of $$dx = 0.00001$$ as used in the book.

{% highlight scheme %}
(define dx 0.00001)
; dx

(define (smoothe f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))
; smoothe

(define (n-fold-smoothe f n)
  ((repeated smoothe n) f))
; n-fold-smoothe
{% endhighlight %}

To test this function, we take the `sin` function. We get the following numbers for x around 1.

| x       | sin(x)       | 1-damp       | 2-damp       | 3-damp       |
|---------|--------------|--------------|--------------|--------------|
| 0.99997 | 0.8414547754 |              |              |              |
| 0.99998 | 0.8414601786 | 0.8414601786 |              |              |
| 0.99999 | 0.8414655817 | 0.8414655817 | 0.8414655817 |              |
| 1       | 0.8414709848 | 0.8414709848 | 0.8414709848 | 0.8414709847 |
| 1.00001 | 0.8414763878 | 0.8414763878 | 0.8414763877 |              |
| 1.00002 | 0.8414817907 | 0.8414817907 |              |              |
| 1.00003 | 0.8414871935 |              |              |              |

Let us test using our code.

{% highlight scheme %}
((n-fold-smoothe sin 1) 1)
; .8414709847798475
((n-fold-smoothe sin 2) 1)
; .8414709847517985
((n-fold-smoothe sin 3) 1)
; .8414709847237495
{% endhighlight %}

As can be seen, correct answers are obtained.

### Exercise 1.45<a name="Exercise1_45">&nbsp;</a>

