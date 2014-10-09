---
layout: post
category: [SICP, Solutions]
post_no: 7
title: "SICP Section 1.3 Exercise Solutions - Part 1"
submenu:
  - { hook: "Exercise1_29", title: "Exercise 1.29" }
---

### Exercise 1.29<a name="Exercise1_29">&nbsp;</a>

In this exercise, we are tasked with implementing the Simposon's rule to compute integrals.

$$\int_a^b f(x) \,dx = \frac{h}{3}(y_0+4y_1+2y_2+4y_3+2y_4+...+2y_{n-2}+4y_{n-1}+y_n)$$

where $$h = (b-a)/n$$, $$n$$ is even and $$y_k=f(a+kh)$$.

To make it amenable to be solved using the given `sum` function, we can transform the equation to

$$\int_a^b f(x) \,dx = \frac{h}{3}[(y_0-y_n)+(4y_1+2y_2)+(4y_3+2y_4)+...+(4y_{n-1}+2y_n)]$$
<!--excerpt-->

Translating this to Scheme, we arrive at:-

{% highlight scheme %}
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
; sum

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (inc2 x)
    (+ x 2))
  (define (term k)
    (+ (* 4 (y k))
       (* 2 (y (+ k 1)))))
  (* (/ h 3.0)
     (+ (f a)
        (- (f b))
		(sum term 1 inc2 n))))
; simpson
{% endhighlight %}

Comparing with the given `integral` function in the book, we get the following:-

{% highlight scheme %}
(integral cube 0 1 0.01)
; .24998750000000042
(integral cube 0 1 0.001)
; .249999875000001
(simpson cube 0 1 2)
; .25
(simpson cube 0 1 100)
; .25
(simpson cube 0 1 1000)
; .25
{% endhighlight %}

As can be seen, we get the correct answer with just two points using Simpson's rule compared to the trapezoidal rule which only approaches the answer as more points are added. This is because Simpson's rule is a higher order approximation compared to the trapezoidal rule. [Wiki Link](http://en.wikipedia.org/wiki/Newton–Cotes_formulas)
