---
layout: post
category: [SICP, Solutions]
post_no: 13
title: "SICP Section 2.1 Exercise Solutions - Part 3"
submenu:
   - { hook: "Exercise2_13", title: "Exercise 2.13" }
   - { hook: "Exercise2_14", title: "Exercise 2.14" }
   - { hook: "Exercise2_15", title: "Exercise 2.15" }
   - { hook: "Exercise2_16", title: "Exercise 2.16" }
---

### Exercise 2.13<a name="Exercise2_13">&nbsp;</a>

In this exercise, we are tasked with investigating if there is a simple formula for approximate percentage tolerance based on the tolerance of the factors.

We are told to make two assumptions:-

* The intervals are positive
* The tolerance percentages are small

<!--excerpt-->

Let us take two intervals *a* and *b* with tolerances $$T_a$$ and $$T_b$$ respectively. Multiplying them and using the rules derived earlier for upper and lower bounds for positive bounds, we get

$$\begin{align}&[a(1-T_a),a(1+T_a)] \times [b(1-T_b),b(1+T_b)]
\\ &= [a(1-T_a)\times b(1-T_b),a(1+T_a)\times b(1+T_b)]
\\ &= [ab(1-T_a - T_b+T_a T_b),ab(1+T_a + T_b + T_a T_b)]
\end{align}$$

Since we have assumed that tolerance percentages are small, we know that the value of $$T_a T_b\ll 1$$. Thus, we can ignore the term, arriving at a new interval

$$[ab(1-(T_a + T_b)),ab(1+(T_a + T_b))]$$

Tolerance of the product $$T_p = T_a + T_b$$.

### Exercise 2.14<a name="Exercise2_14">&nbsp;</a>

In this exercise we are asked to figure out what went wrong when two algebriacally equivalent forms of resistance equations

$$\frac{R_1 R_2}{R_1 + R_2}$$

and

$$\frac{1}{1/R_1 + 1/R_2}$$

give two different intervals when evaluated. Let us demonstrate this by using two resistors. $$10\Ohm$$ and $$20\Ohm$$ with 1% and 2% tolerances respectively. Let us use `par1` and `par2` given in the text.

{% highlight scheme %}
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))
; par1

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))
; par2

(define a (make-center-percent 10 1))
; a
(define b (make-center-percent 20 2))
; b

(define i1 (par1 a b))
; i1
(define i2 (par2 a b))
; i2

i1
; (6.361967213114755 . 6.9844067796610165)
i2
; (6.5776271186440685 . 6.7554098360655725)

(center i1)
; 6.673186996387885
(center i2)
; 6.6665184773548205

(percent i1)
; 4.6637353852303285
(percent i2)
; 1.3334000200059877
{% endhighlight %}

As can be seen, the two methods produce two very different intervals.

Let us see what happens when we evaluate $$a/a$$ and $$a/b$$ directly.

{% highlight scheme %}
(define aa (div-interval a a))
; aa
(define ab (div-interval a b))
; ab

aa
; (.9801980198019803 . 1.02020202020202)
ab
; (.4852941176470589 . .5153061224489796)

(center aa)
; 1.0002000200020003
(percent aa)
; 1.9998000199979906

(center ab)
; .5003001200480193
(percent ab)
; 2.9994001199759954
{% endhighlight %}

We can see that $$a/a$$ yeilds an interval which is not even centered exactly on one. This means that when we transform equations, we should not assume that the value is one directly. Thus, we start to see where things diverge in both forms of equations.

### Exercise 2.15<a name="Exercise2_15">&nbsp;</a>

In this exercise, we are told to determine the validity of the statement that the `par2` procedure gives a better result than `par1`. Let us see why this is right.

We have seen before that for every operation involving intervals, the width of the interval only increases for all operation and never decreases. Thus, it always makes sense to reduce the overall interval by not involving any interval with a non-zero width. For example the expression $$1/R_1$$ is preferable to an equivalent expression $$R_1 / {R_1}^2$$ simply because of the extra errors being contributed to the resulting interval.

Thus, when we take the expression used in `par1`,

$$\frac{R_1 R_2}{R_1 + R_2}$$

we can see that we have 3 operations involving intervals of non-zero width.

Whereas for the expression used in `par2`,

$$\frac{1}{1/R_1 + 1/R_2}$$

we can see that most operations involve a pure number 1, which can be considered an interval of width 0. The only operation involving two intervals of non-zero width is the addition of $$1/R_1$$ and $$1/R_2$$. Thus, the result of this procedure has a tighter error bound.

### Exercise 2.16<a name="Exercise2_16">&nbsp;</a>

In this exercise, we are tasked with determining if an interval package can be written that can minimize error bounds in interval operations by choosing the appropriate equivalent algebraic form.

In general, equivalent algebraic expressions lead to different answers because of the complex relationship between the results of an interval operation and its arguments. Thus, determining the overall interval of a resulting complex expression gets very difficult.

Looking at the wikipedia article [here](http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem). We see that this indeed is the case, but we can reduce error by ensuring that each interval appears only once in the expression. This problem is not an easy one to solve for an arbitrary given expression as it might involve symbolic manipulation.

Thus, the best solution is to use expressions which does not have variables repeating.
