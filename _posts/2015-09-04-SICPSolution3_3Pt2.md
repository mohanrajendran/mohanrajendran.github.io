---
layout: post
category: [SICP, Solutions]
post_no: 42
title: "SICP Section 3.3 Exercise Solution Part 2"
submenu:
- { hook: "Exercise 3_15", title: "Exercise 3.15" }
- { hook: "Exercise 3_16", title: "Exercise 3.16" }
- { hook: "Exercise 3_17", title: "Exercise 3.17" }
- { hook: "Exercise 3_18", title: "Exercise 3.18" }
---

### Exercise 3.15<a name="Exercise3_15">&nbsp;</a>

In this exercise, we are tasked with explaining the difference in behavior when a function `set-to-wow!` is applied to two seemingly similar lists:-

{% highlight scheme %}
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 
  (cons (list 'a 'b) (list 'a 'b)))

z1
; ((a b) a b)
z2
; ((a b) a b)

(set-to-wow! z1)
; ((wow b) wow b)
(set-to-wow! z2)
; ((wow b) a b)
{% endhighlight %}

<!--excerpt-->

Even though *z1* and *z2* looked the same initially, calling `set-to-wow!` on them produces different results. They can be explained by the following box and pointer diagrams resulting from the application:-

<center>
<img src="/images/Ex3_15.svg" alt="After application of set-to-wow!"/>
</center>

As can be seen, both `car` and `cdr` of *z1* point to the same object. Thus, mutating one changes the other. As for *z2*, we had two exact same copies pointing to same elements down the road. Calling `set-to-wow!` on this only modified one copy, leaving the other alone.

### Exercise 3.16<a name="Exercise3_16">&nbsp;</a>

In this exercise, we are told to examine the following `count-pairs` function written by Ben Bitdiddle:-

{% highlight scheme %}
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
{% endhighlight %}
	 
On the outset, we can see that this function will not work correctly because it does not consider pairs that might be pointed to from more than one location. They will be counted more than once. We have seen such cases in the previous exercises. Let us construct cases each with three pairs which give the required answers.

#### 3 pairs

This list can be constructed simply by a linear list which yields the following structure:-

<center>
<img src="/images/Ex3_16_3Pairs.svg" alt="Evaluate to 3 pairs"/>
</center>

{% highlight scheme %}
(define w '(a b c))

(count-pairs w)
; 3
{% endhighlight %}

As can be seen, in this simple case, the algorithm gives the right answer.

#### 4 pairs

This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_4Pairs.svg" alt="Evaluate to 4 pairs"/>
</center>

When evaluated, *x1* is counted twice and yields 4.

{% highlight scheme %}
(define x1 (cons 'a 'b))
(define x2 (cons x1 'c))
(define x (cons x1 x2))

(count-pairs x)
; 4
{% endhighlight %}

#### 7 pairs

This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_7Pairs.svg" alt="Evaluate to 7 pairs"/>
</center>

In this case, *y2* is counted twice and *y1* is counted 4 times to yield 7.

{% highlight scheme %}
(define y1 (cons 'a 'b))
(define y2 (cons y1 y1))
(define y (cons y2 y2))

(count-pairs y)
; 7 
{% endhighlight %}

#### Unlimited pairs

This list can be constructed by introducing a cycle:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="Evaluate to unlimited pairs"/>
</center>

The code would keep evaluating along the cycle and never return at all.

{% highlight scheme %}
(define z (make-cycle '(a b c)))

(count-pairs z)
; Aborting!: maximum recursion depth exceeded
{% endhighlight %}



### Exercise 3.17<a name="Exercise3_17">&nbsp;</a>

In this exercise, we are required to create a correct version of `create-pairs` which returns the correct number of distinct pairs. One good way to do it is to maintain a list of pairs already encountered and only count pairs not seen before. One way is as follows:-

{% highlight scheme %}
(define (count-pairs x)
  (define encountered '())
  (define (count-unique-pairs x)
    (if (and (pair? x)
	     (not (memq x encountered)))
	(begin (set! encountered (cons x encountered))
	       (+ (count-unique-pairs (car x))
		  (count-unique-pairs (cdr x))
		  1))
	0))
  (count-unique-pairs x))
{% endhighlight %}

Let us test it on the lists defined in the previous exercise:-

{% highlight scheme %}
w
; (a b c)
(count-pairs w)
; 3

x
; ((a . b) (a . b) . c)
(count-pairs x)
; 3

y
; (((a . b) a . b) (a . b) a . b)
(count-pairs y)
; 3

(count-pairs z)
; 3
{% endhighlight %}

### Exercise 3.18<a name="Exercise3_18">&nbsp;</a>

