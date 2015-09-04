---
layout: post
category: [SICP, Solutions]
post_no: 41
title: "SICP Section 3.3 Exercise Solutions Part 1"
submenu:
   - { hook: "Exercise 3_12", title: "Exercise 3.12" }
---

### Exercise 3.12<a name="Exercise3_12">&nbsp;</a>

In this exercise, we are tasked with investigating a mutable version of the `append` function seen before called `append!` as defined below:-


{% highlight scheme %}
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
{% endhighlight %}

<!--excerpt-->

The problem is set up by calling the following:-

{% highlight scheme %}
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
; (a b c d)
{% endhighlight %}

Appending by definition makes a copy of the first argument and appends the existing second argument to the end of this copied list. Thus, the box and pointer diagram at this point is as follows:-

<center>
<img src="/images/Ex3_12_Step1.svg" alt="After defining z" width="500"/>
</center>

Next, we are told to call `(cdr x)`. As can be seen from the box and pointer diagram, we expect to see `(b)` since the original list is not modified. This can be verified by the following code:-

{% highlight scheme %}
(cdr x)
; (b)
{% endhighlight %}

Next, we set things up using the new code:-

{% highlight language %}
(define w (append! x y))

w
; (a b c d)
{% endhighlight %}

This appends in-place and thus modifies its arguments. The box and pointer diagram changes as follows:-

<center>
<img src="/images/Ex3_12_Step2.svg" alt="After defining w" width="500"/>
</center>

This modifies the value of `x` as well. Thus, calling `(cdr x)` at this point should yield `(b c d)`. Let us verify this:- 

{% highlight scheme %}
(cdr x)
; (b c d)
{% endhighlight %}
