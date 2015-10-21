---
layout: post
category: [SICP, Solutions]
post_no: 41
title: "SICP Section 3.3 Exercise Solutions Part 1"
submenu:
   - { hook: "Exercise3_12", title: "Exercise 3.12" }
   - { hook: "Exercise3_13", title: "Exercise 3.13" }
   - { hook: "Exercise3_14", title: "Exercise 3.14" }
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

{% highlight scheme %}
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


### Exercise 3.13<a name="Exercise3_13">&nbsp;</a>

In this exercise, we are tasked with examining the following procedure `make-cycle` and the results of executing the following code:-

{% highlight scheme %}
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b' 'c)))
{% endhighlight %}

It results in the following box and pointer diagram as follows:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="After making cycle"/>
</center>

As can be seen, the list starting at *z* does not have a `nil` pointer anywhere. Thus, if we call `(last-pair z)`, we expect the execution to go on forever because of the cycle we have created. It is verified by calling it in the scheme REPL. The code keeps running.


### Exercise 3.14<a name="Exercise3_14">&nbsp;</a>

In this exercise, we are tasked with understanding the following `mystery` function:-

{% highlight scheme %}
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
{% endhighlight %}

This function takes a list and evaluates to its reverse. The box and pointer diagram for *v* where `(define v (list 'a 'b 'c 'd))` is as follows:-

<center>
<img src="/images/Ex3_14_Initial.svg" alt="After defining v"/>
</center>

Now, when we call `(define w (mystery v))`, the following evaluation happens:-

{% highlight scheme %}
(loop '(a b c d) '())

; x = '(a b c d) y = '() temp = '(b c d)
((set-cdr! x y)
 (loop temp x))

; x = '(b c d) y = '(a) temp = '(c d)
((set-cdr! x y)
 (loop temp x))

; x = '(c d) y = '(b a) temp = '(d)
((set-cdr! x y)
 (loop temp x))

; x = '(d) y = '(c b a) temp = '()
((set-cdr! x y)
 (loop temp x))

; x = '() y = '(d c b a)
y
{% endhighlight %}

The final state of the pointers look like the following:-

<center>
<img src="/images/Ex3_14_Fine.svg" alt="After defining w"/>
</center>

Due to the mutating nature of this function, the original value of *v* also changes. Let us verify that:-

{% highlight scheme %}
(define v (list 'a 'b 'c 'd))
; v

(define w (mystery v))
; w

v
; (a)

w
; (d c b a)
{% endhighlight %}

