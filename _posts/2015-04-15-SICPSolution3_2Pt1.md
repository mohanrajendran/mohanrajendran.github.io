---
layout: post
category: [SICP, Solutions]
post_no: 39
title: "SICP Section 3.2 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise3_9", title: "Exercise 3.9" }
   - { hook: "Exercise3_10", title: "Exercise 3.10" }
---
### Exercise 3.9<a name="Exercise3_9">&nbsp;</a>

In this exercise, we are asked to show the environment structure when evaluating the two provided procedures for computing factorials.

##### Recursive solution

{% endhighlight %}
The recursive code is given as follows:-

{% highlight scheme %}
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
{% endhighlight %}

<!--excerpt-->

The environment structures creating by evaluating `(factorial 6)` is as follows:-

<center>
<img src="/images/Ex3_9_Recursive.svg" alt="Environment structure for recursive case"/>
</center>

##### Iterative solution

The iterative code is given as follows:-

{% highlight scheme %}
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product 
                   counter 
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
{% endhighlight %}

<center>
<img src="/images/Ex3_9_Iterative.svg" alt="Environment structure for iterative case"/>
</center>

### Exercise 3.10<a name="Exercise3_10">&nbsp;</a>

In this exercise, we are tasked with illustrating the environment model for the given `make-withdraw` procedure as follows:-

{% highlight scheme %}
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))
{% endhighlight %}

To understand what happens, let us desugar the `let` expression:-

{% highlight scheme %}
(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
                 "Insufficient funds")))
    initial-amount))
{% endhighlight %}

First, when the initial definition of `make-withdraw` is created, the following environment is set up:-

<center>
<img src="/images/Ex3_10_Step1.svg" alt="After defining make-withdraw"/>
</center>

Next, calling `(define W1 (make-withdraw 100))` establishes the following environment after evaluating the inner `lambda`:- 

<center>
<img src="/images/Ex3_10_Step2.svg" alt="After defining W1"/>
</center>

After that, calling `(W1 50)` does the following:- 

<center>
<img src="/images/Ex3_10_Step3.svg" alt="While calling W1"/>
</center>

This sets `balance` value to 50 based on the definition of `W1` and evaluates to `50` similar to the original function. Finally, defining another function `W2` similar to `W1` ends up with the following:-

<center>
<img src="/images/Ex3_10_Step4.svg" alt="After defining W2"/>
</center>

As can be seen, both `W1` and `W2` share the same function code but point to a separate environment. Ultimately, this yields in code very similar to what we have before but has an extra `initial-amount` defined which could be used to extend the functionality of the code.
