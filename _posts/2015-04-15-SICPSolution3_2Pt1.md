---
layout: post
category: [SICP, Solutions]
post_no: 39
title: "SICP Section 3.2 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise3_9", title: "Exercise 3.9" }
---
### Exercise 3.9<a name="Exercise3_9">&nbsp;</a>

In this exercise, we are asked to show the environment structure when evaluating the two provided procedures for computing factorials.

##### Recursive solution

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
