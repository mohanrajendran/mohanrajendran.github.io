---
layout: post
category: [SICP, Solutions]
post_no: 5
title: "SICP Section 1.2 Exercise Solutions - Part 2"
submenu:
  - { hook: "Exercise1_20", title: "Exercise 1.20" }
---

### Exercise 1.20<a name="Exercise1_20">&nbsp;</a>

In this exercise, we revisit the applicative and normal-order evaluation strategies presented in [Section 1.1]({% post_url 2014-07-27-SICPSection1_1 %}) to trace the given function and determine how many times `remainder` is called.

{% highlight scheme %}
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
{% endhighlight %}

#### Normal-order evaluation

In normal-order evaluation, we fully expand first before evaluating.

{% highlight scheme %}
(gcd 206 40)

(if (= 40 0)
    206
	(gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

; 1 evaluation (remainder 206 40) => 6
(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
	
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))))

; 2 evaluations (remainder 40 (remainder 206 40))
;            => (remainder 40 6) => 4
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))

(if (= (remainder (remainder 206 40)
                  (remainder 40 (remainder 206 40))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

; 4 evaluations (remainder (remainder 206 40)
;                          (remainder 40 (remainder 206 40)))
;            => (remainder 6 4) => 2
(if (= 2 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

; 7 evaluations (remainder (remainder 40 (remainder 206 40))
;                          (remainder (remainder 206 40)
;                                     (remainder 40 (remainder 206 40))))
;            => (remainder 4 2) => 0
(if (= 0 0) 
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206
                                                                40)))))))

(remainder (remainder 206 40)
           (remainder 40 (remainder 206 40)))

; 4 evaluations
2

{% endhighlight %}

As can be seen, the overly long evaluation chain evaluates `remainder` 18 times.

#### Applicative-order evaluation

In applicative order evaluation, we evaluate available sub-expressions before expanding.

{% highlight scheme %}
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

; 1 evaluation
(gcd 40 6)

(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6))

; 1 evaluation
(gcd 6 4)

(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4))

; 1 evaluation
(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

(gcd 2 (remainder 4 2))

; 1 evaluation
(gcd 2 0)

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

2
{% endhighlight %}

As can be seen, `remainder` is only called 4 times in the applicative order.

### Exercise 1.21<a name="Exercise1_21">&nbsp;</a>

This exercise tasks to find the smallest divisors of 199, 1999 and 19999 using the given `smallest-divisor` function given in the book.

{% highlight scheme %}
(define (smallest-divisor n)
  (find-divisor n 2))
; smallest-divisor

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))
; find-divisor

(define (divides? a b)
  (= (remainder b a) 0))
; divides?

(smallest-divisor 199)
; 199
(smallest-divisor 1999)
; 1999
(smallest-divisor 19999)
; 7
{% endhighlight %}

### Exercise 1.22<a name="Exercise1_22">&nbsp;</a>

In this exercise, we try to measure the time taken for the `prime?` function to execute and verify that it indeed follows $$O(\sqrt{n})$$ time complexity. For that purpose, we use the built-in `runtime` function which gives the number of seconds since the start of execution. The given timing code is built on top of the code used in previous exercise as follows:-

{% highlight scheme %}
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
{% endhighlight %}

We now write a function that gives the primes in a range. And prints out the time taken to test a number.

{% highlight scheme %}
(define (search-for-primes start end)
        (cond ((even? start) (search-for-primes (+ start 1) end))
              ((< start end) (timed-prime-test start)
                             (search-for-primes (+ start 2) end))))
; search-for-primes

(define (even? n) (= (remainder n 2) 0))
; even?
{% endhighlight %}

The process of finding the prime numbers near the given range of 1000 to 1000000 was too quick to measure using the `runtime` function in the 2.2 GHz Intel Core 2 Duo system running Ubuntu 12.04. Thus, larger number ranges of 1e10 to 1e13 were used for this purpose.

Removing the lines for non-prime numbers and truncating after 3 prime numbers are generated, the following results were obtained:-

{% highlight scheme %}
(search-for-primes 10000000000 (+ 10000000000 100))
; 10000000019. *** .3400000000000034
; 10000000033. *** .3499999999999943
; 10000000061. *** .3400000000000034

(search-for-primes 100000000000 (+ 100000000000 100))

; 100000000003. *** 1.0900000000000034
; 100000000019. *** 1.0900000000000034
; 100000000057. *** 1.0799999999999983

(search-for-primes 1000000000000 (+ 1000000000000 100))

; 1000000000039. *** 3.490000000000009
; 1000000000061. *** 3.4299999999999926
; 1000000000063. *** 3.430000000000007

(search-for-primes 10000000000000 (+ 10000000000000 100))

; 10000000000037. *** 10.920000000000002
; 10000000000051. *** 10.920000000000002
; 10000000000099. *** 10.879999999999995
{% endhighlight %}

We take the median value of the three results for each query and compare the ratios.

{% highlight scheme %}
(/ 1.09 0.34)
; 3.2058823529411766
(/ 3.43 1.09)
; 3.146788990825688
(/ 10.92 3.43)
; 3.183673469387755
{% endhighlight %}

The ratio of time taken when input is multiplied by 10 is given to be approximately equal to $\sqrt{10} = 3.16227$. Thus, we can say that the algorithm has a time complexity of $O(\sqrt{n})$.

### Exercise 1.23<a name="Exercise1_23">&nbsp;</a>

In this exercise, we modify the code used in the previous exercise such that the time taken for the `smallest-divisor` function is halved by using only the odd numbers.

For that purpose, we replace the `find-divisor` function with the following:-

{% highlight scheme %}
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
; find-divisor

(define (next n)
  (if (= n 2) 3
      (+ n 2)))
; next
{% endhighlight %}

Rerunning the same code as before, we get the following results:-

{% highlight scheme %}
(search-for-primes 10000000000 (+ 10000000000 100))
; 10000000019. *** .19999999999998863
; 10000000033. *** .18999999999999773
; 10000000061. *** .20000000000001705

(search-for-primes 100000000000 (+ 100000000000 100))

; 100000000003. *** .6200000000000045
; 100000000019. *** .6199999999999761
; 100000000057. *** .6299999999999955

(search-for-primes 1000000000000 (+ 1000000000000 100))

; 1000000000039. *** 1.960000000000008
; 1000000000061. *** 1.9799999999999898
; 1000000000063. *** 2.

(search-for-primes 10000000000000 (+ 10000000000000 100))

; 10000000000037. *** 6.2900000000000205
; 10000000000051. *** 6.25
; 10000000000099. *** 6.310000000000002
{% endhighlight %}

Once again, we take the median time value and compute the ratio

{% highlight scheme %}
(/ 0.34 0.2)
; 1.7
(/ 1.09 0.62)
; 1.7580645161290325
(/ 3.43 1.98)
; 1.7323232323232325
(/ 10.92 6.29)
; 1.7360890302066772
{% endhighligt %}

As we can see, instead of the expected ratio of 2, we instead get an ratio of 1.73. This is probably because of the extra overhead incurred with replacing a simple `(+ test-divisor 1)` with a more complex `next` which involves evaluating an `if` statement. This extra overhead prevents us from reaching a ratio of 2.

