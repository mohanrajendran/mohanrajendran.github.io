---
layout: post
category: [SICP, Solutions]
post_no: 5
title: "SICP Section 1.2 Exercise Solutions - Part 2"
submenu:
  - { hook: "Exercise1_20", title: "Exercise 1.20" }
  - { hook: "Exercise1_21", title: "Exercise 1.21" }
  - { hook: "Exercise1_22", title: "Exercise 1.22" }
  - { hook: "Exercise1_23", title: "Exercise 1.23" }
  - { hook: "Exercise1_24", title: "Exercise 1.24" }
  - { hook: "Exercise1_25", title: "Exercise 1.25" }
  - { hook: "Exercise1_26", title: "Exercise 1.26" }
  - { hook: "Exercise1_27", title: "Exercise 1.27" }
  - { hook: "Exercise1_28", title: "Exercise 1.28" }
---

### Exercise 1.20<a name="Exercise1_20">&nbsp;</a>

In this exercise, we revisit the applicative and normal-order evaluation strategies presented in [Section 1.1]({% post_url 2014-07-27-SICPSection1_1 %}) to trace the given function and determine how many times `remainder` is called.

{% highlight scheme %}
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
{% endhighlight %}

<!--excerpt-->

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
; prime?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
; timed-prime-test

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
; start-prime-test

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
; report-prime
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

The ratio of time taken when input is multiplied by 10 is given to be approximately equal to $$\sqrt{10} = 3.16227$$. Thus, we can say that the algorithm has a time complexity of $$O(\sqrt{n})$$.

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
{% endhighlight %}

As we can see, instead of the expected ratio of 2, we instead get an ratio of 1.73. This is probably because of the extra overhead incurred with replacing a simple `(+ test-divisor 1)` with a more complex `next` which involves evaluating an `if` statement. This extra overhead prevents us from reaching a ratio of 2.

### Exercise 1.24<a name="Exercise1_24">&nbsp;</a>

In this exercise, we are tasked to compute the time complexity of the `fast-prime?` function similar to the previous exercises. For that purpose, the following timing code is used:-
{% highlight scheme %}
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))
; expmod

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
; fermat-test

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
; fast-prime?
  
(define (prime? n) 
  (fast-prime? n 10000))
; prime?

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
; timed-prime-test

(define (start-prime-test n start-time)
  (if (prime? n) 
      (report-prime n (- (runtime) start-time))))
; start-prime-test
  
(define (report-prime n elapsed-time) 
  (newline) 
  (display n) 
  (display " *** ") 
  (display elapsed-time))
; report-prime
{% endhighlight %}

For our exercise, 10000 trials were used for testing. The following times were observed.

{% highlight scheme %}
(timed-prime-test 1009)
; 1009 *** .3400000000000034
(timed-prime-test 1000003)
; 1000003 *** .6500000000000125

(timed-prime-test 10007)
; 10007 *** .4399999999999977
(timed-prime-test 100000007)
; 10000019 *** .8199999999999795

(timed-prime-test 100003)
; 100003 *** .5099999999999909
(timed-prime-test 1000000000039)
; 1000000000039 *** 1.329999999999984
{% endhighlight %}

As can be seen, when the digits are doubled, ie. the number is squared, the time taken for the procedure to complete is approximately doubled. Thus, the time complexity of the `fast-prime?` function is $$O(\log n)$$.

### Exercise 1.25<a name="Exercise1_25">&nbsp;</a>

In this exercise, we are tasked with trying to understand why we should not compute exponentials directly for this particular problem.

When we compute `(fast-expt base exp)` directly, we have to deal with very large numbers when the argument `exp` is large. The numbers may sometimes be larger than what can be stored in a regular integer. In these cases the computation may take a long while to complete.

Thus, we use the modular arithmetic formula $$(a \mod n)(b \mod n) \equiv ab \pmod{n}$$ to compute the final value without dealing with the large values.

This idea is also mentioned in the [footnote 46](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#footnote_Temp_78) of the book.

### Exercise 1.26<a name="Exercise1_26">&nbsp;</a>

We are asked to explain why the following `expmod` function by Louis Reasoner is slow.

{% highlight scheme %}
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))
{% endhighlight %}

As can be seen, the only modification is replacing `(square (expmod base (/ exp 2) m))` with `(* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m))`. This means that when evaluating the second statement, `(expmod base (/ exp 2) m)` is evaluated twice since they are written twice.

Performing a simple time complexity analysis, we can see that for the original case, time taken for computing $$b^{e}$$ is simply the time taken for computing $$b^{e/2}$$ plus some constant, leading to a time complexity of $$O(\log n)$$. Whereas, for the new code, time taken to compute $$b^{e}$$ is twice that of time taken for computing $$b^{e/2}$$. This leads to a branching execution similar to the original Fibonacci example. The new process takes $$O(n)$$ time.

### Exercise 1.27<a name="Exercise1_27">&nbsp;</a>

This exercise tasks us with verifying that Carmichael numbers fool the Fermat test. For that purpose, we write a function that takes an integer n and tests whether $$a^{n} \equiv a \pmod{n}$$ for all $$a<n$$.

{% highlight scheme %}
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))
; expmod

(define (exp-mod-equals? a n)
  (= (expmod a n n) a))
; exp-mod-equals?

(define (try-fermat n a)
  (cond ((= n a) true)
        ((exp-mod-equals? n a) (try-fermat n (+ a 1)))
        (else false)))
; try-fermat

(define (full-fermat-test n)
  (try-fermat n 1))
; full-fermat-test
{% endhighlight %}

Trying it on the given Carmichael numbers, we get

{% highlight scheme %}
(full-fermat-test 561)
; #t
(full-fermat-test 1105)
; #t
(full-fermat-test 1729)
; #t
(full-fermat-test 2465)
; #t
(full-fermat-test 2821)
; #t
(full-fermat-test 6601)
; #t
{% endhighlight %}

Thus, we can see that Carmichael numbers fool the Fermat test.

### Exercise 1.28<a name="Exercise1_28">&nbsp;</a>

In this exercise, we implement a modified form of Fermat test names Miller-Rabin test that cannot be fooled by Carmichael numbers. Basically, we check $$a^{n-1} \equiv 1 \pmod{n}$$ for $$a<n$$ albeit with some checks in the middle. For that purpose, we modify the existing Fermat test.

{% highlight scheme %}
(define (trivial-square s n)
  (define sqmod (remainder (* s s) n))
  (if (and (not (or (= s 1) (= s (- n 1))))
           (= sqmod 1))
      0
      sqmod))
; trivial-square

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
		   (trivial-square (expmod base (/ exp 2) m) m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))
; expmod

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
; miller-rabin-test

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
; fast-prime?
{% endhighlight %}

Running this code on some random prime and non-prime numbers, we get the following:-

{% highlight scheme %}
(fast-prime? 5849 1000)
; #t
(fast-prime? 92153 1000)
; #t
(fast-prime? 19963 1000)
; #t

(fast-prime? 39984 1000)
; #f
(fast-prime? 83507 1000)
; #f
(fast-prime? 62495 1000)
; #f
{% endhighlight %}

Verifying that this test still works, when we try Carmichael numbers, we get the following:-

{% highlight scheme %}
(fast-prime? 561 1000)
; #f
(fast-prime? 1105 1000)
; #f
(fast-prime? 1729 1000)
; #f
(fast-prime? 2465 1000)
; #f
(fast-prime? 2821 1000)
; #f
(fast-prime? 6601 1000)
; #f
{% endhighlight %}

Thus, the Miller-Rabin test can be used to test Carmichael numbers too.
