---
layout: post
category: [SICP, Solutions]
post_no: 7
title: "SICP Section 1.3 Exercise Solutions - Part 1"
submenu:
  - { hook: "Exercise1_29", title: "Exercise 1.29" }
  - { hook: "Exercise1_30", title: "Exercise 1.30" }
  - { hook: "Exercise1_31", title: "Exercise 1.31" }
  - { hook: "Exercise1_32", title: "Exercise 1.32" }
  - { hook: "Exercise1_33", title: "Exercise 1.33" }
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

### Exercise 1.30<a name="Exercise1_30">&nbsp;</a>

In this exercise, we are tasked with rewriting `sum` so that it is evaluated recursively. It is dones as follows:-

{% highlight scheme %}
(define (sum term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))
{% endhighlight %}

Repeating the procedures with the new definition gives the same answers.

### Exercise 1.31<a name="Exercise1_31">&nbsp;</a>

This exercise tasks us with writing a `product` procedure analogous to the `sum` procedure from earlier. We simply follow the same format and replace addition with multiplication and 0 with 1. 

##### Recursive procedure

We get the following recursive procedure:-

{% highlight scheme %}
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
{% endhighlight %}

We can compute the factorial of a number by using the following code.

{% highlight scheme %}
(define (factorial n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (product id 1 inc n))
; factorial

(factorial 0)
; 1
(factorial 1)
; 1
(factorial 2)
; 2
(factorial 3)
; 6
(factorial 4)
; 24
(factorial 5)
; 120
(factorial 10)
; 3628800
{% endhighlight %}

Also, we can compute the approximate value of $$\pi$$ using the given formula:-

$$\pi = 4\times\dfrac{2.4}{3.3}\times\dfrac{4.6}{5.5}\times\dfrac{6.8}{7.7}\times...$$

{% highlight scheme %}
(define (pi terms)
  (define (inc2 x) (+ x 2))
  (define (func n)
    (/ (* (- n 1) (+ n 1))
       (* n n)))
  (* 4 (product func 3.0 inc2 (+ 3 (* terms 2)))))
; pi

(pi 1)
; 3.413333333333333
(pi 2)
; 3.343673469387755
(pi 10)
; 3.207709732466547
(pi 1000)
; 3.1423765818510354
(pi 10000)
; 3.14167117868269
{% endhighlight %}

##### Iterative procedure

Like how we expressed `sum` using an iterative procedure, we can also express `product` using an iterative procedure as follows:-

{% highlight scheme %}
(define (product term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
; product

(factorial 0)
; 1
(factorial 1)
; 1
(factorial 2)
; 2
(factorial 3)
; 6
(factorial 4)
; 24
(factorial 5)
; 120
(factorial 10)
; 3628800

(pi 1)
; 3.413333333333333
(pi 2)
; 3.343673469387755
(pi 10)
; 3.207709732466548
(pi 1000)
; 3.142376581851035
(pi 10000)
; 3.1416711786826776
{% endhighlight %}

As can be seen, same results are obtained from both recursive and iterative procedure.

### Exercise 1.32<a name="Exercise1_32">&nbsp;</a>

In this exercise, we are tasked with showing that both `sum` and `product` can be shown as a special case of an `accumulate` procedure with the signature.

{% highlight scheme %}
(accumulate 
 combiner null-value term a next b)
{% endhighlight %}

This can simply be done by extracting the differences between the two and encapsulating it in `combiner` and `null-value`. Multiplication and addition go into `combiner`. 1 and 0 go into `null-value`

##### Recursive procedure

The recursive procedure is as follows:- 

{% highlight scheme %}
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
{% endhighlight %}

Then, `product` and `sum` can be defined as follows:-

{% highlight scheme %}
(define (sum term a next b)
  (accumulate + 0 term a next b))
; sum

(define (product term a next b)
  (accumulate * 1 term a next b))
; product
{% endhighlight %}

##### Iterative procedure

The iterative procedure is also simple. It is as follows:-

{% highlight scheme %}
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
{% endhighlight %}

Same answers are obtained when the procedures are evaluated using the functions from previous exercises.

### Exercise 1.33<a name="Exercise1_33">&nbsp;</a>

In this section we are tasked with creating a version of `accumulate` called `filtered-accumulate` that only accumulates values satisfying a single argument predicate named `filter`.

{% highlight scheme %}
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
	  (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))
{% endhighlight %}

##### Sum of squares of prime numbers

Using the `fast-prime?` from the previous section, we can create a function that sums the squares of prime numbers from a given range.

{% highlight scheme %}
(define (fast-prime? n)
   (define (smallest-divisor n)
      (define (find-divisor n test-divisor)
         (define (next x)
            (if (= x 2) 3 (+ x 2)))
         (define (divides? a b)
            (= (remainder b a) 0))
         (cond ((> (square test-divisor) n) n)
               ((divides? test-divisor n) test-divisor)
               (else (find-divisor n (next test-divisor)))))
      (find-divisor n 2))
   (= n (smallest-divisor n)))
; fast-prime?

(define (sum-of-squared-primes a b)
  (define (inc x) (+ x 1))
  (define (square x) (* x x))
  (filtered-accumulate fast-prime? + 0 square a inc b))
; sum-of-squared-primes
{% endhighlight %}

To validate this, we can look at the first few primes 2, 3, 5, 7, 11 and 13.

{% highlight scheme %}
(sum-squared-primes 2 3) ;4+9
; 13
(sum-squared-primes 2 6) ;4+9+25
; 38
(sum-squared-primes 6 10) ;49
; 49
(sum-squared-primes 6 13) ;49+121+169
; 339
{% endhighlight %}

##### Product of coprimes

The product of coprimes of n can be computed as follows:-

{% highlight scheme %}
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; gcd

(define (product-of-coprimes n)
  (define (inc x) (+ x 1))
  (define (id x) x)
  (define (coprime? i) (= (gcd i n) 1))
  (filtered-accumulate coprime? * 1 id 1 inc (- n 1)))
; product-of-coprimes
{% endhighlight %}

To test it, let us check for the coprimes of number 13. Since 13 is a prime, the answer should be $$12!$$.

{% highlight scheme %}
(product-of-coprimes 13)
; 479001600
(factorial 12)
; 479001600
{% endhighlight %}
