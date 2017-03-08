---
layout: spiedpage
order: 6
title: Section 1.3 solutions
exercises: '1.29 - 1.46'
submenu:
  - { hook: "Exercise1_29", title: "Exercise 1.29" }
  - { hook: "Exercise1_30", title: "Exercise 1.30" }
  - { hook: "Exercise1_31", title: "Exercise 1.31" }
  - { hook: "Exercise1_32", title: "Exercise 1.32" }
  - { hook: "Exercise1_33", title: "Exercise 1.33" }
  - { hook: "Exercise1_34", title: "Exercise 1.34" }
  - { hook: "Exercise1_35", title: "Exercise 1.35" }
  - { hook: "Exercise1_36", title: "Exercise 1.36" }
  - { hook: "Exercise1_37", title: "Exercise 1.37" }
  - { hook: "Exercise1_38", title: "Exercise 1.38" }
  - { hook: "Exercise1_39", title: "Exercise 1.39" }
  - { hook: "Exercise1_40", title: "Exercise 1.40" }
  - { hook: "Exercise1_41", title: "Exercise 1.41" }
  - { hook: "Exercise1_42", title: "Exercise 1.42" }
  - { hook: "Exercise1_43", title: "Exercise 1.43" }
  - { hook: "Exercise1_44", title: "Exercise 1.44" }
  - { hook: "Exercise1_45", title: "Exercise 1.45" }
  - { hook: "Exercise1_46", title: "Exercise 1.46" }
---

### Exercise 1.29<a id="Exercise1_29">&nbsp;</a>

In this exercise, we are tasked with implementing the Simposon's rule to compute integrals.

$$\int_a^b f(x) \,dx = \frac{h}{3}(y_0+4y_1+2y_2+4y_3+2y_4+...+2y_{n-2}+4y_{n-1}+y_n)$$

where $$h = (b-a)/n$$, $$n$$ is even and $$y_k=f(a+kh)$$.

To make it amenable to be solved using the given `sum` function, we can transform the equation to

$$\int_a^b f(x) \,dx = \frac{h}{3}[(y_0-y_n)+(4y_1+2y_2)+(4y_3+2y_4)+...+(4y_{n-1}+2y_n)]$$

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

### Exercise 1.30<a id="Exercise1_30">&nbsp;</a>

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

### Exercise 1.31<a id="Exercise1_31">&nbsp;</a>

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

### Exercise 1.32<a id="Exercise1_32">&nbsp;</a>

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

### Exercise 1.33<a id="Exercise1_33">&nbsp;</a>

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

### Exercise 1.34<a id="Exercise1_34">&nbsp;</a>

In this exercise, we have the following function:-

{% highlight scheme %}
(define (f g) (g 2))
{% endhighlight %}

If we ask the interpreter to evaluate `(f f)` we end up getting the following substitutions:-

{% highlight scheme %}
(f f)
(f 2)
(2 2)
{% endhighlight %}

Unfortunately, the statement `(2 2)` can't be evaluated since 2 is not function. Thus, an error should be thrown by the interpreter. Let us verify this:-

{% highlight scheme %}
(f f)
; The object 2 is not applicable.
{% endhighlight %}

Thus, application of `2` throws an error.

### Exercise 1.35<a id="Exercise1_35">&nbsp;</a>

In this exercise, we are simply tasked with utilizing the pre-defined `fixed-point` procedure to determine the golden ratio $$\varphi$$ given by the formula $$x\mapsto1+1/x$$. To perform that we use the following code.

{% highlight scheme %}
(define tolerance 0.00001)
; tolerance

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
; fixed-point

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
; golden-ratio

golden-ratio
; 1.6180327868852458
{% endhighlight %}

Thus the golden-ratio is obtained.

### Exercise 1.36<a id="Exercise1_36">&nbsp;</a>

In this exercise, we are asked to modify `fixed-point` to print out intermediate results along the way and use it to compute the solution to $$x^x=1000$$. We modify the procedure as thus:-

{% highlight scheme %}
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
; fixed-point
{% endhighlight %}

##### Computation without average damping

First, we try to compute the solution to $$x^x=1000$$ by using the direct relation $$x\mapsto log(1000)/log(x)$$ with an initial guess of 2.

{% highlight scheme %}
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; 2.
; 9.965784284662087
; 3.004472209841214
; 6.279195757507157
; 3.759850702401539
; 5.215843784925895
; 4.182207192401397
; 4.8277650983445906
; 4.387593384662677
; 4.671250085763899
; 4.481403616895052
; 4.6053657460929
; 4.5230849678718865
; 4.577114682047341
; 4.541382480151454
; 4.564903245230833
; 4.549372679303342
; 4.559606491913287
; 4.552853875788271
; 4.557305529748263
; 4.554369064436181
; 4.556305311532999
; 4.555028263573554
; 4.555870396702851
; 4.555315001192079
; 4.5556812635433275
; 4.555439715736846
; 4.555599009998291
; 4.555493957531389
; 4.555563237292884
; 4.555517548417651
; 4.555547679306398
; 4.555527808516254
; 4.555540912917957
; 4.555532270803653
{% endhighlight %}

We see a lot of oscillation about the final solution with this undamped iteration. Thus, it takes a long time to converge.

##### Computation average damping

Next, we try to compute the solution to $$x^x=1000$$ by using a modified relation $$x\mapsto \frac{1}{2}(x+log(1000)/log(x))$$ with an initial guess of 2.

{% highlight scheme %}
(define (average x y) (/ (+ x y) 2))
; average

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
; 2.
; 5.9828921423310435
; 4.922168721308343
; 4.628224318195455
; 4.568346513136242
; 4.5577305909237005
; 4.555909809045131
; 4.555599411610624
; 4.5555465521473675
; 4.555537551999825
{% endhighlight %}

As can be seen, this damped relation prevents unwanted oscillations and lets the recurrence converge on right value quicker.

### Exercise 1.37<a id="Exercise1_37">&nbsp;</a>

In this exercise, we are tasked with creating a function `cont-frac` which can be used to compute continued fractions of the form

$$F_k=\frac{N_1}{D_1 + \frac{N_2}{\ddots + \frac{N_k}{D_k}}}$$

given the functions for generating $$N_i$$ and $$D_i$$.

##### Recursive procedure

We define a function of form `(cont-frac n d k)` which computes continued fractions.

{% highlight scheme %}
(define (cont-frac n d k)
  (define (cont-frac-rec i)
    (let ((ni (n i))
          (di (d i)))
      (if (= i k) (/ ni di)
                  (/ ni (+ di (cont-frac-rec (+ i 1)))))))
  (cont-frac-rec 1))
{% endhighlight %}

Next, we are told to compute $$1/\varphi$$ where $$\varphi$$ is the golden ratio using the relation $$N_i = 1$$ and $$D_i = 1$$. This can be having `n` and `d` as `(lambda (i) 1.0)`. To determine the number of steps required for 4 decimal places of accuracy, we perform repeated calculation to see when we reach $$0.6180$$.

{% highlight scheme %}
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
; .625
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 6)
; .6153846153846154
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 7)
; .6190476190476191
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)
; .6176470588235294
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9)
; .6181818181818182
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
; .6179775280898876
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
; .6180555555555556
{% endhighlight %}

Thus, we can see that it takes a *11-term finite continued fraction* to compute the reciprocal of golden ratio to 4 decimal places.

##### Iterative procedure

The conversion of the procedure to be iterative cannot be done directly for this procedure since computing *k-term finite continued fraction* from a *(k-1)-term finite continued fraction* is not an easy task. However, we can compute the whole expression in the reverse direction staring with $$N_k/D_k$$ and use accumulated values. We get the following code in that case:-

{% highlight scheme %}
(define (cont-frac n d k)
  (define (cont-frac-iter i acc)
    (let ((ni (n i))
          (di (d i)))
      (if (= i 0) acc
                  (cont-frac-iter (- i 1) (/ ni (+ di acc))))))
  (cont-frac-iter k 0))
{% endhighlight %}

Repeating with the same arguments as the recursive version gives the same results.

### Exercise 1.38<a id="Exercise1_38">&nbsp;</a>

In this exercise, we are tasked with computing $$e$$, the base of natural logarithms using the `cont-frac` procedure from the previous exercise. We are given the fact that when $$N_i$$ is all 1 and $$D_i$$ are successfully $$1,2,1,1,4,1,1,6,1,1,8,...$$, we get $$e-2$$.

{% highlight scheme %}
(define (e-denominator i)
  (let ((r (remainder i 3)))
    (if (or (= r 0) (= r 1))
        1
        (/ (* (+ i 1) 2) 3))))
; e-denominator

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0) e-denominator k)))
; e

(e 100)
; 2.7182818284590455
{% endhighlight %}

Thus, the value of $$e$$ is computed.

### Exercise 1.39<a id="Exercise1_39">&nbsp;</a>

In this exercise, we are required to compute the tangent function using the formula

$$tan(x) = \frac{x}{1-\frac{x^2}{3-\frac{x^2}{5-...}}}$$

where x is in radians. We define a procedure `tan-cf` as follows:-

{% highlight scheme %}
(define (tan-cf x k)
  (define (tan-n i)
    (if (= i 1) x (- (square x))))
  (define (tan-d i)
    (- (* i 2) 1))
  (cont-frac tan-n tan-d k))
{% endhighlight %}

Let us compare with the built-in `tan` function in Scheme.

{% highlight scheme %}
(define pi 3.14159265359)
; pi

(tan 1.0)
; 1.5574077246549023
(tan-cf 1.0 10)
; 1.557407724654902
(tan 2.5)
; -.7470222972386603
(tan-cf 2.5 10)
; -.747022297267734
{% endhighlight %}

As can be seen, the continued fraction representation gives an accurate computation of the tangent function.

### Exercise 1.40<a id="Exercise1_40">&nbsp;</a>

This exercise tasks us with creating a function `cubic` which can be used in conjunction with `newtons-method` to determine the zeroes of the cubic $$x^3+ax^2+bx+c$$. This problem is fairly simple.

{% highlight scheme %}
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))
{% endhighlight %}

This can be used by Newton's method to determine the roots.

### Exercise 1.41<a id="Exercise1_41">&nbsp;</a>

In this exercise, we are required to write a procedure `double` which takes a function and applies it twice. It can be done easily too.

{% highlight scheme %}
(define (double f)
  (lambda (x) (f (f x))))
{% endhighlight %}

Now, when we perform the following procedure,

{% highlight scheme %}
(((double (double double)) inc) 5)

(((lambda (x) ((double double) ((double double) x))) inc) 5)

(((double double) ((double double) inc)) 5)

(((double double) (double (double inc))) 5)

...

((double (double (double (double inc)))) 5)
{% endhighlight %}

Doubling 4 times, means `inc` would be applied $$2^4=16$$ times. The expected answer would be 21. To verify,

{% highlight scheme %}
(((double (double double)) inc) 5)
; 21
{% endhighlight %}

### Exercise 1.42<a id="Exercise1_42">&nbsp;</a>

In this exercise, we are tasked with writing a `compose` function that takes two one-argument functions *f* and *g* and gives a compound function $$x\mapsto f(g(x))$$

{% highlight scheme %}
(define (compose f g)
  (lambda (x) (f (g x))))
{% endhighlight %}

To test it, we try the given example in Scheme REPL.

{% highlight scheme %}
((compose square inc) 6)
; 49
{% endhighlight %}

### Exercise 1.43<a id="Exercise1_43">&nbsp;</a>

In this exercise, we are tasked with the writing a function `repeated` which would take a single argument function *f* and a number *n* and return a compound function $$x\mapsto f(f(...(f(x))))$$ n times. The procedure for this can be written using the same pattern as the quick exponentiation function from the previous sections as well as the `compose` and `double` functions from the previous exercises.

{% highlight scheme %}
(define (compose f g)
  (lambda (x) (f (g x))))
; compose

(define (double f) (compose f f))
; double

(define (even? n) (= (remainder n 2) 0))
; even?

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x))
        ((even? n)
         (double (repeated f (/ n 2))))
        (else
         (compose f (repeated f (- n 1))))))
; repeated
{% endhighlight %}

To test it, we run through the example cases.

{% highlight scheme %}
((repeated square 2) 5)
; 625
((repeated inc 100) 5)
; 105
{% endhighlight %}

### Exercise 1.44<a id="Exercise1_44">&nbsp;</a>

In this exercise, we are required to create a way of *smoothing* functions repeatedly. Applying smoothing to a function *f* produces a new function *g* such that $$g(x) = [f(x-dx) + f(x) + f(x+dx)]/3$$. Smoothing can be performed repeatedly as well. We utilize the `repeated` function from the previous exercise to accomplish this. We keep a value of $$dx = 0.00001$$ as used in the book.

{% highlight scheme %}
(define dx 0.00001)
; dx

(define (smoothe f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))
; smoothe

(define (n-fold-smoothe f n)
  ((repeated smoothe n) f))
; n-fold-smoothe
{% endhighlight %}

To test this function, we take the `sin` function. We get the following numbers for x around 1.

| x       | sin(x)       | 1-damp       | 2-damp       | 3-damp       |
|---------|--------------|--------------|--------------|--------------|
| 0.99997 | 0.8414547754 |              |              |              |
| 0.99998 | 0.8414601786 | 0.8414601786 |              |              |
| 0.99999 | 0.8414655817 | 0.8414655817 | 0.8414655817 |              |
| 1       | 0.8414709848 | 0.8414709848 | 0.8414709848 | 0.8414709847 |
| 1.00001 | 0.8414763878 | 0.8414763878 | 0.8414763877 |              |
| 1.00002 | 0.8414817907 | 0.8414817907 |              |              |
| 1.00003 | 0.8414871935 |              |              |              |

Let us test using our code.

{% highlight scheme %}
((n-fold-smoothe sin 1) 1)
; .8414709847798475
((n-fold-smoothe sin 2) 1)
; .8414709847517985
((n-fold-smoothe sin 3) 1)
; .8414709847237495
{% endhighlight %}

As can be seen, correct answers are obtained.

### Exercise 1.45<a id="Exercise1_45">&nbsp;</a>

In this exercise, we are essentially tasked with tweaking the number of damps to ensure that the roots of polynomials converge when fixed-point iteration is used. To study this, let use the modified `fixed-point` function that outputs intermediate values in the `fixed-point-of-transform` function.

{% highlight scheme %}
(define tolerance 0.00001)
; tolerance

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
; fixed-point

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))
; fixed-point-of-transform
{% endhighlight %}

Let us create a root function which computes the *n*-th root of x and also takes starting guess and the number of damps used. We can utilize the `repeated` function from the previous exercise.

{% highlight scheme %}
(define (avg x y) (/ (+ x y) 2.0))
; avg

(define (avg-damp f)
  (lambda (x) (avg x (f x))))
; avg-damp

(define (n-th-root-helper n x damps first-guess)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (repeated avg-damp damps)
    first-guess))
; n-th-root-helper
{% endhighlight %}

Let us find the roots of various powers. Let us start with square root with no damping

{% highlight scheme %}
(n-th-root-helper 2 4 0 1.0)
; 1.
; 4.
; 1.
; 4.
...
{% endhighlight %}

As can be seen, we see a repeating pattern. The process does not converge. Let us add one damping. If iteration converges, only the last value is given for brevity.

{% highlight scheme %}
(n-th-root-helper 2 4 1 1.0)
; 2.000000000000002
(n-th-root-helper 3 8 1 1.0)
; 1.9999981824788517

(n-th-root-helper 4 16 1 1.0)
...
; 2.0093631665789298
; 1.9907673178391767
; 2.009361536417286
; 1.9907689027452413
...
{% endhighlight %}

As can be seen, the 4-th root with 1 damp causes oscillation around the actual value. Thus, one average damp is not sufficient for 4-th root and above. Let us add one more damp and try again.

{% highlight scheme %}
(n-th-root-helper 4 16 2 1.0)
; 2.0000000000021965
(n-th-root-helper 5 32 2 1.0)
; 2.000001512995761
(n-th-root-helper 6 64 2 1.0)
; 2.0000029334662086
(n-th-root-helper 7 128 2 1.0)
; 2.0000035538623377

(n-th-root-helper 8 256 2 1.0)
...
; 2.0028934283781528
; 1.9971357466538153
; 2.002893090970039
; 1.9971360772727353
...
{% endhighlight %}

Thus, damping twice stops being effective from the 8-th root onwards. We can sort of see a relationship here. Let us verify this by checking if 3 damps stop being effective for the 16-th root onwards.

{% highlight scheme %}
(n-th-root-helper 15 (expt 2 15) 3 1.0)
; 2.0000040951543028

(n-th-root-helper 16 (expt 2 16) 3 1.0)
...
; 2.001635453681883
; 1.9983845140195047
; 2.001635149361151
; 1.998384810927022
...
{% endhighlight %}

Thus, we have verified that for 16-th root onwards 3 damps are not sufficient. We can see the following pattern:-

- Roots of 2-3 require 1 damp
- Roots of 3-7 require 2 damps
- Roots of 8-15 require 3 damps
- Roots of 16-... require 4 damps

As *n* reaches powers of two, one more average damp is needed to compute root. Thus, the number of damps required $$D$$ can be computed using the floor function.

$$D=\lfloor{log_2{n}}\rfloor$$

Let us create a generic n-th-root function using this knowledge.

{% highlight scheme %}
(define (n-th-root n x)
  (define (log2 x) (/ (log x) (log 2)))
  (let ((damps (floor (log2 n))))
    (n-th-root-helper n x damps 1.0)))
{% endhighlight %}

To test it, let us find large roots of numbers.

{% highlight scheme %}
(n-th-root 100 (expt 2 100))
; 2.0000032790812043
(n-th-root 200 (expt 2 200))
; 2.0000021535853345
{% endhighlight %}

It works!

### Exercise 1.46<a id="Exercise1_46">&nbsp;</a>

In this exercise, we are tasked with writing a generic procedure called `iterative-improve` which takes in two procedures, one two check if a guess is good enough and one to improve the guess. This procedure should return a procedure which, when applied to a guess should improve it iteratively. This is a simple task that can be accomplished using the following code.

{% highlight scheme %}
(define (iterative-improve good-enough? improve)
  (define (iterate guess)
    (if (good-enough? guess)
        guess
        (iterate (improve guess))))
   iterate)
{% endhighlight %}

##### Sqrt function

Let us consider the original `sqrt` function

{% highlight scheme %}
(define (sqrt x)
   (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
   (if (good-enough? guess x)
       guess
       (sqrt-iter (improve guess x)
                  x)))

(define (improve guess x)
   (average guess (/ x guess)))

(define (average x y)
   (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))
{% endhighlight %}

It can be rewritten as follows by reusing some of its functions:-
{% highlight scheme %}
(define (sqrt x)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (square guess) x)) 0.001))
     (lambda (guess)
       (average guess (/ x guess))))
   1.0))
; sqrt

(sqrt 2)
; 1.4142156862745097
(sqrt 100)
; 10.000000000139897
{% endhighlight %}

##### Fixed-point iteration function

Let us reconsider the fixed-point iteration function.

{% highlight scheme %}
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
{% endhighlight %}

It can be rewritten as follows:-

{% highlight scheme %}
(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (f guess) guess)) tolerance))
     (lambda (guess)
       (f guess)))
   first-guess))
; fixed-point

(fixed-point cos 1.0)
; .7390893414033927
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
; 1.2587228743052672
{% endhighlight %}
