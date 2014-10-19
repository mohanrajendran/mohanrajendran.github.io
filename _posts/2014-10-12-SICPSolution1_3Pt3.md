---
layout: post
category: [SICP, Solutions]
post_no: 9
title: "SICP Section 1.3 Exercise Solutions - Part 3"
submenu:
  - { hook: "Exercise1_40", title: "Exercise 1.40" }
  - { hook: "Exercise1_41", title: "Exercise 1.41" }
  - { hook: "Exercise1_42", title: "Exercise 1.42" }
  - { hook: "Exercise1_43", title: "Exercise 1.43" }
  - { hook: "Exercise1_44", title: "Exercise 1.44" }
  - { hook: "Exercise1_45", title: "Exercise 1.45" }
  - { hook: "Exercise1_46", title: "Exercise 1.46" }
---

### Exercise 1.40<a name="Exercise1_40">&nbsp;</a>

This exercise tasks us with creating a function `cubic` which can be used in conjunction with `newtons-method` to determine the zeroes of the cubic $$x^3+ax^2+bx+c$$. This problem is fairly simple.

{% highlight scheme %}
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))
{% endhighlight %}

This can be used by Newton's method to determine the roots.

<!--excerpt-->

### Exercise 1.41<a name="Exercise1_41">&nbsp;</a>

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

### Exercise 1.42<a name="Exercise1_42">&nbsp;</a>

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

### Exercise 1.43<a name="Exercise1_43">&nbsp;</a>

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

### Exercise 1.44<a name="Exercise1_44">&nbsp;</a>

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

### Exercise 1.45<a name="Exercise1_45">&nbsp;</a>

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

### Exercise 1.46<a name="Exercise1_46">&nbsp;</a>

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
