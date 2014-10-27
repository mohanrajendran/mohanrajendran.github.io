---
layout: post
category: [SICP, Solutions]
post_no: 11
title: "SICP Section 2.1 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise2_1", title: "Exercise 2.1" }
   - { hook: "Exercise2_2", title: "Exercise 2.2" }
   - { hook: "Exercise2_3", title: "Exercise 2.3" }
   - { hook: "Exercise2_4", title: "Exercise 2.4" }
   - { hook: "Exercise2_5", title: "Exercise 2.5" }
   - { hook: "Exercise2_6", title: "Exercise 2.6" }
---

### Exercise 2.1<a name="Exercise2_1">&nbsp;</a>

In this exercise, we are tasked with modifying the given `make-rat` procedure to deal with negative arguments. We can simply do that by checking if the denominator is negative and flipping the digits.

{% highlight scheme %}
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (> d 0) (cons (/ n g) (/ d g))
               (cons (/ (- n) g) (/ (- d) g)))))
{% endhighlight %}

<!--excerpt-->

Also, we perform `gcd` on the absolute values because the definition of `remainder` makes the sign of the answer dependant on the recursion depth. For example,

{% highlight scheme %}
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b)))
; gcd

(gcd (- 12) 69)
; -3

(gcd (- 6) 9)
; 3
{% endhighlight %}

Due to the inconsistency in the sign of the answer, we perform `gcd` on the absolute values to get a consistent *GCD*. Finally, some test cases.

{% highlight scheme %}
(print-rat (make-rat (- 3) (- 9)))
; 1/3

(print-rat (make-rat 3 (- 9)))
; -1/3

(print-rat (make-rat (- 3) 9))
; -1/3

(print-rat (make-rat 3 9))
; 1/3
{% endhighlight %}

### Exercise 2.2<a name="Exercise2_2">&nbsp;</a>

In this exercise, we are tasked with creating an abstraction to represent a line segment composed of two points which are in turn composed of two numbers: the *x* and *y* coordinates. Let us first define `point` with a method similar to the fractions.

{% highlight scheme %}
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 5 6))
; (5,6)
{% endhighlight %}

Next, let us define a segment.

{% highlight scheme %}
(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))
{% endhighlight %}

Let us now, go on to define the procedure `midpoint-segment` which computes the midpoint of a segment and returns a point.

{% highlight scheme %}
(define (midpoint-segment s)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))
; midpoint-segment

(define p1 (make-point 0 0))
; p1
(define p2 (make-point 6 10))
; p2
(define s (make-segment p1 p2))
; s

(print-point (midpoint-segment s))
; (3,5)
{% endhighlight %}

### Exercise 2.3<a name="Exercise2_3">&nbsp;</a>

In this exercise, we are tasked with implementing two different representation for rectangles.

##### Rectangle defined using two diagonally opposite points

We can define a rectangle uniquely using two diagonally opposite points. For this purpose, we can actually, reuse the `point` abstraction from the previous exercise.

{% highlight scheme %}
(define (make-rect p1 p2) (cons p1 p2))

(define (width-rect r)
  (abs (- (x-point (car r))
          (x-point (cdr r)))))

(define (height-rect r)
  (abs (- (y-point (car r))
          (y-point (cdr r)))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

(define (perimeter-rect r)
  (* (+ (width-rect r) (height-rect r)) 2))
{% endhighlight %}

Let us test it

{% highlight scheme %}
(define p1 (make-point 0 0))
; p1
(define p2 (make-point 6 10))
; p2
(define r (make-rect p1 p2))
; r

(area-rect r)
; 60

(perimeter-rect r)
; 32
{% endhighlight %}

##### Rectangle defined using bottom left point, width and height

We can define a rectangle uniquely using the bottom left point and itd height and width.

{% highlight scheme %}
(define (make-rect p1 w h) (cons p1 (cons w h)))

(define (width-rect r)
  (abs (car (cdr r))))

(define (height-rect r)
  (abs (cdr (cdr r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

(define (perimeter-rect r)
  (* (+ (width-rect r) (height-rect r)) 2))
{% endhighlight %}

Let us test it

{% highlight scheme %}
(define p1 (make-point 23 46))
; p1
(define r (make-rect p1 6 10))
; r

(area-rect r)
; 60

(perimeter-rect r)
; 32
{% endhighlight %}

### Exercise 2.4<a name="Exercise2_4">&nbsp;</a>

In this beautiful exercise, we are tasked with implementing our own version of `cdr` based on the given alternative representation as follows:-

{% highlight scheme %}
(define (cons x y)
  (lambda (m) (m x y)))
; cons

(define (car z)
  (z (lambda (p q) p)))
; car
{% endhighlight %}

To see how the `car` procedure works, let us perform substitution. As we have seen, `x` and `y` have been captured in the definition of `z`.

{% highlight scheme %}
(car z)

; Definition of car
(z (lambda (p q) p))

; Substituting expression for z
((lambda (m) (m x y)) (lambda (p q) p))

; Substituting value of m
((lambda (p q) p) x y)

; Bind p by x and q by y
x
{% endhighlight %}

In essense, the `cons` procedure takes two arguemnts `x` and `y` which gives and expression that takes a procedure `f` that can work on two arguements. By defining `car` to evaluate to the first argument, we get `x`. Similarly, `cdr` can be implemented by evaluating to `q` instead of `p`. Let us implement and test them.

{% highlight scheme %}
(define (cdr z)
  (z (lambda (p q) q)))
; cdr

(define z (cons 3 5))
; z

(car z)
; 3

(cdr z)
; 5
{% endhighlight %}

### Exercise 2.5<a name="Exercise2_5">&nbsp;</a>

In this exercise, we are required to represent pairs of non-negative integers using only number and arithmetic operations by representing a pair of *a* and *b* using $$2^a 3^b$$.

Let us define `cons`, `car` and `cdr`.

{% highlight scheme %}
(define (fact-power n m)
  (define (fact-power-iter n m acc)
    (if (= (remainder n m) 0)
        (fact-power-iter (/ n m) m (+ acc 1))
		acc))
  (fact-power-iter n m 0))
; fact-power

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
; cons

(define (car z)
  (fact-power z 2))
; car

(define (cdr z)
  (fact-power z 3))
; cdr
{% endhighlight %}

Let us test it

{% highlight scheme %}
(define z (cons 5 7))
; z

(car z)
; 5

(cdr z)
; 7
{% endhighlight %}

### Exercise 2.6<a name="Exercise2_6">&nbsp;</a>

In this exercise, we are tasked with implementing [Church numerals](http://en.wikipedia.org/wiki/Church_encoding) and defining `one` and `two` directly without using `zero` and `add-1` defined below.

{% highlight scheme %}
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
{% endhighlight %}

Following the hint, let us see what `(add-1 zero)` evaluates to.

{% highlight scheme %}
(add-1 zero)

; Substitute n with zero
(lambda (f) (lambda (x) (f ((zero f) x))))

; Substitute zero with its definition, using f0 and x0
; to differentiate from the original f and x
(lambda (f) (lambda (x) (f (((lambda (f0) (lambda (x0) x0)) f) x))))

; Applying lambda (f0) to f
(lambda (f) (lambda (x) (f ((lambda (x0) x0) x))))

; Applying lambda (x0) to x
(lambda (f) (lambda (x) (f x)))
{% endhighlight %}

Thus, we get a procedure which takes a function `f` and returns a prodecure which in turn takes an argument `x` and evaluates `(f x)`.

Looking at the Church numerals [wiki page](http://en.wikipedia.org/wiki/Church_encoding), we can see this matches the description given where $$ 0 = \lambda f.\lambda x.x $$ and $$ 1 = \lambda f.\lambda x.f\;x$$.

Thus, by extension, number *n* is applies `f` *n* times. We get the following definition of `one` and `two`:-

{% highlight scheme %}
(define one
  (lambda (f) (lambda (x) (f x))))
; one

(define two
  (lambda (f) (lambda (x) (f (f x)))))
; two
{% endhighlight %}

Now, we define `+-church` to add two Church numerals. Basically, we add the number of times `f` is applied in each Church numeral and return a new numeral where `f` is applied the sum number of times.

{% highlight scheme %}
(define (+-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
{% endhighlight %}

Basically, we unwrap the definition of `m` and `n` by feeding it `f` which gives `(lambda (x) (f (f (f ... x))))` Next, we unwrap `n` by feeding it `x` simply getting `(f (f (f ... x)))`.

This in turn gets fed into the unwrapped `m` and we get `(f (f (f ... x)))` where `f` is applied $$m+n$$ times. Now, this function gets wrapped with `lambda (x)` and `lambda (f)` to get the original form of Church numeral.

To test this, let us use the `inc` function.

{% highlight scheme %}
(define (inc x) (+ x 1))
; inc

((zero inc) 5)
; 5
((one inc) 5)
; 6
(((add-1 one) inc) 5)
; 7
(((+-church one two) inc) 5)
; 8
{% endhighlight %}

Thus, our formulation works.
