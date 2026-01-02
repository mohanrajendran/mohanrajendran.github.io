---
title: "SICP Section 2.1 solutions"
description: "SICP exercises 2.1 - 2.16 - Section 2.1 solutions"
pubDate: 2017-03-07
tags: ["sicp", "computer-science", "scheme", "programming", "functional-programming"]
---

### Exercise 2.1

In this exercise, we are tasked with modifying the given `make-rat` procedure to deal with negative arguments. We can simply do that by checking if the denominator is negative and flipping the digits.

```scheme
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (> d 0) (cons (/ n g) (/ d g))
               (cons (/ (- n) g) (/ (- d) g)))))
```

Also, we perform `gcd` on the absolute values because the definition of `remainder` makes the sign of the answer dependant on the recursion depth. For example,

```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b)))
; gcd

(gcd (- 12) 69)
; -3

(gcd (- 6) 9)
; 3
```

Due to the inconsistency in the sign of the answer, we perform `gcd` on the absolute values to get a consistent *GCD*. Finally, some test cases.

```scheme
(print-rat (make-rat (- 3) (- 9)))
; 1/3

(print-rat (make-rat 3 (- 9)))
; -1/3

(print-rat (make-rat (- 3) 9))
; -1/3

(print-rat (make-rat 3 9))
; 1/3
```

### Exercise 2.2

In this exercise, we are tasked with creating an abstraction to represent a line segment composed of two points which are in turn composed of two numbers: the *x* and *y* coordinates. Let us first define `point` with a method similar to the fractions.

```scheme
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
```

Next, let us define a segment.

```scheme
(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))
```

Let us now, go on to define the procedure `midpoint-segment` which computes the midpoint of a segment and returns a point.

```scheme
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
```

### Exercise 2.3

In this exercise, we are tasked with implementing two different representation for rectangles.

##### Rectangle defined using two diagonally opposite points

We can define a rectangle uniquely using two diagonally opposite points. For this purpose, we can actually, reuse the `point` abstraction from the previous exercise.

```scheme
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
```

Let us test it

```scheme
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
```

##### Rectangle defined using bottom left point, width and height

We can define a rectangle uniquely using the bottom left point and itd height and width.

```scheme
(define (make-rect p1 w h) (cons p1 (cons w h)))

(define (width-rect r)
  (abs (car (cdr r))))

(define (height-rect r)
  (abs (cdr (cdr r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

(define (perimeter-rect r)
  (* (+ (width-rect r) (height-rect r)) 2))
```

Let us test it

```scheme
(define p1 (make-point 23 46))
; p1
(define r (make-rect p1 6 10))
; r

(area-rect r)
; 60

(perimeter-rect r)
; 32
```

### Exercise 2.4

In this beautiful exercise, we are tasked with implementing our own version of `cdr` based on the given alternative representation as follows:-

```scheme
(define (cons x y)
  (lambda (m) (m x y)))
; cons

(define (car z)
  (z (lambda (p q) p)))
; car
```

To see how the `car` procedure works, let us perform substitution. As we have seen, `x` and `y` have been captured in the definition of `z`.

```scheme
(car z)

; Definition of car
(z (lambda (p q) p))

; Substituting expression for z
((lambda (m) (m x y)) (lambda (p q) p))

; Substituting value of m
((lambda (p q) p) x y)

; Bind p by x and q by y
x
```

In essense, the `cons` procedure takes two arguemnts `x` and `y` which gives and expression that takes a procedure `f` that can work on two arguements. By defining `car` to evaluate to the first argument, we get `x`. Similarly, `cdr` can be implemented by evaluating to `q` instead of `p`. Let us implement and test them.

```scheme
(define (cdr z)
  (z (lambda (p q) q)))
; cdr

(define z (cons 3 5))
; z

(car z)
; 3

(cdr z)
; 5
```

### Exercise 2.5

In this exercise, we are required to represent pairs of non-negative integers using only number and arithmetic operations by representing a pair of *a* and *b* using $$2^a 3^b$$.

Let us define `cons`, `car` and `cdr`.

```scheme
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
```

Let us test it

```scheme
(define z (cons 5 7))
; z

(car z)
; 5

(cdr z)
; 7
```

### Exercise 2.6

In this exercise, we are tasked with implementing [Church numerals](http://en.wikipedia.org/wiki/Church_encoding) and defining `one` and `two` directly without using `zero` and `add-1` defined below.

```scheme
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
```

Following the hint, let us see what `(add-1 zero)` evaluates to.

```scheme
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
```

Thus, we get a procedure which takes a function `f` and returns a prodecure which in turn takes an argument `x` and evaluates `(f x)`.

Looking at the Church numerals [wiki page](http://en.wikipedia.org/wiki/Church_encoding), we can see this matches the description given where $$ 0 = \lambda f.\lambda x.x $$ and $$ 1 = \lambda f.\lambda x.f\;x$$.

Thus, by extension, number *n* is applies `f` *n* times. We get the following definition of `one` and `two`:-

```scheme
(define one
  (lambda (f) (lambda (x) (f x))))
; one

(define two
  (lambda (f) (lambda (x) (f (f x)))))
; two
```

Now, we define `+-church` to add two Church numerals. Basically, we add the number of times `f` is applied in each Church numeral and return a new numeral where `f` is applied the sum number of times.

```scheme
(define (+-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
```

Basically, we unwrap the definition of `m` and `n` by feeding it `f` which gives `(lambda (x) (f (f (f ... x))))` Next, we unwrap `n` by feeding it `x` simply getting `(f (f (f ... x)))`.

This in turn gets fed into the unwrapped `m` and we get `(f (f (f ... x)))` where `f` is applied $$m+n$$ times. Now, this function gets wrapped with `lambda (x)` and `lambda (f)` to get the original form of Church numeral.

To test this, let us use the `inc` function.

```scheme
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
```

Thus, our formulation works.

### Exercise 2.7

In this exercise, we are tasked with creating the selector methods for an abstraction representing an interval. This is fairly simple and be done using the following code:-

```scheme
(define (lower-bound i) (car i))
; lower-bound

(define (upper-bound i) (cdr i))
; upper-bound
```

### Exercise 2.8

In this exercise, we are tasked with computing a procedure `sub-interval` which takes in two intervals, subtracts one from another and returns an interval. When we subtract an interval $$[c,d]$$ from another interval $$[a,b]$$, we get the lowest possible value by subtracting the highest possible value from the lowest possible value and vice versa. Therefore,

$$[a,b] - [c,d] = [a-d,b-c]$$

Knowing this, we can write the procedure.

```scheme
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
```

### Exercise 2.9

In this exercise we are asked to give proof that the width of sum and difference of two intervals is a function only of the respective widths of the two intervals. For this purpose, we take two intervals, $$[a-\Delta a, a+\Delta a]$$ and $$[b-\Delta b, b+\Delta b]$$. $$\Delta$$ is used to denote the respective interval width.

##### Addition of intervals

When we add two intervals, the following occurs based on its definition

$$\begin{align}[a-\Delta a, a+\Delta a] + [b-\Delta b, b+\Delta b] &= [(a-\Delta a)+(b-\Delta b),(a+\Delta a)+(b+\Delta b)]
\\ &= [(a+b)-(\Delta a+\Delta b),(a+b)+(\Delta a+\Delta b)\end{align}$$

We can see that the width of the summed interval is $$\Delta a + \Delta b$$ which is simply the sum of the widths.

##### Subtraction of intervals

When we subtract one interval from another, the following happens

$$\begin{align}[a-\Delta a, a+\Delta a] - [b-\Delta b, b+\Delta b] &= [(a-\Delta a)-(b+\Delta b),(a+\Delta a)-(b-\Delta b)]
\\ &= [(a-b)-(\Delta a+\Delta b),(a-b)+(\Delta a+\Delta b)\end{align}$$

We can see that the width of the subtracted interval is $$\Delta a + \Delta b$$ which is simply the difference of the widths.

##### Multiplication/Division of intervals

We can show that widths of multiplied or divided intervals are not a function of widths of the operands. Let us take three intervals $$[2,4]$$, $$[4,8]$$ and $$[8,12]$$ which have widths *1*, *2* and *2* respectively.

```scheme
(define i1 (make-interval 2 4))
; i1
(define i2 (make-interval 4 8))
; i2
(define i3 (make-interval 8 12))
; i3

(mul-interval i2 i1)
; (8 . 32)
(mul-interval i3 i1)
; (16 . 48)

(div-interval i2 i1)
; (1. . 4.)
(div-interval i3 i1)
; (2. . 6.)
```

Notice that both `i2` and `i3` have the same width. However, the width of their products and division with `i1` are not the same. Thus, we have shown that the width of results for multiplication and division are not a function of the width of the operands.

### Exercise 2.10

In this exercise, we are told to modify the `div-interval` function such that if we try to divide with an interval that spans zero, an error is registered. This is done simply by performing a check.

```scheme
(define (div-interval x y)
  (define (interval-spans i n)
    (and (<= n (upper-bound i))
         (>= n (lower-bound i))))
  (if (interval-spans y 0)
      (error "Attempt to divide by an interval spanning zero")
      (mul-interval x
                    (make-interval 
                    (/ 1.0 (upper-bound y)) 
                    (/ 1.0 (lower-bound y))))))
```

We can test this code by using the following test cases:

```scheme
(define i1 (make-interval 1 5))
; i1
(define i2 (make-interval 5 10))
; i2
(define i3 (make-interval (- 5) 5))
; i3

(div-interval i1 i2)
; (.1 . 1.)
(div-interval i1 i3)
; Attempt to divide by an interval spanning zero
```

### Exercise 2.11

In this exercise, we are told that we can break the `mul-interval` function into 9 different cases for which only one case requires more than two multiplications. When we take an interval, there are three ways that it can be setup in terms of the signs $$[-,-]$$, $$[-,+]$$ and $$[+,+]$$. No other combination is possible since `lower-bound` is always lower than `upper-bound`. Thus, for both intervals, we have a total of nine combinations. Since the multiplication operation is commutative, there are a total of six combinations.

1. $$a\subset[+,+]$$ and $$b\subset[+,+]$$
	* Lower-bound is the product of both lower-bounds
	* Upper-bound is the product of both upper-bounds

2. $$a\subset[-,-]$$ and $$b\subset[-,-]$$
	* Lower-bound is the product of both upper-bounds
	* Upper-bound is the product of both lower-bounds

3. $$a\subset[+,+]$$ and $$b\subset[-,-]$$
	* Lower-bound is the product of *a*'s upper-bound and *b*'s lower-bound
	* Upper-bound is the product of *a*'s lower-bound and *b*'s upper-bound

4. $$a\subset[+,+]$$ and $$b\subset[-,+]$$
	* Lower-bound is the product of *a*'s upper-bound and *b*'s lower-bound
	* Upper-bound is the product of *a*'s upper-bound and *b*'s upper-bound

5. $$a\subset[-,-]$$ and $$b\subset[-,+]$$
	* Lower-bound is the product of *a*'s lower-bound and *b*'s upper-bound
	* Upper-bound is the product of *a*'s lower-bound and *b*'s lower-bound

6. $$a\subset[-,+]$$ and $$b\subset[-,+]$$
	* Lower-bound is the highest of the product of *a*'s lower-bound and *b*'s upper-bound, and product of *a*'s upper-bound and *b*'s lower-bound
	* Upper-bound is the highest of the product of *a*'s lower-bound and *b*'s lower-bound, product of *a*'s upper-bound and *b*'s upper-bound

Now, let us write code for this and call it `mul-interval-mod` to differentiate it from the original code for testing purposes.

```scheme
(define (mul-interval-mod x y)
  (define (neg-neg i) (<= (upper-bound i) 0))
  (define (pos-pos i) (>= (lower-bound i) 0))
  (define (neg-pos i) (and (<= (lower-bound i) 0)
                           (>= (upper-bound i) 0)))
  (let ((xlo (lower-bound x))
        (ylo (lower-bound y))
        (xhi (upper-bound x))
        (yhi (upper-bound y)))
    (cond ((and (pos-pos x) (pos-pos y))
            (make-interval (* xlo ylo) (* xhi yhi)))
          ((and (neg-neg x) (neg-neg y))
            (make-interval (* xhi yhi) (* xlo ylo)))
          ((and (pos-pos x) (neg-neg y))
            (make-interval (* xhi ylo) (* xlo yhi)))
          ((and (neg-neg x) (pos-pos y))
            (make-interval (* xlo yhi) (* xhi ylo)))
          ((and (pos-pos x) (neg-pos y))
            (make-interval (* xhi ylo) (* xhi yhi)))
          ((and (neg-pos x) (pos-pos y))
            (make-interval (* xlo yhi) (* xhi yhi)))
          ((and (neg-neg x) (neg-pos y))
            (make-interval (* xlo yhi) (* xlo ylo)))
          ((and (neg-pos x) (neg-neg y))
            (make-interval (* xhi ylo) (* xlo ylo)))
          ((and (neg-pos x) (neg-pos y))
            (make-interval (max (* xlo yhi)
                                (* xhi ylo))
                           (max (* xlo ylo)
                                (* xhi yhi)))))))
```

Now let us test this behemoth by comparing with the original `mul-interval`.

```scheme
(define nn (make-interval (- 5) (- 1)))
; nn
(define pp (make-interval 1 5))
; pp
(define np (make-interval (- 5) 5))
; np

(mul-interval pp pp)
; (1 . 25)
(mul-interval-mod pp pp)
; (1 . 25)

(mul-interval nn nn)
; (1 . 25)
(mul-interval-mod nn nn)
; (1 . 25)

(mul-interval pp nn)
; (-25 . -1)
(mul-interval-mod pp nn)
; (-25 . -1)

(mul-interval nn pp)
; (-25 . -1)
(mul-interval-mod nn pp)
; (-25 . -1)

(mul-interval pp np)
; (-25 . 25)
(mul-interval-mod pp np)
; (-25 . 25)

(mul-interval np pp)
; (-25 . 25)
(mul-interval-mod np pp)
; (-25 . 25)

(mul-interval nn np)
; (-25 . 25)
(mul-interval-mod nn np)
; (-25 . 25)

(mul-interval np nn)
; (-25 . 25)
(mul-interval-mod np nn)
; (-25 . 25)

(mul-interval np np)
; (-25 . 25)
(mul-interval-mod np np)
; (-25 . 25)
```

As can be seen, both answers match. However, the new code is longer and much harder to maintain. We don't know for sure if we saved on execution time because of the multiple jump statements too. This exercise thus demonstrates the importance of code clarity.

### Exercise 2.12

In this exercise, we are tasked with creating procedures to support intervals specified by the center point and the percentage of error. We can reuse the `make-center-width`, `width` and `center` code given in the book. The code for percentage-based interval is as follows:-

```scheme
(define (make-center-percent center percent)
  (let ((width (/ (* center percent) 100.0)))
    (make-center-width center width)))
; make-center-percent

(define (percent i)
  (/ (* (width i) 100.0) (center i)))
; percent
```

### Exercise 2.13

In this exercise, we are tasked with investigating if there is a simple formula for approximate percentage tolerance based on the tolerance of the factors.

We are told to make two assumptions:-

* The intervals are positive
* The tolerance percentages are small

Let us take two intervals *a* and *b* with tolerances $$T_a$$ and $$T_b$$ respectively. Multiplying them and using the rules derived earlier for upper and lower bounds for positive bounds, we get

$$\begin{align}&[a(1-T_a),a(1+T_a)] \times [b(1-T_b),b(1+T_b)]
\\ &= [a(1-T_a)\times b(1-T_b),a(1+T_a)\times b(1+T_b)]
\\ &= [ab(1-T_a - T_b+T_a T_b),ab(1+T_a + T_b + T_a T_b)]
\end{align}$$

Since we have assumed that tolerance percentages are small, we know that the value of $$T_a T_b\ll 1$$. Thus, we can ignore the term, arriving at a new interval

$$[ab(1-(T_a + T_b)),ab(1+(T_a + T_b))]$$

Tolerance of the product $$T_p = T_a + T_b$$.

### Exercise 2.14

In this exercise we are asked to figure out what went wrong when two algebriacally equivalent forms of resistance equations

$$\frac{R_1 R_2}{R_1 + R_2}$$

and

$$\frac{1}{1/R_1 + 1/R_2}$$

give two different intervals when evaluated. Let us demonstrate this by using two resistors. $$10\Ohm$$ and $$20\Ohm$$ with 1% and 2% tolerances respectively. Let us use `par1` and `par2` given in the text.

```scheme
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))
; par1

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))
; par2

(define a (make-center-percent 10 1))
; a
(define b (make-center-percent 20 2))
; b

(define i1 (par1 a b))
; i1
(define i2 (par2 a b))
; i2

i1
; (6.361967213114755 . 6.9844067796610165)
i2
; (6.5776271186440685 . 6.7554098360655725)

(center i1)
; 6.673186996387885
(center i2)
; 6.6665184773548205

(percent i1)
; 4.6637353852303285
(percent i2)
; 1.3334000200059877
```

As can be seen, the two methods produce two very different intervals.

Let us see what happens when we evaluate $$a/a$$ and $$a/b$$ directly.

```scheme
(define aa (div-interval a a))
; aa
(define ab (div-interval a b))
; ab

aa
; (.9801980198019803 . 1.02020202020202)
ab
; (.4852941176470589 . .5153061224489796)

(center aa)
; 1.0002000200020003
(percent aa)
; 1.9998000199979906

(center ab)
; .5003001200480193
(percent ab)
; 2.9994001199759954
```

We can see that $$a/a$$ yeilds an interval which is not even centered exactly on one. This means that when we transform equations, we should not assume that the value is one directly. Thus, we start to see where things diverge in both forms of equations.

### Exercise 2.15

In this exercise, we are told to determine the validity of the statement that the `par2` procedure gives a better result than `par1`. Let us see why this is right.

We have seen before that for every operation involving intervals, the width of the interval only increases for all operation and never decreases. Thus, it always makes sense to reduce the overall interval by not involving any interval with a non-zero width. For example the expression $$1/R_1$$ is preferable to an equivalent expression $$R_1 / {R_1}^2$$ simply because of the extra errors being contributed to the resulting interval.

Thus, when we take the expression used in `par1`,

$$\frac{R_1 R_2}{R_1 + R_2}$$

we can see that we have 3 operations involving intervals of non-zero width.

Whereas for the expression used in `par2`,

$$\frac{1}{1/R_1 + 1/R_2}$$

we can see that most operations involve a pure number 1, which can be considered an interval of width 0. The only operation involving two intervals of non-zero width is the addition of $$1/R_1$$ and $$1/R_2$$. Thus, the result of this procedure has a tighter error bound.

### Exercise 2.16

In this exercise, we are tasked with determining if an interval package can be written that can minimize error bounds in interval operations by choosing the appropriate equivalent algebraic form.

In general, equivalent algebraic expressions lead to different answers because of the complex relationship between the results of an interval operation and its arguments. Thus, determining the overall interval of a resulting complex expression gets very difficult.

Looking at the wikipedia article [here](http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem). We see that this indeed is the case, but we can reduce error by ensuring that each interval appears only once in the expression. This problem is not an easy one to solve for an arbitrary given expression as it might involve symbolic manipulation.

Thus, the best solution is to use expressions which does not have variables repeating.
