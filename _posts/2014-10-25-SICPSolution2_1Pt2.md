---
layout: post
category: [SICP, Solutions]
post_no: 12
title: "SICP Section 2.1 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_7", title: "Exercise 2.7" }
   - { hook: "Exercise2_8", title: "Exercise 2.8" }
   - { hook: "Exercise2_9", title: "Exercise 2.9" }
   - { hook: "Exercise2_10", title: "Exercise 2.10" }
   - { hook: "Exercise2_11", title: "Exercise 2.11" }
   - { hook: "Exercise2_12", title: "Exercise 2.12" }
---

### Exercise 2.7<a name="Exercise2_7">&nbsp;</a>

In this exercise, we are tasked with creating the selector methods for an abstraction representing an interval. This is fairly simple and be done using the following code:-

{% highlight scheme %}
(define (lower-bound i) (car i))
; lower-bound

(define (upper-bound i) (cdr i))
; upper-bound
{% endhighlight %}

<!--excerpt-->

### Exercise 2.8<a name="Exercise2_8">&nbsp;</a>

In this exercise, we are tasked with computing a procedure `sub-interval` which takes in two intervals, subtracts one from another and returns an interval. When we subtract an interval $$[c,d]$$ from another interval $$[a,b]$$, we get the lowest possible value by subtracting the highest possible value from the lowest possible value and vice versa. Therefore,

$$[a,b] - [c,d] = [a-d,b-c]$$

Knowing this, we can write the procedure.

{% highlight scheme %}
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
{% endhighlight %}

### Exercise 2.9<a name="Exercise2_9">&nbsp;</a>

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

{% highlight scheme %}
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
{% endhighlight %}

Notice that both `i2` and `i3` have the same width. However, the width of their products and division with `i1` are not the same. Thus, we have shown that the width of results for multiplication and division are not a function of the width of the operands.

### Exercise 2.10<a name="Exercise2_10">&nbsp;</a>

In this exercise, we are told to modify the `div-interval` function such that if we try to divide with an interval that spans zero, an error is registered. This is done simply by performing a check.

{% highlight scheme %}
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
{% endhighlight %}

We can test this code by using the following test cases:

{% highlight scheme %}
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
{% endhighlight %}

### Exercise 2.11<a name="Exercise2_11">&nbsp;</a>

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

{% highlight scheme %}
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
{% endhighlight %}

Now let us test this behemoth by comparing with the original `mul-interval`.

{% highlight scheme %}
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
{% endhighlight %}

As can be seen, both answers match. However, the new code is longer and much harder to maintain. We don't know for sure if we saved on execution time because of the multiple jump statements too. This exercise thus demonstrates the importance of code clarity.

### Exercise 2.12<a name="Exercise2_12">&nbsp;</a>

In this exercise, we are tasked with creating procedures to support intervals specified by the center point and the percentage of error. We can reuse the `make-center-width`, `width` and `center` code given in the book. The code for percentage-based interval is as follows:-

{% highlight scheme %}
(define (make-center-percent center percent)
  (let ((width (/ (* center percent) 100.0)))
    (make-center-width center width)))
; make-center-percent

(define (percent i)
  (/ (* (width i) 100.0) (center i)))
; percent
{% endhighlight %}
