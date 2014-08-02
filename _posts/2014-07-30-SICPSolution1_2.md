---
layout: post
category: [SICP, Solutions]
post_no: 4
title: SICP Section 1.2 Exercise Solutions
submenu:
  - { hook: "Exercise1_9", title: "Exercise 1.9" }
  - { hook: "Exercise1_10", title: "Exercise 1.10" }
  - { hook: "Exercise1_11", title: "Exercise 1.11" }
---

### Exercise 1.9<a name="Exercise1_9">&nbsp;</a>

This exercise deals with diferrentiating between recursive and iterative processes by tracing the evaluation of two functions.

##### Expansion of `(+ 4 5)` using first definition

{% highlight scheme %}
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
{% endhighlight %}

As can be seen, there is a chain of deferred `inc` operations as function is evaluated. Thus, this process is *recursive*.

Another way to determine that this process is recursive is by observing that the recursive call is nested within another function. The outer function needs to be stored by the interpreter until the inner function returns from the recursive evaluation.

##### Expansion of `(+ 4 5)` using second definition

{% highlight scheme %}
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9
{% endhighlight %}

In this case, there are no deferred operations. The function evaluates exactly to another instance of itself which in turn evaluates to itself till the terminal case. There are no deferred operations to be tracked. Thus, this process is *iterative*.

### Exercise 1.10<a name="Exercise1_10">&nbsp;</a>

This exercise mainly teaches us to interpret procedures into the mathematical functions. For that purpose, we are presented with the [Ackermann's function](http://en.wikipedia.org/wiki/Ackermann_function):-

{% highlight scheme %}
(define (A x y)
  (cond ((= y 0) 0)
  	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
	      	 (A x (- y 1))))))
{% endhighlight %}

Evaluating the given expressions, we get

{% highlight scheme %}
(A 1 10)
; 1024
(A 2 4)
; 65536
(A 3 3)
; 65536
{% endhighlight %}

#### Mathematical definition of `(A 0 n)`

Let us evaluate `(A 0 n)` for various arguments

{% highlight scheme %}
(define (f n) (A 0 n))
; f
(f 0)
; 0
(f 1)
; 2
(f 2)
; 4
(f 3)
; 6
(f 4)
; 8
(f 5)
; 10
{% endhighlight %}

As can be seen, `(f n)` evaluates to $$2n$$. As we run through the function definition, when `(= x 0)` evaluates to `#t`, the Ackermann's function evaluates to `(* 2 y)` which is the value we see.

#### Mathematical definition of `(A 1 n)`

Let us evaluate `(A 1 n)` for various arguments

{% highlight scheme %}
(define (g n) (A 1 n))
; g
(g 0)
; 0
(g 1)
; 2
(g 2)
; 4
(g 3)
; 8
(g 4)
; 16
(g 5)
; 32
{% endhighlight %}

As can be seen, with the exception of when n is 0, `(g n)` evaluates to $$2^n$$. To see why this is the case, we run through the Ackermann's function evaluation

{% highlight scheme %}
(A 1 n)
(A (- 1 1) (A (1 (- n 1))))
(A 0 (A (1 (- n 1))))
(* 2 (A 1 (- n 1)))

...
{% endhighlight %}

We can see a series developing. $$A(1,n) = 2 \times A(1,n-1)$$ in infix notation. The evaluation expands till the terminal case `(= n 1)` which evaluates to 2. Thus, we multiply 2 with itself n times to get $$n^2$$

#### Mathematical definition of `(A 2 n)`

Evaluating `(A 2 n)` for various arguments, we get

{% highlight scheme %}
(define (h n) (A 2 n))
; h
(h 0)
; 0
(h 1)
; 2
(h 2)
; 4
(h 3)
; 16
(h 4)
; 65536
{% endhighlight %}

Its not clear what is going on immediately. But the expansion of evaluation gives us a clue,

{% highlight scheme %}
(A 2 n)
(A (- 2 1) (A (2 (- n 1))))
(A 1 (A (2 (- n 1))))

...
{% endhighlight %}

Combining with the definition of `(A 1 n)`, we get the relation $$A(2,n) = 2^{A(2,n-1)}$$. Expanding down to the terminal case of `(= n 1)`, we see that 2 is powered with itself n times. This can also be written using [Knuth's up-arrow notation](http://en.wikipedia.org/wiki/Knuth%27s_up-arrow_notation) as $$2\uparrow\uparrow2$$.

### Exercise 1.11<a name="Exercise1_11">&nbsp;</a>

This exercise introduces to us the method of solving the same problem using recursive and iterative processes. We have a function similar to Fibonacci series,

$$ f(n) = \begin{cases} n & n < 3 \\ f(n-1)+2f(n-2)+3f(n-3) & otherwise \end{cases} $$

#### Recursive process

The recursive process can be written simply using the definition.

{% highlight scheme %}
(define (f n)
(if (< n 3) n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))
; f

; Test cases:-
(f 0)
; 0
(f 1)
; 1
(f 2)
; 2
(f 3)
; 4
(f 4)
; 11
(f 5)
; 25
(f 10)
; 1892
{% endhighlight %}

#### Iterative process

The iterative process can be written by using transformations applied on a series of integers similar to the method used for computing Fibonacci series iteratively. Let us initilize $$a=f(2)=2$$, $$b=f(1)=1$$ and $$c=f(0)=0$$. We can apply simultaneous transformations.

$$\begin{align} a &\leftarrow a+2b+3c \\ b &\leftarrow a \\ c &\leftarrow b \end{align}$$

Once the transformations are applied *n* number of times, *c* would evaluate to $$f(n)$$.

{% highlight scheme %}
(define (f-iter a b c count)
        (if (= count 0) c
            (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
; f-iter
 
(define (f n) (f-iter 2 1 0 n))
; f
 
; Test cases:-
(f 0)
; 0
(f 1)
; 1
(f 2)
; 2
(f 3)
; 4
(f 4)
; 11
(f 5)
; 25
(f 10)
; 1892
(f 100)
; 11937765839880230562825561449279733086
{% endhighlight %}

The iterative process is more powerful since it can take significantly smaller time in the order of $$O(n)$$ to compute the result as opposed to the recursive process which takes $$O(f(n))$$ time to compute.

### Exercise 1.12<a name="Exercise1_12">&nbsp;</a>

This exercise makes us write a procedure which evaluates the elements in a [Pascal's triangle](http://en.wikipedia.org/wiki/Pascal%27s_triangle) using a recursive process. We use the coordinate system below.

<center><img src="/images/PascalTriangle.gif" alt="Pascal Triangle" /></center>

We can define the pascal function at any row and column as such:-

$$ p(r,c) = \begin{cases} 1 & c=0|c=r\\ p(r-1,c) + p(r-1,c-1) & otherwise \end{cases} $$

{% highlight scheme %}
(define (pascal row col)
(if (or (= col 0) (= col row)) 1
    (+ (pascal (- row 1) col)
       (pascal (- row 1) (- col 1)))))
; pascal

; Test cases:-
(pascal 0 0)
; 1
(pascal 100 100)
; 1
(pascal 13 6)
; 1716
(pascal 9 4)
; 126
{% endhighlight %}


