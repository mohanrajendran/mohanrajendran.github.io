---
layout: post
category: [SICP, Solutions]
post_no: 4
title: SICP Section 1.2 Exercise Solutions
submenu:
  - { hook: "Exercise1_9", title: "Exercise 1.9" }
  - { hook: "Exercise1_10", title: "Exercise 1.10" }
  - { hook: "Exercise1_11", title: "Exercise 1.11" }
  - { hook: "Exercise1_12", title: "Exercise 1.12" }
  - { hook: "Exercise1_13", title: "Exercise 1.13" }
  - { hook: "Exercise1_14", title: "Exercise 1.14" }
  - { hook: "Exercise1_15", title: "Exercise 1.15" }
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

### Exercise 1.13<a name="Exercise1_13">&nbsp;</a>

This exercise wants us to prove that $$Fib(n)$$ is the closest integer to $$\phi^n/\sqrt{5}$$ where $$\phi=(1+\sqrt{5})/2$$. Let us derive this using the hints given.

Firstly, we are told to prove that

$$Fib(n)=(\phi^n-\psi^n)/\sqrt{5}$$

where $$\psi=(1-\sqrt{5})/2$$. We are also given that $$\phi^2=\phi+1$$ and $$\psi^2=\psi+1$$.

Let us use mathematical induction to prove the relation.

#### Base case

Left-hand side:-

$$\begin{align}Fib(0) &= 0 \\ Fib(1) &= 1 \\ Fib(2) &= Fib(0)+Fib(1)=1\end{align}$$

Right-hand side:-

*n = 0*

$$\begin{align}(\phi^n-\psi^n)/\sqrt{5} &= (\phi^0-\psi^0)/\sqrt{5} \\ &= (1-1)/\sqrt{5}
\\ &= 0/\sqrt{5}
\\ &= 0\end{align}$$

*n = 1*

$$\begin{align}(\phi^n-\psi^n)/\sqrt{5} &= (\phi^1-\psi^1)/\sqrt{5} \\ &= ((1+\sqrt{5})/2-(1-\sqrt{5})/2)/\sqrt{5}
\\ &= ((2\sqrt{5})/2)/\sqrt{5}
\\ &= \sqrt{5}/\sqrt{5}
\\ &= 1\end{align}$$

*n = 2*

$$\begin{align}(\phi^n-\psi^n)/\sqrt{5} &= (\phi^2-\psi^2)/\sqrt{5} \\ &= ((\phi+1)-(\psi+1))/\sqrt{5}
\\ &= (\phi-\psi)/\sqrt{5}
\\ &= 1\end{align}$$

#### Inductive step

Let us assume that the following relations hold true,

$$\begin{align} Fib(n) &= (\phi^n-\psi^n)/\sqrt{5} \\ Fib(n-1) &= (\phi^{n-1}-\psi^{n-1})/\sqrt{5}\end{align}$$

We need to prove that the assumptions lead to

$$Fib(n+1) = (\phi^{n+1}-\psi^{n+1})/\sqrt{5}$$

From definition,

$$\begin{align}Fib(n+1) &= Fib(n)+Fib(n-1)
\\ &= (\phi^n-\psi^n)/\sqrt{5} + (\phi^{n-1}-\psi^{n-1})/\sqrt{5}
\\ &= (\phi^n+\phi^{n-1})/\sqrt{5} - (\psi^n+\psi^{n-1})/\sqrt{5}
\\ &= (\phi^{n-1}(\phi+1))/\sqrt{5} - (\psi^{n-1}(\psi+1))/\sqrt{5}
\\ &= (\phi^{n-1}\phi^2)/\sqrt{5} - (\psi^{n-1}\psi^2)/\sqrt{5}
\\ &= (\phi^{n+1}-\psi^{n+1})/\sqrt{5} \end{align}$$

Hence, we have proven the relation.

#### Original problem

Going back to the original problem, we can reformulate it to say that we need to prove $$\|Fib(n) - \phi^n/\sqrt{5}\| <= 1/2$$.

From the inductive proof above, we can see that $$Fib(n)-\phi^n/\sqrt{5} = \psi^n/\sqrt{5}$$.

Thus, we need to prove $$\|\phi^n/\sqrt{5}\| <= 1/2$$ or $$\|\phi^n\| <= \sqrt{5}/2$$.

We have $$\phi = (1-\sqrt{5})/2 \approx -0.618034$$ and $$\sqrt{5}/2 \approx 1.118034$$.

Since $$\|\phi\|<1$$, we have $$\|\phi^n\|<1$$ where n is any positive integer in $$Fib(n)$$.

We can safely say that

$$\|Fib(n) - \phi^n/\sqrt{5}\| <= 1/2$$

Thus, *Fib(n) is the closest integer to* $$\phi^n/\sqrt{5}$$.

### Exercise 1.14<a name="Exercise1_14">&nbsp;</a>

This exercise tasks us with understanding the growth of a recursive process. We are told to draw the tree illustrating the process generated by the given procedure `(count-change 11)`.

{% highlight scheme %}
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
{% endhighlight %}

The graph obtained is as follows *(click to enlarge)*:-

<center><a href="/images/Ex1_14plot.png"><img src="/images/Ex1_14plot.png" alt="Count change recursion tree" height="300"/></a></center>

*Note:- The Python code used to generate this tree is found [here](https://gist.github.com/mohanrajendran/a3a8ea66bd5f66cbfbc5)*

#### Complexity of procedure

For the purpose of simplicity, let us denote *n* as the total amount and *m* as the number of types of coins(denominations).

**Space Complexity**: The space complexity of the procedure is fairly straight forward. The maximum space taken by the solution occurs when the most depth is reached. The longest depth can be reached by first taking the branch where `kinds-of-coins` is reduced to 1 followed by reducing the `amount` by the smallest denomination of 1. This results in a space complexity of $$O(n+m)$$. Since *m* is usually much smaller than *n*, we get an asymptotic space complexity of $$O(n)$$.

**Time Complexity**: The time complexity can be determined by building up from simpler cases. Thanks to Bill the Lizard's post [here](http://www.billthelizard.com/2009/12/sicp-exercise-114-counting-change.html).

We start with a simple case whereupon we have no coins, ie. we call `(cc amount 0)`. In this case, we get the following graph:-

<center><img src="/images/Ex1_14plot0.png" alt="Count change recursion tree" height="100"/></center>

A simple case where we have only one branch yielding time complexity $$O(1)$$.

Next, we move to one type of coin invoking `(cc amount 1)`. To determine its time complexity, we look at the process generated for that case:-

<center><a href="/images/Ex1_14plot1.png"><img src="/images/Ex1_14plot1.png" alt="Count change recursion tree" height="300"/></a></center>

Using the same notation as in space complexity, we can see that in this case, the number of steps taken is given by calling `(cc n 1)` $$n+1$$ times with successively smaller values which results in `(cc amount 0)` given above being called n times, yielding a time of $$2n+1$$ and time complexity of $$O(n)$$.

We next look at two types of coins invoking `(cc amount 2)`. We once again look at the process generated for this case.

<center><a href="/images/Ex1_14plot2.png"><img src="/images/Ex1_14plot2.png" alt="Count change recursion tree" height="300"/></a></center>

We can see the main path calling `(cc n 2)` gets called $$n/5$$ times because of the second denomination of 5 cents. Each node in this path each calls its own `(cc n 1)` sub-process. Summing them up, we obtain a sum for total time, $$(2n+1) + (2(n-5)+1) + (2(n-10)+1) + ...$$. The number of terms in this summation is $$n/5$$ yielding a time complexity of $$O(n^2)$$.

As such, we can see a pattern building up. Since we have five types of coins ultimately, we end up with a final time complexity for `(cc amount 5)` to be $$O(n^5)$$.

### Exercise 1.15<a name="Exercise1_15">&nbsp;</a>

As with previous case, we try to analyse the complexity of the following function:-

{% highlight scheme %}
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
{% endhighlight %}

To get the number of times procedure `p` is applied in evaluating `(sine 12.15)`, we look at its expansion:-

{% highlight scheme %}
(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine (/ 4.05 3.0))))
(p (p (sine 1.35)))
(p (p (p (sine (/ 1.35 3.0)))))
(p (p (p (sine 0.45))))
(p (p (p (p (sine (/ 0.45 3.0))))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine (/ 0.15 3.0)))))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
(p (p (p (p 0.1495))))
(p (p (p 0.4351)))
(p (p 0.9758))
(p -0.7896)
-0.3998
{% endhighlight %}

From the above, we can see that the procedure `p` is applied 5 times.

#### Order of growth

The time and space complexity should have the same order in this case because we do not see any branches in the evaluation tree.

As we can see, the number of steps taken by the evaluation of `(sine a)` increases by one when `a` is tripled. This hints at a logarithmic growth of $$O(log n)$$ for both time and space complexity.
