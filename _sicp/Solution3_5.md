---
layout: spiedpage
order: 26
title: Section 3.5 solutions
exercises: '3.50 - 3.66'
submenu:
  - { hook: "Exercise3_50", title: "Exercise 3.50" }
  - { hook: "Exercise3_51", title: "Exercise 3.51" }
  - { hook: "Exercise3_52", title: "Exercise 3.52" }
  - { hook: "Exercise3_53", title: "Exercise 3.53" }
  - { hook: "Exercise3_54", title: "Exercise 3.54" }
  - { hook: "Exercise3_55", title: "Exercise 3.55" }
  - { hook: "Exercise3_56", title: "Exercise 3.56" }
  - { hook: "Exercise3_57", title: "Exercise 3.57" }
  - { hook: "Exercise3_58", title: "Exercise 3.58" }
  - { hook: "Exercise3_59", title: "Exercise 3.59" }
  - { hook: "Exercise3_60", title: "Exercise 3.60" }
  - { hook: "Exercise3_61", title: "Exercise 3.61" }
  - { hook: "Exercise3_62", title: "Exercise 3.62" }
  - { hook: "Exercise3_63", title: "Exercise 3.63" }
  - { hook: "Exercise3_64", title: "Exercise 3.64" }
  - { hook: "Exercise3_65", title: "Exercise 3.65" }
  - { hook: "Exercise3_66", title: "Exercise 3.66" }
  - { hook: "Exercise3_67", title: "Exercise 3.67" }
---

In this section, we are introduced to streams. Implementation of streams require lazy evaluation. Since all of user-created functions in Scheme is eagerly evaluated, we need to use [macros](https://en.wikipedia.org/wiki/Macro_(computer_science)). They can be declared using the `define-syntax` command as follows:-

{% highlight scheme %}
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))
{% endhighlight %}

### Exercise 3.50<a id="Exercise3_50">&nbsp;</a>

In this exercise, we are tasked with creating a generalized `stream-map` function that can take multiple streams and a procedure that takes as many arguments as the number of streams provided. The code can be implemented as follows:-

{% highlight scheme %}
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))
{% endhighlight %}

### Exercise 3.51<a id="Exercise3_51">&nbsp;</a>

In this exercise, we are tasked with executing the following code:-

{% highlight scheme %}
(define (show x)
  (display-line x)
  x)

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))
; 0

(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5
; 5
(stream-ref x 7)
; 6
; 7
; 7
{% endhighlight %}

*0* is printed immediately since `stream-map` immediately accesses the first element. Subsequently, the next elements are accessed and their values are displayed. Calling `(stream-ref x 5)` accesses the first five elements and diplays their value. Calling `(stream x 7)` next displays only *6* and *7* because the previous values have already been memoized by the `delay` macro. Thus the values were returned without displaying them first. Also, each call displays the final number twice. Once is from the actual `show` and the other is the value returned by `stream-ref`.

### Exercise 3.52<a id="Exercise3_52">&nbsp;</a>

In this exercise, we are tasked with tracing the value of `sum` as the following code is executed:-

{% highlight scheme %}
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)
{% endhighlight %}

At this point `sum` is *0* since it has not been mutated yet.

{% highlight scheme %}
(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
{% endhighlight %}

At this point `sum` is *1* since the `stream-map` processes the first value in the interval stream and calls `accum`.

{% highlight scheme %}
(define y (stream-filter even? seq))
{% endhighlight %}

At the end of this call, `sum` holds the value of *6*. This is because *stream-filter* consumes value until the first element that passes its criteria. That first value would be $$1+2+3=6$$.

{% highlight scheme %}
(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
{% endhighlight %}

At the end of this call, `sum` holds the value of *10*. This is because the first element in `seq` that is divisible by five is $$1+2+3+4=10$$.

{% highlight scheme %}
(stream-ref y 7)
; 136
{% endhighlight %}

At the end of this call, `sum` holds the value of *136* which is the 7th event value of `seq` is $$1+...+16=136$$.

{% highlight scheme %}
(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; 'done
{% endhighlight %}

At the end of this call, `sum` holds the value of *210*. This is because `display-stream` goes through all of `seq`. Thus the final value is $$1+...+20=210$$.

If the `delay` function has not been memoized, we will get different values in this case. This is because of the side-effect in the function given to `stream-map`. Each time the stream is used, the value of `sum` would have mutated and it would be hard to predict the value. By memoization, we restrict the side effect to only the first time the stream is resolved.

### Exercise 3.53<a id="Exercise3_53">&nbsp;</a>

In this exercise, we are asked to describe the elements of the stream defined as follows:-

{% highlight scheme %}
(define s (cons-stream 1 (add-streams s s)))
{% endhighlight %}

The first element is obviously *1* as defined. The second element is the sum of first element with itself. Thus it is *2*. The third element would be the sum of the second element with itself. It will be *4*. In essense, we get $$1 2 4 8 16 32 64 ...$$. Another way to look at it would be through the following code transformation:-

{% highlight scheme %}
(define s (cons-stream 1 (add-streams s s)))

(define s (cons-stream 1 (stream-map (lambda (x) (* 2 x)) s)))

(define s (cons-stream 1 (scale-stream s 2)))

double
{% endhighlight %}

As can be seen, the given stream is the same as the one already given in the book which gives the same value.

### Exercise 3.54<a id="Exercise3_54">&nbsp;</a>

In this exercise, we are taked with defining a stream for calculating factorial using `mul-streams` function. The code to do so is as follows:-

{% highlight scheme %}
(define (mul-streams s1 s2) 
  (stream-map * s1 s2))
{% endhighlight %}

We can then define `factorials` stream using the following code:-

{% highlight scheme %}
(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))
{% endhighlight %}

### Exercise 3.55<a id="Exercise3_55">&nbsp;</a>

In this exercise, we are asked to define `partial-sums`, a function that takes in a stream $$S$$ and outputs $$S_0, S_0+S_1, S_0+S_1+S_2,...$$. The function can be defined as follows:-

{% highlight scheme %}
(define (partial-sums s)
  (cons-stream (stream-car s)
               (stream-map (lambda (x) (+ x (stream-car s)))
                           (partial-sums (stream-cdr s)))))
{% endhighlight %}

### Exercise 3.56<a id="Exercise3_56">&nbsp;</a>

In this exercise, we are aked to define `S` which is a stream of numbers beginning with *1* and contains numbers whose multiples are *2*, *3* and *5*. We are given a hint that `(scale-stream S 2)`, `(scale-stream S 3)` and `(scale-stream S 5)` also belongs in `S`. Given this and a merge function, `S` can be defined as follows:-

{% highlight scheme %}
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S
  (cons-stream 1
               (merge (merge (scale-stream S 2) (scale-stream S 3))
                      (scale-stream S 5))))
{% endhighlight %}

### Exercise 3.57<a id="Exercise3_57">&nbsp;</a>

In this task, we asked for the number of additions performed when $$n^{th}$$ fibonacci number is calculated by the given `fibs` stream. Since the `dela` function is memoized, the time taken for calculating `fibs` is $$O(n)$$ since all *n-1* terms required for its computation has already been computed and can be recovered in $$O(1)$$ time. Now, if the streams is not memoized, as calculated in Chapter 1, we take $$O(fib(n))$$ time to calculate.

### Exercise 3.58<a id="Exercise3_58">&nbsp;</a>

In this exercise, we are given the following stream:-

{% highlight scheme %}
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
{% endhighlight %}

When we call `(expand 1 7 10)`, we get the following:-

{% highlight scheme %}
(expand 1 7 10)
(cons-stream 1 (expand 3 7 10))
(cons 1 (cons-stream 4 (expand 2 7 10)))
(cons 1 (cons 4 (cons-stream 2 (expand 6 7 10))))
...
; 1 4 2 8 5 7 1 4 2 8
{% endhighlight %}

Next, when we call `(expand 3 8 10)`, we get the following:-

{% highlight scheme %}
(expand 3 8 10)
(cons-stream 3 (expand 6 8 10))
(cons 3 (cons-stream 7 (expand 4 8 10)))
(cons 3 (cons 7 (cons-stream 5 (expand 0 8 10))))
...
; 3 7 5 0 0 0 0 0 0
{% endhighlight %}

In essence, this stream performs [long division](https://www.wikiwand.com/en/Long_division) to calculate $$\text{num}/\text{den}$$ with the given radix or base.

### Exercise 3.59<a id="Exercise3_59">&nbsp;</a>

In this exercise, we are tasked wtih creating a procedure `integrate-series` which takes as input a stream $$a_0$$, $$a_1$$, $$a_2$$,... which denotes a polynomial $$a_0+a_1x+a_2x^2+...$$ and gives its stream which denotes its integrated polynomial without the constant term, $$a_0x+\frac{1}{2}a_1x^2+\frac{1}{3}a_2x^3+...$$ as a stream $$a_0$$, $$\frac{1}{2}a_1$$, $$\frac{1}{3}a_2$$,... . First, let us define `integrate-series` as following:-

{% highlight scheme %}
(define (integrate-series series)
  (define (integrate-helper s divisor)
    (cons-stream (/ (stream-car s) divisor)
                 (integrate-helper (stream-cdr s) (+ divisor 1))))
  (integrate-helper series 1))
{% endhighlight %}

Next, we need to define `cosine-series` and `sine-series` using this function. It can be done as follows:-

{% highlight scheme %}
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series)
                               -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
{% endhighlight %}

The series are defined using [mutual recursion](https://www.wikiwand.com/en/Mutual_recursion). As more terms are needed, the counterpart generates enough terms.

### Exercise 3.60<a id="Exercise3_60">&nbsp;</a>

In this exercise, we are tasked with writing a `mul-series` function that multiplies two series streams as defined in the above exercise. It can be done as follows:-

{% highlight scheme %}
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series s2
                                        (stream-cdr s1)))))
{% endhighlight %}

We are basically calculating 

$$\begin{align}
(a_0 + a_{rest})*(b_0 + b_{rest}) &= (a_0 * b_0) + (a_0 * b_{rest}) + (a_{rest} * b_0) + (a_{rest} * b_{rest}) 
\\&= (a_0 * b_0) + (a_0 * b_{rest}) + (a_{rest} * b)
\end{align}$$

### Exercise 3.61<a id="Exercise3_61">&nbsp;</a>

In this exercise, we are tasked with writing a function that computes the inverse of a series. It can be done as follows:-

{% highlight scheme %}
(define (invert-unit-series series)
  (define inverted
    (cons-stream 1
                 (scale-stream (mul-series (stream-cdr series)
                                           inverted)
                               -1)))
  inverted)
{% endhighlight %}

### Exercise 3.62<a id="Exercise3_62">&nbsp;</a>

In this exercise, we are tasked with creating `div-series` which divides two power series. It should work for all denominators with non-zero constant. We can go about it as follows:-

{% highlight scheme %}
(define (div-series num-series den-series)
  (let ((divisor (stream-car den-series)))
    (if (= divisor 0)
        (error "Cannot divide denominator with 0 constant term")
        (mul-series (invert-unit-series (scale-stream den-series
                                                      (/ 1 divisor)))
                    num-series))))
{% endhighlight %}

### Exercise 3.63<a id="Exercise3_63">&nbsp;</a>

In this exercise, we are given the following modified version for `sqrt-stream`:-

{% highlight scheme %}
(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))
{% endhighlight %}

The reason is that each recursive call of `(sqrt-stream x)` constructs a new stream. This means extracting value from this stream would take fresh computations. This is exactly what would happen if we did not memoize `delay`.

### Exercise 3.64<a id="Exercise3_64">&nbsp;</a>

In this exercise, we are tasked with implementing `stream-limit` which takes a stream and a tolerance number. The function then examines the stream until it finds two successive elements which differ by less than tolerance and return the second number. It can be implemented as follows:-

{% highlight scheme %}
(define (stream-limit s tol)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0))
           tol)
        s1
        (stream-limit (stream-cdr s) tol))))
{% endhighlight %}

### Exercise 3.65<a id="Exercise3_65">&nbsp;</a>

In this exercise, we are tasked with creating a stream to compute $$ln2 = 1 - \frac{1}{2} + \frac{1}{3} - \frac{1}{4} + ...$$. Then, we apply [Euler's series acceleration transformation](https://www.wikiwand.com/en/Series_acceleration#/Euler.27s_transform) to examine the convergence characteristics. The `ln2-stream` can be defined as follows:-

{% highlight scheme %}
(define (ln2-summands n)
        (cons-stream
         (/ 1.0 n) 
         (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
        (partial-sums (ln2-summands 1)))
{% endhighlight %}

Taking into account $$ln2=0.69314718056$$, let us examine the convergences,

{% highlight scheme %}
(display-stream ln2-stream)
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333333
; 0.6166666666666667
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349206
; ...

(display-stream (euler-transform ln2-stream))
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539682
; 0.6930657506744463

(display-stream (accelerated-sequence euler-transform ln2-stream))
; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604038
; 0.6931471805599444
; 0.6931471805599426
; 0.6931471805599453
{% endhighlight %}

As can be seen, recursive acceleration provides the best convergence.

### Exercise 3.66<a id="Exercise3_66">&nbsp;</a>

In this exercise, we are given the following code to generate pairs of integers:-

{% highlight scheme %}
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
{% endhighlight %}

We are tasked with examining the pairs generated by calling `(pairs integers integers)` and calculated the number of pairs preceding given pairs at least approximately.

##### `(1 100)`

The first level of call gives `(1 1)` as the first pair and interleaves `(1 2) (1 3) (1 4) ...` with `(pairs (2 3 ...) (2 3 ...))`. This means `(1 x)` appears as every second element. `(1 2)` has 1 preceding element. `(1 3)` has 3 preceding elements. `(1 4)` has 5 preceding elements. This gives the relationship where `(1 n)` has $$2n-3$$ elements before it. Thus, `(1 100)` has *197* elements preceding it.

##### `(99 100)`

For finding the number of preceding elements for `(99 100)`, lets us peel out the `(1 x)` terms, from the previous case. This results in `(2 x)` occuring every other terms in a sequence interleaved with `(pairs (3 4 ...) (3 4 ...))`. Overall, this leads to `(2 x)` occuring every $$2^2=4$$ terms in the overall sequence. Extending this, we can conclude that the terms `(n x)` occur every $$2^n$$ steps. Next, we need to determine the first terms in such a sequence `(x x)`. As can be seen, `(1 1)` is the first element, `(2 2)` is the third element and `(3 3)` is the seventh element. Extending this, we can see that `(n n)` occurs as the term number $$2^n-1$$ in proportion to term spacing.  Thus, the term `(99 99)` occurs as term number $$2^99-1$$ and `(99 100)` occurs $$2^100$$ terms after that. We thus get $$2^100 + 2^99 - 2$$ terms preceding `(99 100)`

##### `(100 100)`

As determined in the previous segment, `(100 100)` has $$2^100-1$$ preceding elements.

### Exercise 3.67<a id="Exercise3_67">&nbsp;</a>

In the above exercise, the `pairs` stream generates pair of form `(i j)` where $$i<=j$$. In this exercise, we are tasked with generating a stream with all pairs without this condition. This can be done by modifying the `pairs` function as follows:-

{% highlight scheme %}
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) 
                   (list x (stream-car s)))
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t))))))
{% endhighlight %}

### Exercise 3.68<a id="Exercise3_68">&nbsp;</a>

In this exercise, we are given an alternative definition for `pairs` by Louis Reasoner as follows:-

{% highlight scheme %}
(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))
{% endhighlight %}

This would not work. When we evaluate `(pairs integers integers)`, we would also recursively evaluate `(pairs (stream-cdr integers) (stream-cdr integers))` immediately to feed as an argument to `interleave`. This would lead to an infinite recursion and stack to blow up.

### Exercise 3.69<a id="Exercise3_69">&nbsp;</a>

In this exercise, we are tasked with defining a stream `triples` that produces a stream of triples `(i j k)` where $$i<=j<=k$$. Then, we are tasked with using `triples` to generate the stream of all [Pythagorean triples](https://www.wikiwand.com/en/Pythagorean_triple). The code is as follows:-

{% highlight scheme %}
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x)
                  (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (x)
                   (= (+ (square (car x))
                         (square (cadr x)))
                      (square (caddr x))))
                 (triples integers integers integers)))
{% endhighlight %}