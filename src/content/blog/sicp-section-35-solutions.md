---
title: "SICP Section 3.5 solutions"
description: "SICP exercises 3.50 - 3.82 - Section 3.5 solutions"
pubDate: 2017-05-06
tags: ["sicp", "computer-science", "scheme", "programming", "functional-programming"]
---

In this section, we are introduced to streams. Implementation of streams require lazy evaluation. Since all of user-created functions in Scheme is eagerly evaluated, we need to use [macros](https://en.wikipedia.org/wiki/Macro_(computer_science)). They can be declared using the `define-syntax` command as follows:-

```scheme
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
```

### Exercise 3.50

In this exercise, we are tasked with creating a generalized `stream-map` function that can take multiple streams and a procedure that takes as many arguments as the number of streams provided. The code can be implemented as follows:-

```scheme
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))
```

### Exercise 3.51

In this exercise, we are tasked with executing the following code:-

```scheme
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
```

*0* is printed immediately since `stream-map` immediately accesses the first element. Subsequently, the next elements are accessed and their values are displayed. Calling `(stream-ref x 5)` accesses the first five elements and diplays their value. Calling `(stream x 7)` next displays only *6* and *7* because the previous values have already been memoized by the `delay` macro. Thus the values were returned without displaying them first. Also, each call displays the final number twice. Once is from the actual `show` and the other is the value returned by `stream-ref`.

### Exercise 3.52

In this exercise, we are tasked with tracing the value of `sum` as the following code is executed:-

```scheme
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)
```

At this point `sum` is *0* since it has not been mutated yet.

```scheme
(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
```

At this point `sum` is *1* since the `stream-map` processes the first value in the interval stream and calls `accum`.

```scheme
(define y (stream-filter even? seq))
```

At the end of this call, `sum` holds the value of *6*. This is because *stream-filter* consumes value until the first element that passes its criteria. That first value would be $$1+2+3=6$$.

```scheme
(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
```

At the end of this call, `sum` holds the value of *10*. This is because the first element in `seq` that is divisible by five is $$1+2+3+4=10$$.

```scheme
(stream-ref y 7)
; 136
```

At the end of this call, `sum` holds the value of *136* which is the 7th event value of `seq` is $$1+...+16=136$$.

```scheme
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
```

At the end of this call, `sum` holds the value of *210*. This is because `display-stream` goes through all of `seq`. Thus the final value is $$1+...+20=210$$.

If the `delay` function has not been memoized, we will get different values in this case. This is because of the side-effect in the function given to `stream-map`. Each time the stream is used, the value of `sum` would have mutated and it would be hard to predict the value. By memoization, we restrict the side effect to only the first time the stream is resolved.

### Exercise 3.53

In this exercise, we are asked to describe the elements of the stream defined as follows:-

```scheme
(define s (cons-stream 1 (add-streams s s)))
```

The first element is obviously *1* as defined. The second element is the sum of first element with itself. Thus it is *2*. The third element would be the sum of the second element with itself. It will be *4*. In essense, we get $$1 2 4 8 16 32 64 ...$$. Another way to look at it would be through the following code transformation:-

```scheme
(define s (cons-stream 1 (add-streams s s)))

(define s (cons-stream 1 (stream-map (lambda (x) (* 2 x)) s)))

(define s (cons-stream 1 (scale-stream s 2)))

double
```

As can be seen, the given stream is the same as the one already given in the book which gives the same value.

### Exercise 3.54

In this exercise, we are taked with defining a stream for calculating factorial using `mul-streams` function. The code to do so is as follows:-

```scheme
(define (mul-streams s1 s2) 
  (stream-map * s1 s2))
```

We can then define `factorials` stream using the following code:-

```scheme
(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))
```

### Exercise 3.55

In this exercise, we are asked to define `partial-sums`, a function that takes in a stream $$S$$ and outputs $$S_0, S_0+S_1, S_0+S_1+S_2,...$$. The function can be defined as follows:-

```scheme
(define (partial-sums s)
  (cons-stream (stream-car s)
               (stream-map (lambda (x) (+ x (stream-car s)))
                           (partial-sums (stream-cdr s)))))
```

### Exercise 3.56

In this exercise, we are aked to define `S` which is a stream of numbers beginning with *1* and contains numbers whose multiples are *2*, *3* and *5*. We are given a hint that `(scale-stream S 2)`, `(scale-stream S 3)` and `(scale-stream S 5)` also belongs in `S`. Given this and a merge function, `S` can be defined as follows:-

```scheme
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
                    s2))))))))

(define S
  (cons-stream 1
               (merge (merge (scale-stream S 2) (scale-stream S 3))
                      (scale-stream S 5))))
```

### Exercise 3.57

In this task, we asked for the number of additions performed when $$n^{th}$$ fibonacci number is calculated by the given `fibs` stream. Since the `dela` function is memoized, the time taken for calculating `fibs` is $$O(n)$$ since all *n-1* terms required for its computation has already been computed and can be recovered in $$O(1)$$ time. Now, if the streams is not memoized, as calculated in Chapter 1, we take $$O(fib(n))$$ time to calculate.

### Exercise 3.58

In this exercise, we are given the following stream:-

```scheme
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
```

When we call `(expand 1 7 10)`, we get the following:-

```scheme
(expand 1 7 10)
(cons-stream 1 (expand 3 7 10))
(cons 1 (cons-stream 4 (expand 2 7 10)))
(cons 1 (cons 4 (cons-stream 2 (expand 6 7 10))))
...
; 1 4 2 8 5 7 1 4 2 8
```

Next, when we call `(expand 3 8 10)`, we get the following:-

```scheme
(expand 3 8 10)
(cons-stream 3 (expand 6 8 10))
(cons 3 (cons-stream 7 (expand 4 8 10)))
(cons 3 (cons 7 (cons-stream 5 (expand 0 8 10))))
...
; 3 7 5 0 0 0 0 0 0
```

In essence, this stream performs [long division](https://www.wikiwand.com/en/Long_division) to calculate $$\text{num}/\text{den}$$ with the given radix or base.

### Exercise 3.59

In this exercise, we are tasked wtih creating a procedure `integrate-series` which takes as input a stream $$a_0$$, $$a_1$$, $$a_2$$,... which denotes a polynomial $$a_0+a_1x+a_2x^2+...$$ and gives its stream which denotes its integrated polynomial without the constant term, $$a_0x+\frac{1}{2}a_1x^2+\frac{1}{3}a_2x^3+...$$ as a stream $$a_0$$, $$\frac{1}{2}a_1$$, $$\frac{1}{3}a_2$$,... . First, let us define `integrate-series` as following:-

```scheme
(define (integrate-series series)
  (define (integrate-helper s divisor)
    (cons-stream (/ (stream-car s) divisor)
                 (integrate-helper (stream-cdr s) (+ divisor 1))))
  (integrate-helper series 1))
```

Next, we need to define `cosine-series` and `sine-series` using this function. It can be done as follows:-

```scheme
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series)
                               -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
```

The series are defined using [mutual recursion](https://www.wikiwand.com/en/Mutual_recursion). As more terms are needed, the counterpart generates enough terms.

### Exercise 3.60

In this exercise, we are tasked with writing a `mul-series` function that multiplies two series streams as defined in the above exercise. It can be done as follows:-

```scheme
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series s2
                                        (stream-cdr s1)))))
```

We are basically calculating 

$$\begin{align}
(a_0 + a_{rest})*(b_0 + b_{rest}) &= (a_0 * b_0) + (a_0 * b_{rest}) + (a_{rest} * b_0) + (a_{rest} * b_{rest}) 
\\&= (a_0 * b_0) + (a_0 * b_{rest}) + (a_{rest} * b)
\end{align}$$

### Exercise 3.61

In this exercise, we are tasked with writing a function that computes the inverse of a series. It can be done as follows:-

```scheme
(define (invert-unit-series series)
  (define inverted
    (cons-stream 1
                 (scale-stream (mul-series (stream-cdr series)
                                           inverted)
                               -1)))
  inverted)
```

### Exercise 3.62

In this exercise, we are tasked with creating `div-series` which divides two power series. It should work for all denominators with non-zero constant. We can go about it as follows:-

```scheme
(define (div-series num-series den-series)
  (let ((divisor (stream-car den-series)))
    (if (= divisor 0)
        (error "Cannot divide denominator with 0 constant term")
        (mul-series (invert-unit-series (scale-stream den-series
                                                      (/ 1 divisor)))
                    num-series))))
```

### Exercise 3.63

In this exercise, we are given the following modified version for `sqrt-stream`:-

```scheme
(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))
```

The reason is that each recursive call of `(sqrt-stream x)` constructs a new stream. This means extracting value from this stream would take fresh computations. This is exactly what would happen if we did not memoize `delay`.

### Exercise 3.64

In this exercise, we are tasked with implementing `stream-limit` which takes a stream and a tolerance number. The function then examines the stream until it finds two successive elements which differ by less than tolerance and return the second number. It can be implemented as follows:-

```scheme
(define (stream-limit s tol)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0))
           tol)
        s1
        (stream-limit (stream-cdr s) tol))))
```

### Exercise 3.65

In this exercise, we are tasked with creating a stream to compute $$ln2 = 1 - \frac{1}{2} + \frac{1}{3} - \frac{1}{4} + ...$$. Then, we apply [Euler's series acceleration transformation](https://www.wikiwand.com/en/Series_acceleration#/Euler.27s_transform) to examine the convergence characteristics. The `ln2-stream` can be defined as follows:-

```scheme
(define (ln2-summands n)
        (cons-stream
         (/ 1.0 n) 
         (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
        (partial-sums (ln2-summands 1)))
```

Taking into account $$ln2=0.69314718056$$, let us examine the convergences,

```scheme
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
```

As can be seen, recursive acceleration provides the best convergence.

### Exercise 3.66

In this exercise, we are given the following code to generate pairs of integers:-

```scheme
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
```

We are tasked with examining the pairs generated by calling `(pairs integers integers)` and calculated the number of pairs preceding given pairs at least approximately.

##### `(1 100)`

The first level of call gives `(1 1)` as the first pair and interleaves `(1 2) (1 3) (1 4) ...` with `(pairs (2 3 ...) (2 3 ...))`. This means `(1 x)` appears as every second element. `(1 2)` has 1 preceding element. `(1 3)` has 3 preceding elements. `(1 4)` has 5 preceding elements. This gives the relationship where `(1 n)` has $$2n-3$$ elements before it. Thus, `(1 100)` has *197* elements preceding it.

##### `(99 100)`

For finding the number of preceding elements for `(99 100)`, lets us peel out the `(1 x)` terms, from the previous case. This results in `(2 x)` occuring every other terms in a sequence interleaved with `(pairs (3 4 ...) (3 4 ...))`. Overall, this leads to `(2 x)` occuring every $$2^2=4$$ terms in the overall sequence. Extending this, we can conclude that the terms `(n x)` occur every $$2^n$$ steps. Next, we need to determine the first terms in such a sequence `(x x)`. As can be seen, `(1 1)` is the first element, `(2 2)` is the third element and `(3 3)` is the seventh element. Extending this, we can see that `(n n)` occurs as the term number $$2^n-1$$ in proportion to term spacing.  Thus, the term `(99 99)` occurs as term number $$2^99-1$$ and `(99 100)` occurs $$2^100$$ terms after that. We thus get $$2^100 + 2^99 - 2$$ terms preceding `(99 100)`

##### `(100 100)`

As determined in the previous segment, `(100 100)` has $$2^100-1$$ preceding elements.

### Exercise 3.67

In the above exercise, the `pairs` stream generates pair of form `(i j)` where $$i<=j$$. In this exercise, we are tasked with generating a stream with all pairs without this condition. This can be done by modifying the `pairs` function as follows:-

```scheme
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
```

### Exercise 3.68

In this exercise, we are given an alternative definition for `pairs` by Louis Reasoner as follows:-

```scheme
(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))
```

This would not work. When we evaluate `(pairs integers integers)`, we would also recursively evaluate `(pairs (stream-cdr integers) (stream-cdr integers))` immediately to feed as an argument to `interleave`. This would lead to an infinite recursion and stack to blow up.

### Exercise 3.69

In this exercise, we are tasked with defining a stream `triples` that produces a stream of triples `(i j k)` where $$i<=j<=k$$. Then, we are tasked with using `triples` to generate the stream of all [Pythagorean triples](https://www.wikiwand.com/en/Pythagorean_triple). The code is as follows:-

```scheme
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
```

### Exercise 3.70

In this exercise, we are tasked with writing `weighted-pairs` which is similar to `pairs` except it also takes a weighing function and produces pairs ordered according to the weights. The code is as follows:-

```scheme
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((> (weight s1car) (weight s2car))
                  (cons-stream 
                   s2car 
                   (merge-weighted
                    s1 
                    (stream-cdr s2)
                    weight)))
                 (else
                  (cons-stream 
                   s1car
                   (merge-weighted 
                    (stream-cdr s1)
                    s2
                    weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))
```

Note that we assume weights are monotonously increasing for pairs `(x y) (x (+ y 1)) (x (+ y 2))...`.

Let us see the code for the required orders.

##### Ordered according to sum $$i+j$$

```scheme
(define pairs1
  (weighted-pairs integers
                  integers
                  (lambda (x)
                    (+ (car x)
                       (cadr x)))))
```

##### Ordered according to sum $$2i+3j+5ij$$

```scheme
(define int235
  (stream-filter (lambda (x) (not (or (= (remainder x 2) 0)
                                      (= (remainder x 3) 0)
                                      (= (remainder x 5) 0))))
                 integers))

(define pairs2
  (weighted-pairs int235
                  int235
                  (lambda (x)
                    (+ (* 2 (car x))
                       (* 3 (cadr x))
                       (* 5 (car x) (cadr x))))))
```

### Exercise 3.71

In this exercise, we are tasked with generating a stream of [Ramanujam numbers](http://mathworld.wolfram.com/Hardy-RamanujanNumber.html). The code for that is as follows:-

```scheme
(define (ramanujam-numbers)
  (define (cube-sum x)
    (+ (* (car x) (car x) (car x))
       (* (cadr x) (cadr x) (cadr x))))
  (define (cube-stream)
    (weighted-pairs integers integers cube-sum))
  (define (stream-cadr s) (stream-car (stream-cdr s)))
  (define (same-successive s)
    (let ((first (stream-car s))
          (second (stream-cadr s)))
      (if (= (cube-sum first)
             (cube-sum second))
          (cons-stream (list (cube-sum first) first second)
                       (same-successive (stream-cdr s)))
          (same-successive (stream-cdr s)))))
  (same-successive (cube-stream)))
```

Running this code, we get the following numbers:-

$$\begin{align}
1^3 + 12^3 &= 9^3 + 10^3 &&= 1729 \\
2^3 + 16^3 &= 9^3 + 15^3 &&= 4104 \\
2^3 + 24^3 &= 18^3 + 20^3 &&= 13832 \\
10^3 + 27^3 &= 19^3 + 24^3 &&= 20683 \\
4^3 + 32^3 &= 18^3 + 30^3 &&= 32832
\end{align}$$

### Exercise 3.72

In this exercise, we are tasked with generating a stream of all numbers that can be written as the sum of two squares in three different ways. We can do that similar to `ramanujam-numbers` stream above:-

```scheme
(define (sum-square-three-ways)
  (define (square-sum x)
    (+ (* (car x) (car x))
       (* (cadr x) (cadr x))))
  (define (square-stream)
    (weighted-pairs integers integers square-sum))
  (define (stream-cadr s) (stream-car (stream-cdr s)))
  (define (stream-cddr s) (stream-cdr (stream-cdr s)))
  (define (stream-caddr s) (stream-car (stream-cddr s)))
  (define (same-successive s)
    (let ((first (stream-car s))
          (second (stream-cadr s))
          (third (stream-caddr s)))
      (if (= (square-sum first)
             (square-sum second)
             (square-sum third))
          (cons-stream (list (square-sum first) first second third)
                       (same-successive (stream-cddr s)))
          (same-successive (stream-cdr s)))))
  (same-successive (square-stream)))
```

The first five such numbers are as follows:-

$$\begin{align}
1^2 + 18^2 &= 6^2 + 17^2 &&= 10^2 + 15^2 &&&= 325 \\
5^2 + 20^2 &= 8^2 + 19^2 &&= 13^2 + 16^2 &&&= 425 \\
5^2 + 25^2 &= 11^2 + 23^2 &&= 17^2 + 19^2 &&&= 650 \\
7^2 + 26^2 &= 10^2 + 25^2 &&= 14^2 + 23^2 &&&= 725 \\
2^2 + 29^2 &= 13^2 + 26^2 &&= 19^2 + 22^2 &&&= 845
\end{align}$$

### Exercise 3.73

In this exercise, we are tasked with creating a stream simulating a circuit $$v=v_0+\frac{1}{C}\int_{0}^{t} i dt + Ri$$. We need to create a function `RC` that takes resistance *R* and capacitance *C* along with time step *dt* and returns a function. This function should take the stream representing the time sequence of currents *i* and initial voltage $$v_0$$ and return a stream of output voltage *v*. The code for that is as follows:-

```scheme
(define (RC R C dt)
  (define (circuit i v0)
    (add-streams (integral (scale-stream i (/ 1.0 C)) v0 dt)
                 (scale-stream i R)))
  circuit)
```

### Exercise 3.74

In this exercise, we are tasked with filling out the code given by Eva Lu Ator to simplify `zero-crossings` function. The code is as follows:-

```scheme
(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (stream-cdr sense-data)))
```

### Exercise 3.75

We are given a function from Louis Reasoner to smooth out a stream of numbers by averaging successive numbers to produce a smoothed zero-crossings function. The given code is as follows:-

```scheme
(define (make-zero-crossings 
         input-stream last-value)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt last-value)
     (make-zero-crossings 
      (stream-cdr input-stream) avpt))))
```

The main mistake being done in the code is that we are feeding the current averge and last value to the `sign-change-detector`. The correct method is to feed the previous average and the current average. Thus the correct code is as follows:-

```scheme
(define (make-zero-crossings 
         input-stream last-value last-average)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt last-average)
     (make-zero-crossings 
      (stream-cdr input-stream) (stream-car input-stream) avpt))))
```

### Exercise 3.76

We are advised by Eva Lu Ator to modularize our code so as to separate out the smoothing algorithm and the crossings calculation. The code to do that is as follows:-

```scheme
(define (smooth input-stream)
  (stream-map (lambda (x y)
                (/ (+ x y ) 2))
              input-stream
              (stream-cdr input-stream)))

(define (make-zero-crossings input-stream)
  (let ((smoothed-stream (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed-stream
                (stream-cdr smoothed-stream))))
```

### Exercise 3.77

We are required to modify the given `integral` procedure to use a delayed integrand function. The modified function is as follows:-

```scheme
(define (integral
         delayed-integrand initial-value dt)
  (cons-stream 
   initial-value
   (let ((integrand 
          (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral 
          (stream-cdr integrand)
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))
```

### Exercise 3.78

In this exercise, we are asked to design a signal-processing system to solve $$\frac{d^2y}{dt^2}-a\frac{dy}{dt}-by=0$$. The code is as follows:-

```scheme
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream ddy b)))
  y)
```

### Exercise 3.79

We are now asked to modify `solve-2nd` to solve equations of form $$\frac{d^2y}{dt^2}=f(\frac{dy}{dt}, y)$$. The code to do that is as follows:-

```scheme
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map dy y))
  y)
```

### Exercise 3.80

In this exercise, we are asked to simulate an series RLC circuit with *R*, *L* and *C* as the resistance, inductance and capacitance. The circuit can be simulated as follows:-

```scheme
(define (RLC R L C dt)
  (define (circuit vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ (- 1) C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (/ (- R) L))))
    (cons vC iL))
  circuit)
```

The stream for R = 1 ohm, CC = 0.2 farad, LL = 1 henry, dt = 0.1 second, and initial values iL0 = 0 amps and vC0 = 10 volts can be generated as follows:-

```scheme
(define RLC1 (RLC 1 0.2 1 0.1))
(define streams (RLC1 0 10))
(define vC (car streams))
(define iL (cdr streams))
```

### Exercise 3.81

In this exercise, we are tasked with creating a random stream function that takes a command streams containing either `'generate` or `('reset n)`. The code for that is as follows:-

```scheme
(define (rand-update x)
  (let ((m (expt 2 31))
        (a 1103515245)
        (c 12345))
    (modulo (+ (* a x) c) m)))

(define (random-numbers command-stream)
  (define (rand-helper num command-stream)
    (let ((command (stream-car command-stream)))
      (cond ((eq? command 'generate)
             (cons-stream num
                          (rand-helper (rand-update num)
                                       (stream-cdr command-stream))))
            ((and (pair? command)
                  (eq? (car command) 'reset))
             (cons-stream (cdr command)
                          (rand-helper (rand-update (cdr command))
                                       (stream-cdr command-stream)))))))
  (rand-helper 0 command-stream))
```

### Exercise 3.82

In this exercise, we are tasked with implementing `estimate-integral` which can perform Monte Carlo integration. The code for that is as follows:-

```scheme
(define (rand-stream)
  (define command-stream (cons-stream 'generate command-stream))
  (random-numbers command-stream))

(define (estimate-integral p x1 x2 y1 y2)
  (define (trial-stream rand-stream)
    (let ((xraw (stream-car rand-stream))
          (yraw (stream-car (stream-cdr rand-stream))))
      (let ((x (+ x1 (modulo xraw (- x2 x1))))
            (y (+ y1 (modulo yraw (- y2 y1)))))
        (cons-stream (p x y)
                     (trial-stream (stream-cdr (stream-cdr rand-stream)))))))
  (trial-stream (rand-stream)))
```
