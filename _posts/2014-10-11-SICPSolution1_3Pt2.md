---
layout: post
category: [SICP, Solutions]
post_no: 8
title: "SICP Section 1.3 Exercise Solutions - Part 2"
submenu:
  - { hook: "Exercise1_34", title: "Exercise 1.34" }
  - { hook: "Exercise1_35", title: "Exercise 1.35" }
  - { hook: "Exercise1_36", title: "Exercise 1.36" }
  - { hook: "Exercise1_37", title: "Exercise 1.37" }
  - { hook: "Exercise1_38", title: "Exercise 1.38" }
  - { hook: "Exercise1_39", title: "Exercise 1.39" }
---

### Exercise 1.34<a name="Exercise1_34">&nbsp;</a>

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

<!--excerpt-->

### Exercise 1.35<a name="Exercise1_35">&nbsp;</a>

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

### Exercise 1.36<a name="Exercise1_36">&nbsp;</a>

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

### Exercise 1.37<a name="Exercise1_37">&nbsp;</a>

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

### Exercise 1.38<a name="Exercise1_38">&nbsp;</a>

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

### Exercise 1.39<a name="Exercise1_39">&nbsp;</a>

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
