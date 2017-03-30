
---
layout: spiedpage
order: 26
title: Section 3.5 solutions
exercises: '3.50 - 3.51'
submenu:
  - { hook: "Exercise3_50", title: "Exercise 3.50" }
  - { hook: "Exercise3_51", title: "Exercise 3.51" }
  - { hook: "Exercise3_52", title: "Exercise 3.52" }
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