
---
layout: spiedpage
order: 26
title: Section 3.5 solutions
exercises: '3.50'
submenu:
  - { hook: "Exercise3_50", title: "Exercise 3.50" }
---

In this section, we are introduced to streams. Implementation of streams require lazy evaluation. Since all of user-created functions in Scheme is eagerly evaluated, we need to use [macros](https://en.wikipedia.org/wiki/Macro_(computer_science)). They can be declared using the `define-syntax` command as follows:-

{% highlight scheme %}
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))
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