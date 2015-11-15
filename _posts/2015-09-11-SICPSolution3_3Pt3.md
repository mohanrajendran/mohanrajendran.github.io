---
layout: post
category: [SICP, Solutions]
post_no: 43
title: "SICP Section 3.3 Exercise Solution Part 3"
submenu:
- { hook: "Exercise 3_21", title: "Exercise 3.21" }
- { hook: "Exercise 3_22", title: "Exercise 3.22" }
- { hook: "Exercise 3_23", title: "Exercise 3.23" }
---

### Exercise 3.21<a name="Exercise3_21">&nbsp;</a>

In this exercise, we are tasked with explaining why the output of the queue is not wrong like what Ben Bitdiddle thinks. He evaluates the following code:-

{% highlight scheme %}
(define q1 (make-queue))
; q1

(insert-queue! q1 'a)
; ((a) a)
(insert-queue! q1 'b)
; ((a b) b)

(delete-queue! q1)
; ((b) b)
(delete-queue! q1)
; (() b)
{% endhighlight %}

<!--excerpt-->

He claims that even after de-queuing all the inserted items in *q1*, we still the last element *b* being printed out. This is because we do not shift the `rear-ptr` when we dequeue the queue. Thus, when we simply display the queue object, we print both the pointers. The correct way to interpret the state of the queue is to print out the `front-ptr` alone. Thus, we define a `print-queue` function as follows:-

{% highlight scheme %}
(define (print-queue queue)
  (newline)
  (display (front-ptr queue)))
; print-queue

(define q1 (make-queue))
; q1

(print-queue q1)
; ()
(print-queue (insert-queue! q1 'a))
; (a)
(print-queue (insert-queue! q1 'b))
; (a b)

(print-queue (delete-queue! q1))
; (b)
(print-queue (delete-queue! q1))
; ()
{% endhighlight %}

As can be see, we print out the representation with the new function.


### Exercise 3.22<a name="Exercise3_22">&nbsp;</a>

In this exercise, we are told to implement a queue with local state. It can be done so with the following code:-

{% highlight scheme %}
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (print-queue)
      (newline)
      (display front-ptr))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" front-ptr)
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue" front-ptr))
	    (else
	     (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'print-queue) (print-queue))
	    (else (error "Undefined operation: MAKE-QUEUE" m))))
    dispatch))

(define (print-queue queue) (queue 'print-queue))
(define (front-queue queue) (queue 'front-queue))
(define (empty-queue? queue) (queue 'empty-queue?))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  (print-queue queue))
(define (delete-queue! queue)
  (queue 'delete-queue!)
  (print-queue queue))
{% endhighlight %}

Let us test this with the code from previous exercise:-

{% highlight scheme %}
(define q1 (make-queue))
; q1

(insert-queue! q1 'a)
; (a)
(insert-queue! q1 'b)
; (a b)

(delete-queue! q1)
; (b)
(delete-queue! q1)
; ()
{% endhighlight %}

### Exercise 3.23<a name="Exercise3_23">&nbsp;</a>

