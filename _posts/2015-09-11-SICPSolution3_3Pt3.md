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
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'print-queue) print-queue)
            (else (error "Undefined operation: MAKE-QUEUE" m))))
    dispatch))

(define (print-queue queue) ((queue 'print-queue)))
(define (front-queue queue) ((queue 'front-queue)))
(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  (print-queue queue))
(define (delete-queue! queue)
  ((queue 'delete-queue!))
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

In this exercise, we are tasked with implementing a double ended queue called `deque`. This data structure is similar to the queue data structure from before, except we are able to add elements to the back of the queue and remove elements from the front of the queue. To do this, we need to alter the queue nodes to refer to both the element after and before itself.

{% highlight scheme %}
(define (make-deque)
  (define (make-node item)
    (cons item
          (cons '() '())))
  (define (node-item node)
    (car node))
  (define (node-next node)
    (cadr node))
  (define (node-prev node)
    (cddr node))
  (define (set-node-next! node next)
    (set-car! (cdr node) next))
  (define (set-node-prev! node prev)
    (set-cdr! (cdr node) prev))

  (let ((front-ptr '())
        (rear-ptr '()))

    (define (print-deque)
      (define (flatten ptr)
        (if (null? ptr)
            '()
            (cons (node-item ptr)
                  (flatten (node-next ptr)))))
      (newline)
      (display (flatten front-ptr)))
    
    (define (empty-deque?)
      (or (null? front-ptr)
          (null? rear-ptr)))
      
    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty deque" front-ptr)
          (node-item front-ptr)))
    
    (define (rear-deque)
      (if (empty-deque?)
          (error "REAR called with an empty deque" rear-ptr)
          (node-item rear-ptr)))

    (define (front-insert-deque! item)
      (let ((new-node (make-node item)))
        (cond ((empty-deque?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-node-prev! front-ptr new-node)
               (set-node-next! new-node front-ptr)
               (set! front-ptr new-node)))))

    (define (rear-insert-deque! item)
      (let ((new-node (make-node item)))
        (cond ((empty-deque?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-node-next! rear-ptr new-node)
               (set-node-prev! new-node rear-ptr)
               (set! rear-ptr new-node)))))
    
    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "FRONT-DELETE! called with an empty deque" front-ptr))
            (else
             (set! front-ptr (node-next front-ptr))
             (if (null? front-ptr)
                 (set! rear-ptr '())
                 (set-node-prev! front-ptr '())))))

    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "REAR-DELETE! called with an empty deque" rear-ptr))
            (else
             (set! rear-ptr (node-prev rear-ptr))
             (if (null? rear-ptr)
                 (set! front-ptr '())
                 (set-node-next! rear-ptr '())))))

    (define (dispatch m)
      (cond ((eq? m 'print-deque) print-deque)
            ((eq? m 'empty-deque?) empty-deque?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)))
    dispatch))

(define (print-deque deque) ((deque 'print-deque)))
(define (front-deque deque) ((deque 'front-deque)))
(define (rear-deque deque) ((deque 'rear-deque)))
(define (empty-deque? deque) ((deque 'empty-deque?)))
(define (front-insert-deque! deque item)
  ((deque 'front-insert-deque!) item)
  (print-deque deque))
(define (rear-insert-deque! deque item)
  ((deque 'rear-insert-deque!) item)
  (print-deque deque))
(define (front-delete-deque! deque)
  ((deque 'front-delete-deque!))
  (print-deque deque))
(define (rear-delete-deque! deque)
  ((deque 'rear-delete-deque!))
  (print-deque deque))
{% endhighlight %}

Let us test this code:-

{% highlight scheme %}
(define dq (make-deque))
;dq

(print-deque dq)
; ()
(front-insert-deque! dq 'a)
; (a)
(front-insert-deque! dq 'b)
; (b a)
(rear-delete-deque! dq)
; (b)
(rear-insert-deque! dq 'c)
; (b c)
(front-delete-deque! dq)
; (b)
{% endhighlight %}