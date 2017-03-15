---
layout: spiedpage
order: 22
title: Section 3.3 solutions
exercises: '3.12 - 3.27'
submenu:
  - { hook: "Exercise3_12", title: "Exercise 3.12" }
  - { hook: "Exercise3_13", title: "Exercise 3.13" }
  - { hook: "Exercise3_14", title: "Exercise 3.14" }
  - { hook: "Exercise3_15", title: "Exercise 3.15" }
  - { hook: "Exercise3_16", title: "Exercise 3.16" }
  - { hook: "Exercise3_17", title: "Exercise 3.17" }
  - { hook: "Exercise3_18", title: "Exercise 3.18" }
  - { hook: "Exercise3_19", title: "Exercise 3.19" }
  - { hook: "Exercise3_20", title: "Exercise 3.20" }
  - { hook: "Exercise3_21", title: "Exercise 3.21" }
  - { hook: "Exercise3_22", title: "Exercise 3.22" }
  - { hook: "Exercise3_23", title: "Exercise 3.23" }
  - { hook: "Exercise3_24", title: "Exercise 3.24" }
  - { hook: "Exercise3_25", title: "Exercise 3.25" }
  - { hook: "Exercise3_26", title: "Exercise 3.26" }
  - { hook: "Exercise3_27", title: "Exercise 3.27" }
  - { hook: "Exercise3_28", title: "Exercise 3.28" }
  - { hook: "Exercise3_29", title: "Exercise 3.29" }
  - { hook: "Exercise3_30", title: "Exercise 3.30" }
---

### Exercise 3.12<a id="Exercise3_12">&nbsp;</a>

In this exercise, we are tasked with investigating a mutable version of the `append` function seen before called `append!` as defined below:-

{% highlight scheme %}
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
{% endhighlight %}

The problem is set up by calling the following:-

{% highlight scheme %}
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
; (a b c d)
{% endhighlight %}

Appending by definition makes a copy of the first argument and appends the existing second argument to the end of this copied list. Thus, the box and pointer diagram at this point is as follows:-

<center>
<img src="/images/Ex3_12_Step1.svg" alt="After defining z" width="500"/>
</center>

Next, we are told to call `(cdr x)`. As can be seen from the box and pointer diagram, we expect to see `(b)` since the original list is not modified. This can be verified by the following code:-

{% highlight scheme %}
(cdr x)
; (b)
{% endhighlight %}

Next, we set things up using the new code:-

{% highlight scheme %}
(define w (append! x y))

w
; (a b c d)
{% endhighlight %}

This appends in-place and thus modifies its arguments. The box and pointer diagram changes as follows:-

<center>
<img src="/images/Ex3_12_Step2.svg" alt="After defining w" width="500"/>
</center>

This modifies the value of `x` as well. Thus, calling `(cdr x)` at this point should yield `(b c d)`. Let us verify this:- 

{% highlight scheme %}
(cdr x)
; (b c d)
{% endhighlight %}


### Exercise 3.13<a id="Exercise3_13">&nbsp;</a>

In this exercise, we are tasked with examining the following procedure `make-cycle` and the results of executing the following code:-

{% highlight scheme %}
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b' 'c)))
{% endhighlight %}

It results in the following box and pointer diagram as follows:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="After making cycle"/>
</center>

As can be seen, the list starting at *z* does not have a `nil` pointer anywhere. Thus, if we call `(last-pair z)`, we expect the execution to go on forever because of the cycle we have created. It is verified by calling it in the scheme REPL. The code keeps running.


### Exercise 3.14<a id="Exercise3_14">&nbsp;</a>

In this exercise, we are tasked with understanding the following `mystery` function:-

{% highlight scheme %}
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
{% endhighlight %}

This function takes a list and evaluates to its reverse. The box and pointer diagram for *v* where `(define v (list 'a 'b 'c 'd))` is as follows:-

<center>
<img src="/images/Ex3_14_Initial.svg" alt="After defining v"/>
</center>

Now, when we call `(define w (mystery v))`, the following evaluation happens:-

{% highlight scheme %}
(loop '(a b c d) '())

; x = '(a b c d) y = '() temp = '(b c d)
((set-cdr! x y)
 (loop temp x))

; x = '(b c d) y = '(a) temp = '(c d)
((set-cdr! x y)
 (loop temp x))

; x = '(c d) y = '(b a) temp = '(d)
((set-cdr! x y)
 (loop temp x))

; x = '(d) y = '(c b a) temp = '()
((set-cdr! x y)
 (loop temp x))

; x = '() y = '(d c b a)
y
{% endhighlight %}

The final state of the pointers look like the following:-

<center>
<img src="/images/Ex3_14_Final.svg" alt="After defining w"/>
</center>

Due to the mutating nature of this function, the original value of *v* also changes. Let us verify that:-

{% highlight scheme %}
(define v (list 'a 'b 'c 'd))
; v

(define w (mystery v))
; w

v
; (a)

w
; (d c b a)
{% endhighlight %}

### Exercise 3.15<a id="Exercise3_15">&nbsp;</a>

In this exercise, we are tasked with explaining the difference in behavior when a function `set-to-wow!` is applied to two seemingly similar lists:-

{% highlight scheme %}
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 
  (cons (list 'a 'b) (list 'a 'b)))

z1
; ((a b) a b)
z2
; ((a b) a b)

(set-to-wow! z1)
; ((wow b) wow b)
(set-to-wow! z2)
; ((wow b) a b)
{% endhighlight %}

Even though *z1* and *z2* looked the same initially, calling `set-to-wow!` on them produces different results. They can be explained by the following box and pointer diagrams resulting from the application:-

<center>
<img src="/images/Ex3_15.svg" alt="After application of set-to-wow!"/>
</center>

As can be seen, both `car` and `cdr` of *z1* point to the same object. Thus, mutating one changes the other. As for *z2*, we had two exact same copies pointing to same elements down the road. Calling `set-to-wow!` on this only modified one copy, leaving the other alone.

### Exercise 3.16<a id="Exercise3_16">&nbsp;</a>

In this exercise, we are told to examine the following `count-pairs` function written by Ben Bitdiddle:-

{% highlight scheme %}
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
{% endhighlight %}
	 
On the outset, we can see that this function will not work correctly because it does not consider pairs that might be pointed to from more than one location. They will be counted more than once. We have seen such cases in the previous exercises. Let us construct cases each with three pairs which give the required answers.

#### 3 pairs

This list can be constructed simply by a linear list which yields the following structure:-

<center>
<img src="/images/Ex3_16_3Pairs.svg" alt="Evaluate to 3 pairs"/>
</center>

{% highlight scheme %}
(define w '(a b c))

(count-pairs w)
; 3
{% endhighlight %}

As can be seen, in this simple case, the algorithm gives the right answer.

#### 4 pairs
	
This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_4Pairs.svg" alt="Evaluate to 4 pairs"/>
</center>

When evaluated, *x1* is counted twice and yields 4.

{% highlight scheme %}
(define x1 (cons 'a 'b))
(define x2 (cons x1 'c))
(define x (cons x1 x2))

(count-pairs x)
; 4
{% endhighlight %}

#### 7 pairs

This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_7Pairs.svg" alt="Evaluate to 7 pairs"/>
</center>

In this case, *y2* is counted twice and *y1* is counted 4 times to yield 7.

{% highlight scheme %}
(define y1 (cons 'a 'b))
(define y2 (cons y1 y1))
(define y (cons y2 y2))

(count-pairs y)
; 7 
{% endhighlight %}

#### Unlimited pairs

This list can be constructed by introducing a cycle:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="Evaluate to unlimited pairs"/>
</center>

The code would keep evaluating along the cycle and never return at all.

{% highlight scheme %}
(define z (make-cycle '(a b c)))

(count-pairs z)
; Aborting!: maximum recursion depth exceeded
{% endhighlight %}

### Exercise 3.17<a id="Exercise3_17">&nbsp;</a>
	
In this exercise, we are required to create a correct version of `create-pairs` which returns the correct number of distinct pairs. One good way to do it is to maintain a list of pairs already encountered and only count pairs not seen before. One way is as follows:-

{% highlight scheme %}
(define (count-pairs x)
  (define encountered '())
  (define (count-unique-pairs x)
    (if (and (pair? x)
	     (not (memq x encountered)))
	(begin (set! encountered (cons x encountered))
	       (+ (count-unique-pairs (car x))
		  (count-unique-pairs (cdr x))
		  1))
	0))
  (count-unique-pairs x))
{% endhighlight %}

Let us test it on the lists defined in the previous exercise:-

{% highlight scheme %}
w
; (a b c)
(count-pairs w)
; 3

x
; ((a . b) (a . b) . c)
(count-pairs x)
; 3

y
; (((a . b) a . b) (a . b) a . b)
(count-pairs y)
; 3

(count-pairs z)
; 3
{% endhighlight %}

### Exercise 3.18<a id="Exercise3_18">&nbsp;</a>

In this exercise, we are tasked with implementing a procedure that determines if a list contains a cycle. It is given that a list with cycle is defined as one that would result in an infinite loop if successive `cdr` is invoked on it. We can use the code from the previous exercise which tracks the pairs seen already to perform this check:-

{% highlight scheme %}
(define (has-cycle? x)
  (define encountered '())
  (define (check-if-seen x)
    (cond ((not (pair? x)) #f)
	      ((memq x encountered) #t)
	      (else (begin (set! encountered (cons x encountered))
		               (check-if-seen (cdr x))))))
  (check-if-seen x))
{% endhighlight %}

Let us go ahead and test this code on the cases from before. Only *z* should result in true:-

{% highlight scheme %}
(has-cycle? w)
; #f

(has-cycle? x)
; #f

(has-cycle? y)
; #f

(has-cycle? z)
; #t
{% endhighlight %}

### Exercise 3.19<a id="Exercise3_19">&nbsp;</a>

In this exercise, we are tasked with re-implementing the `has-cycle?` code from the previous exercise so that only constant space is required. It can be implemented using [Floyd's algorithm](https://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare). 

{% highlight scheme %}
(define (has-cycle? x)
  (define (safe-cdr x)
    (if (pair? x)
	(cdr x)
	'()))
  (define (safe-cddr x)
    (safe-cdr (safe-cdr x)))
  (define (advance-pointer t h)
    (cond ((null? h) #f)
	  ((eq? t h) #t)
	  (else (advance-pointer
		 (safe-cdr t)
		 (safe-cddr h)))))
  (advance-pointer x (safe-cdr x)))
{% endhighlight %}

With this, let us test the cases from before:-

{% highlight scheme %}
(has-cycle? w)
; #f

(has-cycle? x)
; #f

(has-cycle? y)
; #f

(has-cycle? z)
; #t

(has-cycle? (cons 'd z))
; #t
{% endhighlight %}


### Exercise 3.20<a id="Exercise3_20">&nbsp;</a>

In this exercise, we are tasked with giving the environment diagrams when the following code is executed:-

{% highlight scheme %}
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined 
                 operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons 1 2))
(define z (cons x x))

(set-car! (cdr z) 17)

(car x)
17
{% endhighlight %}

Initially, when the definition for `cons`, `car`, `cdr`, `set-car!` and `set-cdr!` is set up, the environment looks like the following:-

<center>
<img src="/images/Ex3_20_Step1.svg" alt="After definition of cons"/>
</center>

Next, defining *x* and *z* sets up the following:-

<center>
<img src="/images/Ex3_20_Step2.svg" alt="After defining x and z"/>
</center>

When we call `(cdr z)` we evaluate to *x*. Then, calling `set-car!` on that alters the value of *x* in environment *E1* to 17 as follows:-

<center>
<img src="/images/Ex3_20_Step3.svg" alt="After calling set-car"/>
</center>

We have skipped evaluation of `cdr` and `set-car!` to drill down to the lower levels. Finally, evaluating in *E5* sets the value of *x* in *E1* to 17.

### Exercise 3.21<a id="Exercise3_21">&nbsp;</a>

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


### Exercise 3.22<a id="Exercise3_22">&nbsp;</a>

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

### Exercise 3.23<a id="Exercise3_23">&nbsp;</a>

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

### Exercise 3.24<a id="Exercise3_24">&nbsp;</a>

In this exercise, we are required to re-write the given `make-table` function to take another function as an argument which can be used as a custom equality checker that is different from the `equal?` function used by the given `assoc` function. The new constructor can be simply written by redefining the `assoc` function to use the argument instead.

{% highlight scheme %}
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
       (cond ((null? records) false)
             ((same-key? key (caar records)) 
              (car records))
             (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))
{% endhighlight %}

### Exercise 3.25<a id="Exercise3_25">&nbsp;</a>

In this exercise, we are tasked with implementing a table that can store values under an arbitrary number of keys as opposed to the 1- and 2-dimensional tables described in this section. This can be done using a recursive solution that goes through the keys and traverses the table data structure. Further a record node at any depth can contain a value or another table. 

{% highlight scheme %}
(define (make-n-table)
  (let ((root-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((not (pair? records)) false)
            ((equal? key (caar records)) 
             (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (lookup-helper keys cur-table)
        (if (null? keys)
            (cdr cur-table)
            (let ((subtable
                   (assoc (car keys) (cdr cur-table))))
              (if subtable
                  (lookup-helper (cdr keys) subtable)
                  false))))
      (lookup-helper keys root-table))
    
    (define (insert! keys value)
      (define (insert-helper keys cur-table)
        (if (null? keys)
            (set-cdr! cur-table value)
            (let ((subtable
                   (assoc (car keys) (cdr cur-table))))
              (if subtable
                  (insert-helper (cdr keys) subtable)
                  (let ((new-table (cons (cons (car keys) '())
                                         (cdr cur-table))))
                    (set-cdr! cur-table new-table)
                    (insert-helper (cdr keys) (car new-table)))))))
      (insert-helper keys root-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (lookup table keys) ((table 'lookup) keys))
(define (insert! table keys value) ((table 'insert!) keys value))
{% endhighlight %}

This can be tested using the following code:-

{% highlight scheme %}
(define table (make-n-table))
; table
(lookup table '(a b c))
; #f

(insert! table '(a b c) 42)
(lookup table '(a b c))
; 42

(insert! table '(a b d) 37)
(lookup table '(a b d))
; 37

(insert! table '(a d) 45)
(lookup table '(a d))
; 45

(lookup table '(a b c d))
; #f
{% endhighlight %}

### Exercise 3.26<a id="Exercise3_26">&nbsp;</a>

In this exercise, we are tasked with implementing the search table using a binary tree so that access would take a shorter time than a list. Since a binary tree requires a concept of ordering between keys, we have to make sure all keys are of the same type.

{% highlight scheme %}
(define (make-table comparator<?)
  (define (make-node key value)
    (cons (cons key value)
          (cons '() '())))
  (define (node-key node)
    (caar node))
  (define (node-value node)
    (cdar node))
  (define (node-left node)
    (cadr node))
  (define (node-right node)
    (cddr node))
  (define (set-node-value! node value)
    (set-cdr! (car node) value))
  (define (set-node-left! node left)
    (set-car! (cdr node) left))
  (define (set-node-right! node right)
    (set-cdr! (cdr node) right))

  (let ((root '()))
    (define (lookup key)
      (define (lookup-helper node)
      (cond ((null? node) false)
            ((comparator<? key (node-key node))
             (lookup-helper (node-left node)))
            ((comparator<? (node-key node) key)
             (lookup-helper (node-right node)))
            (else (node-value node))))      
      (lookup-helper root))

    (define (insert! key value)
      (define (insert-helper! node)
        (cond ((comparator<? key (node-key node))
               (if (null? (node-left node))
                   (set-node-left! node (make-node key value))
                   (insert-helper! (node-left node))))
              ((comparator<? (node-key node) key)
               (if (null? (node-right node))
                   (set-node-right! node (make-node key value))
                   (insert-helper! (node-right node))))
              (else (set-node-value! node value))))
      (if (null? root)
          (set! root (make-node key value))
          (insert-helper! root)))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (make-table-with-key-type key-type)
  (let ((comparator (cond ((eq? key-type 'number) <)
                          ((eq? key-type 'char) char<?)
                          ((eq? key-type 'string) string<?)
                          (else (error "Unknown key-type" key-type)))))
    (make-table comparator)))
(define (lookup table key) ((table 'lookup) key))
(define (insert! table key value) ((table 'insert!) key value))
{% endhighlight %}

We can also test the code using a table with string key:-

{% highlight scheme %}
(define string-table (make-table-with-key-type 'string))
; string-table

(lookup string-table "hello")
; #f

(insert! string-table "hello" 42)
(lookup string-table "hello")
; 42

(insert! string-table "world" 45)
(lookup string-table "world")
; 45

(insert! string-table "hello" "new value")
(lookup string-table "hello")
; "new value"
{% endhighlight %}

Since there is no comparison with values, they can be of any arbitrary type.

### Exercise 3.27<a id="Exercise3_27">&nbsp;</a>

In this exercise, we are required to draw en environment diagram for a memoized function defined using the following code:-

{% highlight scheme %}
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))
{% endhighlight %}

The environment right after making these definitions looks like the following:-

<center>
<img src="/images/Ex3_27_Step1.svg" alt="Environment structure for memoized fibonacci"/>
</center>

Now whenever we call `memo-fib`, it first checks if the argumet is available in the `table` binding. If it is, it means that the function had been called previously with the same argument. In that case the retrieved value would be returned immediately. Otherwise the function does the actual fibonacci computation, inserts the value in the table and returns it. The environment diagram for that is as follows:-

<center>
<img src="/images/Ex3_27_Step2.svg" alt="Environment structure evaluating memo-fib"/>
</center>

When `(memo-fib 3)` is evaluated, it creates a new environment **E1** with an empty table. There `previously-computed-result` would be `#f` and it causes it to evaluate `(+ (memo-fib 2) (memo-fib 1))`. The first part is evaulated in **E2** and it in turn spawns **E3** and **E4**. At this point, the terminal conditions give results of *1* and *0* respectively. These two values are cached in the table at this point. The second part of **E2**, `(memo-fib 1)` is evaluated in **E5** finally. At this point, the argument of *1* is already computed and it simply returns the result directly. Assuming that the table lookup uses a constant time, we only need to calculate each number once. Since a call `(memo-fib n)` would entail calculating `(memo-fib (- n 1))` and so on until `(memo-fib 0)`, we require $$n+1$$ calculations and getting an $$O(n)$$ complexity.

Now, if we merely defined `memo-fib` to be `(memoize fib)`, it would simply call `fib` recursively which itself is not memoized. This only the final result end up in the table and  all intermediate calculations need to be repeated.

### Exercise 3.28<a id="Exercise3_28">&nbsp;</a>

In this section, we are primarily interested in simulating digital circuits. We are given examples of `not-gate` and `and-gate`. We are required to define an `or-gate`. It can be defined as follows:-

{% highlight scheme %}
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (or-action! a1 or-action-procedure)
  (or-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1)
             (= s2 1)) 1)
        (else 0)))
{% endhighlight %}

### Exercise 3.29<a id="Exercise3_29">&nbsp;</a>

In this exercise, we are asked to construct an or-gate using and-gates and inverters. For that purpose, we can use the [De Morgan's laws](https://www.wikiwand.com/en/De_Morgan's_laws#/Substitution_form). 

$$(P \vee Q) \equiv \neg(\neg P \wedge \neg Q)$$

Putting it together, we get the following:-

{% highlight scheme %}
(define (or-gate a1 a2 output)
  (let ((a1not (make-wire))
        (a2not (make-wire))
        (outnot (make-wire)))
    (inverter a1 a1not)
    (inverter a2 a2not)
    (and-gate a1not a2not outnot)
    (inverter outnot output)
    'ok))
{% endhighlight %}

In this process, we run the first two inverters simultaneously followed by an *and* operation and an inverter. Thus, the total delay would be:-

$$\text{or-gate-delay} = 2*\text{inverter-delay} + \text{and-gate-delay}$$.

### Exercise 3.30<a id="Exercise3_30">&nbsp;</a>

In this exercise, we are required to implement a  *ripple-carry adder* which can be constructed by connection *n* full-adders serially. This setup takes two n-bit numbers $$A$$ and $$B$$, and outputs $$S=A+B$$ and $$C$$ where C is the carry bit from the addition process. This problem can be solved easily using a recursive solution, where an adder of length *n* can be constructed by taking an adder of length *n-1* and attaching a new full-adder's carry output to the carry input of the ripple-carry adder. A solution is as follows:-

{% highlight scheme %}
(define (ripple-carry-adder A B S C)
  (if (= (length A)
         (length B)
         (length S))
      (if (null? A)
          (set-signal! C 0)
          (let ((carry-wire (make-wire)))
            (ripple-carry-adder (cdr A)
                                (cdr B)
                                (cdr S)
                                carry-wire)
            (full-adder (car A)
                        (car B)
                        carry-wire
                        (car S)
                        C)))
      (error "Inputs and outputs must be of the same length" A B S)))
{% endhighlight %}