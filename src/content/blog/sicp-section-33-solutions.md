---
title: "SICP Section 3.3 solutions"
description: "SICP exercises 3.12 - 3.37 - Section 3.3 solutions"
pubDate: 2017-03-18
tags: ["sicp", "computer-science", "scheme", "programming", "functional-programming"]
---

### Exercise 3.12

In this exercise, we are tasked with investigating a mutable version of the `append` function seen before called `append!` as defined below:-

```scheme
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
```

The problem is set up by calling the following:-

```scheme
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z
; (a b c d)
```

Appending by definition makes a copy of the first argument and appends the existing second argument to the end of this copied list. Thus, the box and pointer diagram at this point is as follows:-

<center>
<img src="/images/Ex3_12_Step1.svg" alt="After defining z" width="500"/>
</center>

Next, we are told to call `(cdr x)`. As can be seen from the box and pointer diagram, we expect to see `(b)` since the original list is not modified. This can be verified by the following code:-

```scheme
(cdr x)
; (b)
```

Next, we set things up using the new code:-

```scheme
(define w (append! x y))

w
; (a b c d)
```

This appends in-place and thus modifies its arguments. The box and pointer diagram changes as follows:-

<center>
<img src="/images/Ex3_12_Step2.svg" alt="After defining w" width="500"/>
</center>

This modifies the value of `x` as well. Thus, calling `(cdr x)` at this point should yield `(b c d)`. Let us verify this:- 

```scheme
(cdr x)
; (b c d)
```


### Exercise 3.13

In this exercise, we are tasked with examining the following procedure `make-cycle` and the results of executing the following code:-

```scheme
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b' 'c)))
```

It results in the following box and pointer diagram as follows:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="After making cycle"/>
</center>

As can be seen, the list starting at *z* does not have a `nil` pointer anywhere. Thus, if we call `(last-pair z)`, we expect the execution to go on forever because of the cycle we have created. It is verified by calling it in the scheme REPL. The code keeps running.


### Exercise 3.14

In this exercise, we are tasked with understanding the following `mystery` function:-

```scheme
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
```

This function takes a list and evaluates to its reverse. The box and pointer diagram for *v* where `(define v (list 'a 'b 'c 'd))` is as follows:-

<center>
<img src="/images/Ex3_14_Initial.svg" alt="After defining v"/>
</center>

Now, when we call `(define w (mystery v))`, the following evaluation happens:-

```scheme
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
```

The final state of the pointers look like the following:-

<center>
<img src="/images/Ex3_14_Final.svg" alt="After defining w"/>
</center>

Due to the mutating nature of this function, the original value of *v* also changes. Let us verify that:-

```scheme
(define v (list 'a 'b 'c 'd))
; v

(define w (mystery v))
; w

v
; (a)

w
; (d c b a)
```

### Exercise 3.15

In this exercise, we are tasked with explaining the difference in behavior when a function `set-to-wow!` is applied to two seemingly similar lists:-

```scheme
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
```

Even though *z1* and *z2* looked the same initially, calling `set-to-wow!` on them produces different results. They can be explained by the following box and pointer diagrams resulting from the application:-

<center>
<img src="/images/Ex3_15.svg" alt="After application of set-to-wow!"/>
</center>

As can be seen, both `car` and `cdr` of *z1* point to the same object. Thus, mutating one changes the other. As for *z2*, we had two exact same copies pointing to same elements down the road. Calling `set-to-wow!` on this only modified one copy, leaving the other alone.

### Exercise 3.16

In this exercise, we are told to examine the following `count-pairs` function written by Ben Bitdiddle:-

```scheme
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
```
	 
On the outset, we can see that this function will not work correctly because it does not consider pairs that might be pointed to from more than one location. They will be counted more than once. We have seen such cases in the previous exercises. Let us construct cases each with three pairs which give the required answers.

#### 3 pairs

This list can be constructed simply by a linear list which yields the following structure:-

<center>
<img src="/images/Ex3_16_3Pairs.svg" alt="Evaluate to 3 pairs"/>
</center>

```scheme
(define w '(a b c))

(count-pairs w)
; 3
```

As can be seen, in this simple case, the algorithm gives the right answer.

#### 4 pairs
	
This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_4Pairs.svg" alt="Evaluate to 4 pairs"/>
</center>

When evaluated, *x1* is counted twice and yields 4.

```scheme
(define x1 (cons 'a 'b))
(define x2 (cons x1 'c))
(define x (cons x1 x2))

(count-pairs x)
; 4
```

#### 7 pairs

This list can be constructed as follows:-

<center>
<img src="/images/Ex3_16_7Pairs.svg" alt="Evaluate to 7 pairs"/>
</center>

In this case, *y2* is counted twice and *y1* is counted 4 times to yield 7.

```scheme
(define y1 (cons 'a 'b))
(define y2 (cons y1 y1))
(define y (cons y2 y2))

(count-pairs y)
; 7 
```

#### Unlimited pairs

This list can be constructed by introducing a cycle:-

<center>
<img src="/images/Ex3_13_Pointer.svg" alt="Evaluate to unlimited pairs"/>
</center>

The code would keep evaluating along the cycle and never return at all.

```scheme
(define z (make-cycle '(a b c)))

(count-pairs z)
; Aborting!: maximum recursion depth exceeded
```

### Exercise 3.17
	
In this exercise, we are required to create a correct version of `create-pairs` which returns the correct number of distinct pairs. One good way to do it is to maintain a list of pairs already encountered and only count pairs not seen before. One way is as follows:-

```scheme
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
```

Let us test it on the lists defined in the previous exercise:-

```scheme
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
```

### Exercise 3.18

In this exercise, we are tasked with implementing a procedure that determines if a list contains a cycle. It is given that a list with cycle is defined as one that would result in an infinite loop if successive `cdr` is invoked on it. We can use the code from the previous exercise which tracks the pairs seen already to perform this check:-

```scheme
(define (has-cycle? x)
  (define encountered '())
  (define (check-if-seen x)
    (cond ((not (pair? x)) #f)
	      ((memq x encountered) #t)
	      (else (begin (set! encountered (cons x encountered))
		               (check-if-seen (cdr x))))))
  (check-if-seen x))
```

Let us go ahead and test this code on the cases from before. Only *z* should result in true:-

```scheme
(has-cycle? w)
; #f

(has-cycle? x)
; #f

(has-cycle? y)
; #f

(has-cycle? z)
; #t
```

### Exercise 3.19

In this exercise, we are tasked with re-implementing the `has-cycle?` code from the previous exercise so that only constant space is required. It can be implemented using [Floyd's algorithm](https://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare). 

```scheme
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
```

With this, let us test the cases from before:-

```scheme
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
```


### Exercise 3.20

In this exercise, we are tasked with giving the environment diagrams when the following code is executed:-

```scheme
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
```

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

### Exercise 3.21

In this exercise, we are tasked with explaining why the output of the queue is not wrong like what Ben Bitdiddle thinks. He evaluates the following code:-

```scheme
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
```

He claims that even after de-queuing all the inserted items in *q1*, we still the last element *b* being printed out. This is because we do not shift the `rear-ptr` when we dequeue the queue. Thus, when we simply display the queue object, we print both the pointers. The correct way to interpret the state of the queue is to print out the `front-ptr` alone. Thus, we define a `print-queue` function as follows:-

```scheme
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
```

As can be see, we print out the representation with the new function.


### Exercise 3.22

In this exercise, we are told to implement a queue with local state. It can be done so with the following code:-

```scheme
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
```

Let us test this with the code from previous exercise:-

```scheme
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
```

### Exercise 3.23

In this exercise, we are tasked with implementing a double ended queue called `deque`. This data structure is similar to the queue data structure from before, except we are able to add elements to the back of the queue and remove elements from the front of the queue. To do this, we need to alter the queue nodes to refer to both the element after and before itself.

```scheme
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
```

Let us test this code:-

```scheme
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
```

### Exercise 3.24

In this exercise, we are required to re-write the given `make-table` function to take another function as an argument which can be used as a custom equality checker that is different from the `equal?` function used by the given `assoc` function. The new constructor can be simply written by redefining the `assoc` function to use the argument instead.

```scheme
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
```

### Exercise 3.25

In this exercise, we are tasked with implementing a table that can store values under an arbitrary number of keys as opposed to the 1- and 2-dimensional tables described in this section. This can be done using a recursive solution that goes through the keys and traverses the table data structure. Further a record node at any depth can contain a value or another table. 

```scheme
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
```

This can be tested using the following code:-

```scheme
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
```

### Exercise 3.26

In this exercise, we are tasked with implementing the search table using a binary tree so that access would take a shorter time than a list. Since a binary tree requires a concept of ordering between keys, we have to make sure all keys are of the same type.

```scheme
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
```

We can also test the code using a table with string key:-

```scheme
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
```

Since there is no comparison with values, they can be of any arbitrary type.

### Exercise 3.27

In this exercise, we are required to draw en environment diagram for a memoized function defined using the following code:-

```scheme
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
```

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

### Exercise 3.28

In this section, we are primarily interested in simulating digital circuits. We are given examples of `not-gate` and `and-gate`. We are required to define an `or-gate`. It can be defined as follows:-

```scheme
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
```

### Exercise 3.29

In this exercise, we are asked to construct an or-gate using and-gates and inverters. For that purpose, we can use the [De Morgan's laws](https://www.wikiwand.com/en/De_Morgan's_laws#/Substitution_form). 

$$(P \vee Q) \equiv \neg(\neg P \wedge \neg Q)$$

Putting it together, we get the following:-

```scheme
(define (or-gate a1 a2 output)
  (let ((a1not (make-wire))
        (a2not (make-wire))
        (outnot (make-wire)))
    (inverter a1 a1not)
    (inverter a2 a2not)
    (and-gate a1not a2not outnot)
    (inverter outnot output)
    'ok))
```

In this process, we run the first two inverters simultaneously followed by an *and* operation and an inverter. Thus, the total delay would be:-

$$\text{or-gate-delay} = 2*\text{inverter-delay} + \text{and-gate-delay}$$.

### Exercise 3.30

In this exercise, we are required to implement a  *ripple-carry adder* which can be constructed by connection *n* full-adders serially. This setup takes two n-bit numbers $$A$$ and $$B$$, and outputs $$S=A+B$$ and $$C$$ where C is the carry bit from the addition process. This problem can be solved easily using a recursive solution, where an adder of length *n* can be constructed by taking an adder of length *n-1* and attaching a new full-adder's carry output to the carry input of the ripple-carry adder. A solution is as follows:-

```scheme
(define (ripple-carry-adder A B S C)
  (if (= (length A)
         (length B)
         (length S))
      (if (null? A)
          (begin (set-signal! C 0)
                 'ok)
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
```

To calculate the delay of the ripple-carry adder, let us start with half-adders.

$$\begin{align}
D_i &= \text{inverter-delay} \\
D_a &= \text{and-gate-delay} \\
D_o &= \text{or-gate-delay} \\

\text{Half adder sum delay}, D_{hs} &= D_a + \max(D_o, D_a + D_i) \\
\text{Half adder carry delay}, D_{hc} &= D_a \\

\text{Full adder sum delay}, D_{fs} &= D_{hs} \\
\text{Full adder carry delay}, D_{fc} &= D_o + \max(D_{hs}+D_{hc}, D_{hc}) \\
&= D_o + D_{hs} + D_{hc} \\

\text{Ripple adder sum delay for 1-bit}, D_{rs_1} &= D_{fs} \\
\text{Ripple adder carry delay for 1-bit}, D_{rc_1} &= D_{fc} \\

\text{Ripple adder sum elay for n-bit}, D_{rs_n} &= D_{rc_{n-1}} + D_{fs} \\
\text{Ripple adder carry delay for n-bit}, D_{rc_n} &= D_{rc_{n-1}} + D_{fc} \\

\text{Ripple adder overall delay}, D_{r_n} &= D_{rc_{n-1}} + \max(D_{fs}, D_{fc}) \\
&= D_{rc_{n-1}} + \max(D_{hs}, D_o + D_{hs} + D_{hc}) \\
&= D_{rc_{n-1}} + D_{fc} \\
&= n*D_{fc} \\
&= n*(D_o + D_{hs} + D_{hc}) \\
&= n*(D_o + 2*D_a + \max(D_o, D_a + D_i))
\end{align}$$

### Exercise 3.31

By this exercise, we are introduced to the code that is used to create wires and add actions to the wires. The following code is used to add an action to a wire:-

```scheme
(define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures))
      (proc))
```

In the given code, the function gets evaluated once after added to the list of `action-procedures`. This initialization is necessary because we require the propogation process to be kick-started with initial values. In the given code, a wire's `signal-value` is set to 0 upon initialization. Thus when an action is added and not run once initially, the action will only be triggered when the `signal-value` is set to 1. Until then, the system would be in an inconsistent state until all wires cycle through a 0 to 1 and back to 0.

To demonstrate this we are tasked with giving the reponse to the half-adder example given in the book if the `(proc)` statement is not called. The following response was seen:-

```scheme
(probe 'sum sum)

(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
; ok

(set-signal! input-1 1)
; done

(propagate)
; done

(set-signal! input-2 1)
; done

(propagate)
; carry 11  New-value = 1
; done
```

This is mainly due to the inverter in the half-adder. If the signal is not propagated, the default state would be having input and output of 0 which is inconsistent. This causes most of the problems seen. However, after cycling through all possible inputs, the behaviour returns back to normal.

### Exercise 3.32

In this exercise, we are asked to justiy executing the actions registered for a segment in a FIFO order. For example, let us take a take a look at an and-gate with *0* and *1* as inputs and propogated. When we set inputs to *1* and *0* respectively, two events are added to the agenda in the following order:-

1) Inputs are 1 and 1, output of 1
2) Inputs are 0 and 1, output of 0

Two events are generated because the change needs to be performed in two steps.

If the events are executed in the FIFO order, the output temporarily gets set to an intermediate value of 1 and then settles with the correct value of 0. This phenomenon is called a [hazard](https://www.wikiwand.com/en/Hazard_(logic)).

Now, if the events are executed in the LIFO order, we output gets set to 0 first and then to a wrong value of 1. This is because processing the events in the reverse order leads to having an incomplete intermediate state as the final state. Thus, the events for a segment should be executed in FIFO order to get the consistent final output.

### Exercise 3.33

In this exercise, we are tasked with creating a network called `averager` that takes three arguments `a`, `b` and `c` and maintains a constraint where `c` is the average of `b` and `a`. This can be accomplised as follows:-

```scheme
(define (averager a b c)
  (let ((total (make-connector))
        (divisor (make-connector)))
    (adder a b total)
    (multiplier divisor c total)
    (constant 2 divisor)
    'ok))
```

Let us test the network.

```scheme
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)
(probe "A" a)
(probe "B" b)
(probe "C" c)

(set-value! a 42 'user)
; Probe: A = 42
; done

(set-value! b 32 'user)
; Probe: B = 32
; Probe: C = 37
; done

(set-value! c 39 'user)
; Error! Contradiction (37 39)

(forget-value! b 'user)
; Probe: B = ?
; Probe: C = ?
; done

(set-value! c 39 'user)
; Probe: C = 39
; Probe: B = 36
```

As can be seen, correct results are obtained.

### Exercise 3.34

In this exercise, we are told that Louis Reasoner builds a squarer using a multiplier using the following code:-

```scheme
(define (squarer a b) (multiplier a a b))
```

The mistake with this way of doing things is primarily because `multiplier` requires two inputs to compute the other value. Thus, when we set the value of `a`, we can obtain the value of `b` because `a` counts as two inputs to the `multiplier` in question. The reverse would not work because `b` is only one input out of the two required.

The right way to go about it would be to define a new primitive constraint since we can't exactly calculate a square root using a predefined number of adders and multipliers. It takes an iterative method to obtain a square root.

### Exercise 3.35

In this exercise, we are tasked with implementing a proper `squarer` component that maintains $$a^2=b$$. The following code does that:-

```scheme
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (* (get-value a)
                           (get-value a))
                    me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request:
                   SQUARER"
                  request))))
  (connect a me)
  (connect b me)
  me)
```

Let us test the code:-

```scheme
(define a (make-connector))
(define b (make-connector))
(squarer a b)
(probe "A" a)
(probe "B" b)

(set-value! b 5 'user)
; Probe: B = 5
; Probe: A = 2.23606797749979
; done

(forget-value! b 'user)
; Probe: B = ?
; Probe: A = ?
; done

(set-value! a 5 'user)
; Probe: A = 5
; Probe: B = 25
; done
```

The code works as expected.

### Exercise 3.36

In this exercise, we are asked to draw the environment diagram when the the `for-each-except` is called when the following code is evaluated:-

```scheme
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)
```

<center>
<img src="/images/Ex3_36.svg" alt="for-each-except environment"/>
</center>

The above diagram shows the environment. `for-each-except` is executed in environment **E3**.

### Exercise 3.37

In this exercise, we are required to create a shorthand version of constraints that can create a network as follows:-

```scheme
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
```

Such constraint functions are as follows:-

```scheme
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))
```
