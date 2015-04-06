---
layout: post
category: [SICP, Solutions]
post_no: 37
title: "SICP Section 3.1 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise3_1", title: "Exercise 3.1" }
   - { hook: "Exercise3_2", title: "Exercise 3.2" }
   - { hook: "Exercise3_3", title: "Exercise 3.3" }
   - { hook: "Exercise3_4", title: "Exercise 3.4" }
   - { hook: "Exercise3_5", title: "Exercise 3.5" }
   - { hook: "Exercise3_6", title: "Exercise 3.6" }
   - { hook: "Exercise3_7", title: "Exercise 3.7" }
   - { hook: "Exercise3_8", title: "Exercise 3.8" }
---
### Exercise 3.1<a name="Exercise3_1">&nbsp;</a>

In this exercise, we are tasked with writing a procedure `make-accumulator` which can be used to generate accumulaors which can maintain their independant sum. It can be done using the following code by maintaining a local variable:-

{% highlight scheme %}
(define (make-accumulator acc)
  (lambda (n)
	(begin (set! acc (+ acc n))
		   acc)))
; make-accumulator

(define A (make-accumulator 5))
; a
(A 10)
; 15
(A 10)
; 25
{% endhighlight %}

Thus, an accumulator object is created.
<!--excerpt-->

### Exercise 3.2<a name="Exercise3_2">&nbsp;</a>

In this exercise, we are asked to write a procedure `make-monitored` which takes in a procedure `f` which itself takes in one input and returns a modified procedure `mf`. It can be used to keep track of the number of times the function was called. The following code allows us to achieve that by updating a local variable each time a function is called:-

{% highlight scheme %}
(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
	      calls
	      (begin (set! calls (+ calls 1))
	             (f arg))))))
; make-monitored

(define s (make-monitored sqrt))
; s
(s 100)
; 10
(s 'how-many-calls?)
; 1
(s 1000)
; 31.622776601683793
(s 'how-many-calls?)
; 2
{% endhighlight %}

### Exercise 3.3<a name="Exercise3_3">&nbsp;</a>

In this exercise, we need to modify the `make-account` procedure given in the book to take passowords.

{% highlight scheme %}
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
	           balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request: MAKE-ACCOUNT" m))))
  (define (check-and-dispatch pwd op)
    (if (eq? pwd password)
        (dispatch op)
        (lambda (x) "Incorrect password")))
check-and-dispatch)
; make-account

(define acc (make-account 100 'secret-password))
; acc
((acc 'secret-password 'withdraw) 40)
; 60
((acc 'some-other-password 'deposit) 50)
; "Incorrect password"
{% endhighlight %}

Of course, passwords cannot be stored plainly as shown are usually stored hashed.

### Exercise 3.4<a name="Exercise3_4">&nbsp;</a>

In this exercise, we are told to extend the password protected transactions above to call the cops if the number of wrong tries consecutively exceeds 7. The following code does that:-

{% highlight scheme %}
(define (make-account balance password)
  (define call-the-cops "CALLING THE COPS")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
        	   balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request: MAKE-ACCOUNT" m))))
  (let ((calls 0))
    (define (check-and-dispatch pwd op)
      (cond ((= calls 7) (lambda (x) call-the-cops))
	        ((eq? pwd password)
	          (begin (set! calls 0)
		             (dispatch op)))
	        (else (begin (set! calls (+ calls 1))
		                 (lambda (x) "Incorrect password")))))
  check-and-dispatch))
; make-account

(define acc (make-account 10 'pwd))
; acc
((acc 'wpwd 'deposit) 10)
; "Incorrect password"
((acc 'wpwd 'deposit) 10)
; "Incorrect password"

...

((acc 'wpwd 'deposit) 10)
; "CALLING THE COPS"
((acc 'pwd 'deposit) 10)
; "CALLING THE COPS"
{% endhighlight %}

Each correct password resets the counter and each wrong password increments the `calls` counter. When it reaches 7, we call the cops even if right password is input.

### Exercise 3.5<a name="Exercise3_5">&nbsp;</a>

In this exercise, we are told to use the `monte-carlo` function given in the book to perform *Monte Carlo integration*. The function is as follows:-

{% highlight scheme %}
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))
{% endhighlight %}

We can reuse this function to perform the monte carlo integration using the following code:-

{% highlight scheme %}
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
; random-in-range

(define (integral-test P x1 x2 y1 y2)
  (let ((x (random-in-range x1 x2))
        (y (random-in-range y1 y2)))
    (P x y)))
; integral-test

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((x (- x2 x1))
        (y (- y2 y1)))
    (* (monte-carlo
	     trials
	     (lambda () (integral-test P x1 x2 y1 y2)))
       x
	   y)))
; estimate-integral
{% endhighlight %}

We can test this code by finding the area of a unit circle.

{% highlight scheme %}
(define (unit-circle x y)
  (<= (+ (square x) (square y)) 1.0))
; unit-circle

(estimate-integral unit-circle -1.0 1.0 -1.0 1.0 10000)
; 3.1268
(estimate-integral unit-circle -1.0 1.0 -1.0 1.0 1000000)
; 3.14328

(estimate-integral unit-circle -2.0 2.0 -2.0 2.0 10000)
; 3.1568
{% endhighlight %}

As can be seen we get a good approximation for $\pi$ which is the area of a unit circle. We can use any size of rectangle to compute this value.

### Exercise 3.6<a name="Exercise3_6">&nbsp;</a>

In this exercise we are tasked with writing a modified random-number generator that can have its seed set so that we can perform repeated experiments. It can be achieved using the following code:-

{% highlight scheme %}
(define default-seed 42)
; default-seed

(define rand
  (let ((seed default-seed))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	         (begin (set! seed (rand-update seed))
		            seed))
	        ((eq? m 'reset)
	         (lambda (new-value) (set! seed new-value)))
	        (else (error "Unknown request: RAND" m))))
    dispatch))
; rand
{% endhighlight %}

It uses a default seed value of 42 if no seed is set initially. As for the `rand-update` function, we use the [Linear Congruential Generator](http://en.wikipedia.org/wiki/Linear_congruential_generator).

{% highlight scheme %}
(define (rand-update x)
  (let ((m (expt 2 31))
        (a 1103515245)
        (c 12345))
    (modulo (+ (* a x) c) m)))
; rand-update
{% endhighlight %}

The following tests can be run to verify the code:-

{% highlight scheme %}
(rand-update 42)
; 1250496027
(rand 'generate)
; 1250496027

(rand 'generate)
; 1116302264
(rand 'generate)
; 1000676753

((rand 'reset) 1250496027)
; 
(rand 'generate)
; 1116302264
(rand 'generate)
; 1000676753
{% endhighlight %}

As can be seen, we can reset the stream of random numbers anytime to our chosen value.

### Exercise 3.7<a name="Exercise3_7">&nbsp;</a>

In this exercise, we are required to create a procedure `make-account` which can be used to create joint accounts. The joint account should be operated from the original account using the original password as well. We do that with the following code:-

{% highlight scheme %}
(define (make-joint orig-account orig-password joint-password)
  (define (check-and-dispatch pwd op)
     (if (eq? pwd joint-password)
         (orig-account orig-password op)
         (lambda (x) "Incorrect password")))
  check-and-dispatch)
; make-joint

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; paul-acc
((peter-acc 'open-sesame 'withdraw) 100)
; 900
((paul-acc 'open-sesame 'withdraw) 100)
; "Incorrect password"
((paul-acc 'rosebud 'withdraw) 100)
; 800
{% endhighlight %}

As seen, both accounts share the same internal balance amount. Now, we can add more helpful error messages to users based on which password is wrong.

### Exercise 3.8<a name="Exercise3_8">&nbsp;</a>

In this exercise, we are told to create a function `f` which, when substituted into `(+ (f 0) (f 1))` should evaluate to 0 when the subexpressions are evaluated from left to right and 1 otherwise. One such function is as follows:-

{% highlight scheme %}
(define f
  (let ((set #f))
    (define (fn n)
      (if set
	      0
 	      (begin (set! set #t)
	             n)))
  fn))
{% endhighlight %}

{% highlight scheme %}
(f 0)
; 0
(f 1)
; 0

; After redefining f

(f 1)
; 1
(f 0)
; 0
{% endhighlight %}

As can be seen, when `(f 0)` is evaluated first followed by `(f 1)` we get *0* and *0* which sum to *0*. If `(f 1)` is evaluated first followed by `(f 0)`, we get *1* and *0* which sum to *1*. This fulfils the condition set forth.

We can check the actual order in *mit-scheme* and we get the folowing:-

{% highlight scheme %}
(+ (f 0) (f 1))
; 1
{% endhighlight %}

Thus, it can be seen that the arguments to `+` are evaluated from right to left.
