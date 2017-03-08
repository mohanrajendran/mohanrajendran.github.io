---
layout: spiedpage
order: 14
title: Section 2.4 solutions
exercises: '2.73 - 2.76'
submenu:
  - { hook: "Exercise2_73", title: "Exercise 2.73" }
  - { hook: "Exercise2_74", title: "Exercise 2.74" }
  - { hook: "Exercise2_75", title: "Exercise 2.75" }
  - { hook: "Exercise2_76", title: "Exercise 2.76" }
---

### Exercise 2.73<a id="Exercise2_73">&nbsp;</a>

In this exercise, we deal with the reimplementation of the `deriv` program that we had worked on before to use the the data-derected dispatch.

We are given the following code which does that:-

{% highlight scheme %}
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
{% endhighlight %}

##### Part-1
The first part of the question asks us to describe what is happening in the code above and why the predicates for `number?` and `variable?` cannot be combined into the data-direction dispatch described in this section.

In the code above, when given an expression, we first use the built-in function `number?` to check if the expression is a number and return 0. Then, when given a symbol, we check if it is the same as the variable with we are differentiating with respect to and returns 0 or 1. Finally, we user the `operator` function to obtain the operator of the expression which can be `'+`, or `'*` as we had seen before. We then use the `get` function to look up the exact `deriv` procedure define for the particular expression and apply them to the expression.

Now, we are also asked why the number and variable branches cannot be merged. The reason for that is because these expressions are the primitive parts which do not have a type tag. So there is no dispatching based on the operator type. They are therefore they are dispatched based on their data type.

##### Part-2
In the second part of the exercise, we are told to write the procedures for derivatives of sums and products along with the code to install them in the table to be used by the given code. For that purpose, let us implement a the dispatch table using the built-in `hashtable` which supports the `put` and `get` operations.

{% highlight scheme %}
(define (equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
		((and (pair? p1) (pair? p2))
		  (and (equal? (car p1) (car p2))
			   (equal? (cdr p1) (cdr p2))))
	    ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))
; equal?

(define dispatch-table (make-hash-table equal?))
; dispatch-table

(define (put op type item)
  (hash-table/put! dispatch-table (list op type) item))
; put
(define (get op type)
  (hash-table/get dispatch-table (list op type) #f))
; get

(put 'hello 'world 5)
; Unspecified return value
(get 'hello 'world)
; 5
{% endhighlight %}

With the dispatch table in hand, let us tackle the next problem at hand.

{% highlight scheme %}
(define (install-deriv-package)
  ;; internal function
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  ;; internal procedures for sums
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  ;; internal procedures for products
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (prod-deriv operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  
  ;; interface to the rest of the system
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* prod-deriv))
; install-deriv-package

(define variable? symbol?)
; variable?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
; same-variable?
{% endhighlight %}

Using the above code, we initialize everything the `deriv` program needs. We can install the package by calling `install-deriv-package`. Let us see test it.

{% highlight scheme %}
(install-deriv-package)
; Unspecified return value

(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
{% endhighlight %}

##### Part-3
In the third part of the exercise, we are told to create an additional package such as one for differentiating exponents and install it into the system. Let us write the code which does that.

{% highlight scheme %}
(define (install-deriv-package-exponentiation)
  ;; internal function
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))

  ;; internal procedures for exponents
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent)) (expt base exponent))
          ((number? exponent) (list '** base exponent))
          (else (error "cannot exponentiate by an expression"))))
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (exp-deriv operands var)
    (make-product
      (make-product
        (exponent operands)
        (make-exponentiation (base operands) (- (exponent operands) 1)))
      (deriv (base operands) var)))

  ;; interface to the rest of the system
  (put 'deriv '** exp-deriv))
; install-deriv-package-exponentiation

(install-deriv-package-exponentiation)
;Unspecified return value

(deriv '(** x 5) 'x)
; (* 5 (** x 4))
{% endhighlight %}

By simply installing the new functions into the dispatch table, we extend the functionality of the `deriv` system. Note that we have retyped some internal functions such as `=number?` and `make-product`. The constructors need to be implemented globally so that they can be reused. Further, any upgrade to these procedures can be done in one location and the changes can propogate consistently.

##### Part-4
In the final part of the exercise, we ae asked how we can change the order of indexing in `get` by using the following code instead.

{% highlight scheme %}
((get (operator exp) 'deriv) 
 (operands exp) var)
{% endhighlight %}

We can utilize the function as is by changing the order in which the index to the hash-table is constructed in the `get` function.

{% highlight scheme %}
(define (get type op)
  (hash-table/get dispatch-table (list op type) #f))
{% endhighlight %}

By changing the order in the function signature, this can be implemented efficiently. Thus, this layer of abstraction lets us make modifications.

### Exercise 2.74<a id="Exercise2_74">&nbsp;</a>

In this exercise, we are told of a system set up by *Insatiable Enterprises, Inc.,* which is a networked system with each component having its own way of storing records. We then look at ways to integrate them.

##### Part-1
The first part of the exercise asks us how to implement a `get-record` procedure that can retreive an employee's record from various disparate computer systems. First, we need a way of creating tagged records which specify their storage scheme.

{% highlight scheme %}
(define (create-generic-file division file)
  (cons division file))
(define (get-division generic-file)
  (car generic-file))
(define (get-file generic-file)
  (cdr generic-file))
{% endhighlight %}

Next, we need to retreive the specific operation that retreives the data and apply it to the file.

{% highlight scheme %}
(define (get-record generic-file employee)
  (let ((division (get-division generic-file)))
	(let ((func (get 'get-record division))
		  (file (get-file generic-file)))
	  (func file employee))))
{% endhighlight %}

With this, we can get record from any installed file system. Each division is in charge of creating the appropriate functions and files with correct tagged types.

##### Part-2
In this part of the question, we are tasked with implementing a `get-salary` procedure which returns the salary information from a given employee's record. Similar to how we created the tagged data and respective retreival methods in the previous part, we tackle this problem the same way.

{% highlight scheme %}
(define (create-employee-record division record)
  (cons division record))
(define (get-division employee-record)
  (car employee-record))
(define (get-record employee-record)
  (cdr employee-record))

(define (get-salary employee-record)
  (let ((division (get-division employee-record)))
	(let ((func (get 'get-salary division))
		  (record (get-record employee-record)))
	  (func record))))
{% endhighlight %}

Again, its the responsibility of each division to tag its record and install appropriate functions in the dispatch table to retreive the salient information. If it is to be combined with the employee record retreival from the previous part, we can skip tagging each individual record by reusing the `division` information.

##### Part-3
In the third part of the exercise, we are tasked with implementing a function called `find-employee-record` which takes an employee and a list of files, and returns an employee record. We can reuse the retreival functions defined in the first part. We add one more function which checks if the given employee is found in a file.

{% highlight scheme %}
(define (record-present? generic-file employee)
  (let ((division (get-division generic-file)))
	(let ((func (get 'record-present? division))
		  (file (get-file generic-file)))
	  (func file employee))))

(define (find-employee-record files employee)
  (cond ((null? files)
	      (error "Not found: FIND-EMPLOYEE-RECORD" employee))
	    ((record-present? (car files) employee)
		  (get-record (car files) employee))
	    (else (find-employee-record (cdr files) employee))))
{% endhighlight %}

##### Part-4
In the last part of the exercise, we are asked what needs to be done when *Insatiable* takes over a new company and needs to incorporate its records into its system.

The answer is quite simple. The new data needs to be tagged with their division names and their versions of `get-record`, `get-salary` and `record-present?` need to be installed in the dispatch table.

### Exercise 2.75<a id="Exercise2_75">&nbsp;</a>

In this exercise, we are told to implement the constructor `make-from-mag-ang` analogous to the given `make-from-real-imag` constructor using the message-passing style.

{% highlight scheme %}
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
		  ((eq? op 'angle) a)
		  ((eq? op 'real-part) (* r (cos a)))
		  ((eq? op 'imag-part) (* a (sin a)))
		  (else
		   (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)
{% endhighlight %}

### Exercise 2.76<a id="Exercise2_76">&nbsp;</a>

This exercise deal with ways to evolve a large system and grow it using three strategies - generic operations with explicit dispatch, data-directed style and message-passing style and asks us to compare them.

- **Generic operations with explicit dispatch** means that whenever new operations are added, we need to modify existing code to add a branching statement to utilize a new procedure whenever functions are called on a new data type.
- **Data-directed style** means that we do not need to modify existing code. We can simply add more entries to the dispatch table to look up the new procedures. This allows us to build modules easily.
- **Message-passing style** is similar in that we do not need to modify existing code. We simply need to encapsulate the new procedures into the new data type. Then, we need to only supply the object with the function to be called and it will dispatch accordingly.

Based on what we have learnt so far in this section, reducing modification to existing code base and only extension is a wiser decision because of its simplicity. Thus, both data-directed style and message-passing style are recommended for large systems. They are equivalent in power.
