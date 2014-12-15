---
layout: post
category: [SICP, Solutions]
post_no: 28
title: "SICP Section 2.4 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_74", title: "Exercise 2.74" }
   - { hook: "Exercise2_75", title: "Exercise 2.75" }
   - { hook: "Exercise2_76", title: "Exercise 2.76" }
---
### Exercise 2.74<a name="Exercise2_74">&nbsp;</a>

In this exercise, we are told of a system set up by *Insatiable Enterprises, Inc.,* which is a networked system with each component having its own way of storing records. We then look at ways to integrate them.

<!--excerpt-->

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

### Exercise 2.75<a name="Exercise2_75">&nbsp;</a>

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

### Exercise 2.76<a name="Exercise2_76">&nbsp;</a>

This exercise deal with ways to evolve a large system and grow it using three strategies - generic operations with explicit dispatch, data-directed style and message-passing style and asks us to compare them.

- **Generic operations with explicit dispatch** means that whenever new operations are added, we need to modify existing code to add a branching statement to utilize a new procedure whenever functions are called on a new data type.
- **Data-directed style** means that we do not need to modify existing code. We can simply add more entries to the dispatch table to look up the new procedures. This allows us to build modules easily.
- **Message-passing style** is similar in that we do not need to modify existing code. We simply need to encapsulate the new procedures into the new data type. Then, we need to only supply the object with the function to be called and it will dispatch accordingly.

Based on what we have learnt so far in this section, reducing modification to existing code base and only extension is a wiser decision because of its simplicity. Thus, both data-directed style and message-passing style are recommended for large systems. They are equivalent in power.
