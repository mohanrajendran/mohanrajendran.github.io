---
layout: post
category: [SICP, Solutions]
post_no: 44
title: "SICP Section 3.3 Exercise Solution Part 4"
submenu:
- { hook: "Exercise 3_24", title: "Exercise 3.24" }
- { hook: "Exercise 3_25", title: "Exercise 3.25" }
---

### Exercise 3.24<a name="Exercise3_24">&nbsp;</a>

In this exercise, we are required to re-write the given `make-table` function to take another function as an argument which can be used as a custom equality checker that is different from the `equal?` function used by the given `assoc` function. The new constructor can be simply written by redefining the `assoc` function to use the argument instead.

<!--excerpt-->

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

### Exercise 3.25<a name="Exercise3_25">&nbsp;</a>

In this exercise, we are tasked with implementing a table that can store values under an arbitrary number of keys as opposed to the 1- and 2-dimensional tables described in this section. This can be done using a recursive solution that goes through the keys and traverses the table data structure. Further a record node at any depth can contain a value or 

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