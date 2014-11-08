---
layout: post
category: [SICP, Solutions]
post_no: 15
title: "SICP Section 2.2 Exercise Solutions - Part 1"
submenu:
   - { hook: "Exercise2_17", title: "Exercise 2.17" }
   - { hook: "Exercise2_18", title: "Exercise 2.18" }
   - { hook: "Exercise2_19", title: "Exercise 2.19" }
   - { hook: "Exercise2_20", title: "Exercise 2.20" }
---

### Exercise 2.17<a name="Exercise2_17">&nbsp;</a>

In this exercise, we are tasked with creating a procedure `last-pair` and returns a list containing the last element of the list. It canbe done as follows:-

{% highlight scheme %}
(define (last-pair items)
  (if (null? (cdr items))
      items
	  (last-pair (cdr items))))
; last-pair

(last-pair (list 23 72 149 34))
; (34)
(last-pair (list 23 72 149 (list 34 45)))
; ((34 45))
{% endhighlight %}

<!--excerpt-->

### Exercise 2.18<a name="Exercise2_18">&nbsp;</a>

In this exercise, we are tasked with writing a procedure `reverse` which can be used to convert a list to another list with the same elements in reverse order. It can be done with the assistance of the `append` function given in the book.

{% highlight scheme %}
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))
; append

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items))
              (list (car items)))))
; reverse

(reverse (list 1 4 9 16 25))
; (25 16 9 4 1)
{% endhighlight %}

### Exercise 2.19<a name="Exercise2_19">&nbsp;</a>

In this exercise, we are tasked with rewriting the change-counting program in Section 1.2.2 so that they can be used on arbitrary currency by allowing the denominations to be specified as a list. We are given the folowing function `cc`:-

{% highlight scheme %}


(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))
{% endhighlight %}

We are tasked with writing the procedures `first-denomination`, `except-first-denomination` and `no-more?` to complete the procedure. Let us do that and test the function.

{% highlight scheme %}
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins 
  (list 50 25 10 5 1))
; us-coins
(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))
; uk-coins

(cc 100 us-coins)
; 292
(cc 100 uk-coins)
; 104561
{% endhighlight %}

We can see that our solution gives the same right answer as before.

Further, we are asked if the order of the list `coin-values` affect the answer produced by `cc` or not. The answer is that it should not affect the answer. This is because our current algorithm checks all coin combinations irrespective of their denomination order. Let us test it.

{% highlight scheme %}
(define us-coins-scr (list 10 25 1 50 5))
; us-coins-scr
(cc 100 us-coins-scr)
; 292
{% endhighlight %}

As we can see, the same answer is obtained.

### Exercise 2.20<a name="Exercise2_20">&nbsp;</a>

In this exercise, we are tasked with using the *dotted-tail notation* to define a procedure `same-parity` which can be used to return a list of all arguments that has the same even-odd parity as the first argument.

{% highlight scheme %}
(define (same-parity head . tail)
  (let ((rem (remainder head 2)))
    (define (filter-even-odd items)
      (if (null? items)
        items
		(if (= (remainder (car items) 2) rem)
            (cons (car items)
                  (filter-even-odd (cdr items)))
	        (filter-even-odd (cdr items)))))
    (cons head (filter-even-odd tail))))
; same-parity

(same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
(same-parity 2 3 4 5 6 7)
; (2 4 6)
{% endhighlight %}
