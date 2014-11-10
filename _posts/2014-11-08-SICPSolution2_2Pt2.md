---
layout: post
category: [SICP, Solutions]
post_no: 16
title: "SICP Section 2.2 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_24", title: "Exercise 2.24" }
   - { hook: "Exercise2_25", title: "Exercise 2.25" }
   - { hook: "Exercise2_26", title: "Exercise 2.26" }
   - { hook: "Exercise2_27", title: "Exercise 2.27" }
   - { hook: "Exercise2_28", title: "Exercise 2.28" }
   - { hook: "Exercise2_29", title: "Exercise 2.29" }
---

### Exercise 2.24<a name="Exercise2_24">&nbsp;</a>

In this exercise, we are asked to evaluate the expression `(list 1 (list 2 (list 3 4)))`. Then, we are asked to provide the box-and-pointer structure and the tree interpretation of the result. Evaluating the expression, we get the following:-

{% highlight scheme %}
(list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))
{% endhighlight %}
<!--excerpt-->

The box-and-pointer structure is as follows:-

<center><img src="/images/Ex2_24_BP.svg" alt="Box-and-pointer structure"/></center>

Likewise, the tree structure is as follows:-

<center><img src="/images/Ex2_24_Tree.svg" alt="Tree structure"/></center>

### Exercise 2.25<a name="Exercise2_25">&nbsp;</a>

In this exercise, we are tasked with giving the combination of `car`s and `cdr`s required to pick `7` from the lists.

##### `(1 3 (5 7) 9)`

{% highlight scheme %}
(define a (list 1 3 (list 5 7) 9))
; a
a
; (1 3 (5 7) 9)

(cdr a)
; (3 (5 7) 9)
(cdr (cdr a))
; ((5 7) 9)
(car (cdr (cdr a)))
; (5 7)
(cdr (car (cdr (cdr a))))
; (7)

(car (cdr (car (cdr (cdr a)))))
; 7
{% endhighlight %}

##### `((7))`

{% highlight scheme %}
(define b (list (list 7)))
; b
b
; ((7))

(car b)
; (7)

(car (car b))
; 7
{% endhighlight %}

##### `(1 (2 (3 (4 (5 (6 7))))))`

{% highlight scheme %}
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; c
c
; (1 (2 (3 (4 (5 (6 7))))))

(cdr c)
; ((2 (3 (4 (5 (6 7))))))
(car (cdr c))
; (2 (3 (4 (5 (6 7)))))
(cdr (car (cdr c)))
; ((3 (4 (5 (6 7)))))
(car (cdr (car (cdr c))))
; (3 (4 (5 (6 7))))
(cdr (car (cdr (car (cdr c)))))
; ((4 (5 (6 7))))
(car (cdr (car (cdr (car (cdr c))))))
; (4 (5 (6 7)))
(cdr (car (cdr (car (cdr (car (cdr c)))))))
; ((5 (6 7)))
(car (cdr (car (cdr (car (cdr (car (cdr c))))))))
; (5 (6 7))
(cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))
; ((6 7))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))
; (6 7)
(cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))
; (7)

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
; 7
{% endhighlight %}

Note that we can write successive `car`s and `cdr`s upto four times using a short-hand notation. Eg. `(cadadr x)` for `(car (cdr (car (cdr x))))` and so on.

### Exercise 2.25<a name="Exercise2_25">&nbsp;</a>

This exercise simply tasks us with finding out the result printed when evaluated by the interpreter.

{% highlight scheme %}
(define x (list 1 2 3))
; x
(define y (list 1 2 3))
; y

x
; (1 2 3)
y
; (1 2 3)

(append x y)
; (1 2 3 1 2 3)
(cons x y)
; ((1 2 3) 1 2 3)
(list x y)
; ((1 2 3) (1 2 3))
{% endhighlight %}

### Exercise 2.27<a name="Exercise2_27">&nbsp;</a>

In this exercise, we are tasked with modifying the `reverse` procedure from before to produce `deep-reverse` which reverses it elements and reverses all sublists as well.

Let us look at the original `reverse` procedure.

{% highlight scheme %}
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items))
              (list (car items)))))
{% endhighlight %}

Similarly, `deep-reverse` can be defined as follows:-

{% highlight scheme %}
(define (deep-reverse items)
  (if (or (null? items)
          (not (pair? items)))
	  items
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))))
{% endhighlight %}

Let us test it.

{% highlight scheme %}
(define x (list (list 1 2) (list 3 4)))
; x

x
; ((1 2) (3 4))

(reverse x)
; ((3 4) (1 2))

(deep-reverse x)
; ((4 3) (2 1))
{% endhighlight %}

Works as predicted. Let us try with a more complex expression.

{% highlight scheme %}
(define y (list (list 1 2) (list 3 4) (list (list 5 6) (list 7 8))))
; y

y
; ((1 2) (3 4) ((5 6) (7 8)))

(deep-reverse y)
; (((8 7) (6 5)) (4 3) (2 1))
{% endhighlight %}

### Exercise 2.28<a name="Exercise2_28">&nbsp;</a>

In this exercise, we are tasked with writing a procedure `fringe` which takes a tree and returns the leaves arranged left-to-right. It can be accomplished using the following code:-

{% highlight scheme %}
(define (fringe items)
  (cond ((null? items) items)
        ((pair? items) (append (fringe (car items))
                               (fringe (cdr items))))
        (else (list items))))
{% endhighlight %}

Let us test the code using the same test cases as the previous exercise.

{% highlight scheme %}
x
; ((1 2) (3 4))
(fringe x)
; (1 2 3 4)
(fringe (list x x))
; (1 2 3 4 1 2 3 4)

y
; ((1 2) (3 4) ((5 6) (7 8)))
(fringe y)
; (1 2 3 4 5 6 7 8)
(fringe (list y y))
; (1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
{% endhighlight %}

### Exercise 2.28<a name="Exercise2_28">&nbsp;</a>

In this exercise, we are tasked with implementing binary mobile data structures.

A mobile is created by taking two branches and combining them.

{% highlight scheme %}
(define (make-mobile left right)
  (list left right))
{% endhighlight %}

Further, a `branch` is created by compounding a `length` for the structure and the structure itself, which could be simple number or a mobile.

{% highlight scheme %}
(define (make-branch length structure)
  (list length structure))
{% endhighlight %}

##### Branch selectors

Given a mobile, the branches can be selected using the following code:-

{% highlight scheme %}
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))
{% endhighlight %}

With the branch selected, its length and structure can be obtained as follows:-

{% highlight scheme %}
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))
{% endhighlight %}

##### Weight calculation

The weight of a given mobile can be computed as follows:-

{% highlight scheme %}
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((b (branch-structure branch)))
    (if (pair? b)
        (total-weight b)
		b)))
{% endhighlight %}

##### Balance verification

We can verify if a mobile is balanced using the following code:-

{% highlight scheme %}
(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
	   (balanced-branch? (left-branch mobile))
	   (balanced-branch? (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

(define (balanced-branch? branch)
  (let ((b (branch-structure branch)))
    (if (pair? b)
        (balanced? b)
		#t)))
{% endhighlight %}

##### Change of representation

When the representation of mobiles are changed to `cons` instead of `list` as follows,

{% highlight scheme %}
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))
{% endhighlight %}

Only the accessors need to be changed as follows,

{% highlight scheme %}
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))
  
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))
{% endhighlight %}
