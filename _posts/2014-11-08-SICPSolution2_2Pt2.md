---
layout: post
category: [SICP, Solutions]
post_no: 16
title: "SICP Section 2.2 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_24", title: "Exercise 2.24" }
   - { hook: "Exercise2_25", title: "Exercise 2.25" }
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

Note that we can write successive `car`s and `cdr`s upto four times using a short-hand notation `(cadadr x)` for `(car (cdr (car (cdr x))))` and so on.
