---
layout: post
category: [SICP, Solutions]
post_no: 23
title: "SICP Section 2.3 Exercise Solutions - Part 2"
submenu:
   - { hook: "Exercise2_58", title: "Exercise 2.58" }
   - { hook: "Exercise2_59", title: "Exercise 2.59" }
   - { hook: "Exercise2_60", title: "Exercise 2.60" }
---
### Exercise 2.58<a name="Exercise2_58">&nbsp;</a>

In this exercise, we are required to modify the `deriv` program to work on a more natural form of expressing equations. 

<!--excerpt-->

##### Differentiation of infix expressions

In the first part of this exercise, we modify the differentiation process to work on expressions in infix form. So as not to fizzle our brains, we are told first to approach this in a form where each operator only has two operands. Thus, expressions are natually parenthesized in terms of precedence and list decompositions give the required sub expressions.

Further, this task can be accomplished by only modifying the lower level representaion of expressions. The higher level `deriv` function need not be touched. First, let us convert the representations of `sum` and `product`.

{% highlight scheme %}
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
; make-sum

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
; sum?

(define (addend s) (car s))
; addend

(define (augend s) (caddr s))
; augend

(define s (make-sum 'x 'y))
; s
s
; (x + y)
(sum? s)
; #t
(addend s)
; x
(augend s)
; y
{% endhighlight %}

{% highlight scheme %}
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list m1 '* m2))))
; make-product

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
; product?

(define (multiplier p) (car p))
; multiplier

(define (multiplicand p) (caddr p))
; multiplicand

(define p (make-product 'u 'v))
; p
p
; (u * v)
(product? p)
; #t
(multiplier p)
; u
(multiplicand p)
; v
{% endhighlight %}

Once that is done, let us test the code with some test cases:-

{% highlight scheme %}
(deriv '(x + (3 * (x + (y + 2)))) 'x)
; 4

(deriv '((x * y) + (y * x)) 'x)
; (y + y)
{% endhighlight %}

##### Differentiation of standard algebraic notation

In this part, we go all the way out such that we allow expressions with standard algebraic notation in the `deriv` program. This program should also take operator precedence into consideration when computing.

As before, the logic of the `deriv` procedure need not be altered. We are also told that we only have multipliaction and addition operations which simplifies the problem substantially.

First, let us look at parsing `sum` expressions. When given an expression containing a `'+` symbol (disregarding the sub-expressions), we split the equation into two parts around the symbol. For eg., $$3*x+x*y+5=(3*x)+(x*y+5)$$. We can safely do this because of the lower precedence of the addition compared to multiplication. With this in mind, let us write the selectors and predicates for `sum`.

{% highlight scheme %}
(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
      '()
	  (cons (car list) (prefix sym (cdr list)))))
; prefix

(define (trim list)
  (if (= (length list) 1)
      (car list)
      list))
; trim

(define (sum? x)
  (pair? (memq '+ x)))
; sum?

(define (addend s)
  (trim (prefix '+ s)))
; addend

(define (augend s)
  (trim (cdr (memq '+ s))))
; augend

(sum? '(3 * x + x * y + 5))
; #t
(addend '(3 * x + x * y + 5))
; (3 * x)
(augend '(3 * x + x * y + 5))
; (x * y + 5)
(addend '(x + 5))
; x
(augend '(x + 5))
; 5
{% endhighlight %}

Let us now redefine the definition for multiplication. A given expression can only be considered a product expression when two conditions are fulfilled:-
1. The expression contains a `'*` symbol (disregarding the sub-expressions).
2. The expression does not contain a `'+` symbol (disregarding the sub-expressions).

For example, $$3*(5+x)$$ can be considered a product expression whereas $$3*5+x$$ cannot be considered so because it is a sum expression of form $$(3*5)+x$$. With these ideas in mind, let us write the code for products:-

{% highlight scheme %}
(define (product? x)
  (and (not (sum? x)) (pair? (memq '* x))))
; product?

(define (multiplier p)
  (trim (prefix '* p)))
; multiplier

(define (multiplicand p)
  (trim (cdr (memq '* p))))
; multiplicand

(product? '(3 * 5 + x))
; #f
(product? '(3 * (5 + x)))
; #t
(multiplier '(3 * (5 + x)))
; 3
(multiplicand '(3 * (5 + x)))
; (5 + x)
{% endhighlight %}

Let us test the final program out:-

{% highlight scheme %}
(deriv '(x + 3 * (x + y + 2)) 'x)
; 4
(deriv '(x * 3 + 5 * (x + y + 2)) 'x)
; 8
(deriv '(x + 3 * (x + y + 2)) 'y)
; 3
(deriv '(x * x * x + 3 * x * x) 'x)
; (((x * (x + x)) + (x * x)) + (3 * (x + x)))
{% endhighlight %}

### Exercise 2.59<a name="Exercise2_59">&nbsp;</a>

In this exercise, we are asked to implement `union-set`, a procedure used to find the union of two sets represented as unordered-lists.

We can simply take the first set and use `adjoin-set` to combine them with the second set.

{% highlight scheme %}
(define (union-set set1 set2)
  (if (null? set1)
      set2
	  (union-set (cdr set1)
                 (adjoin-set (car set1) set2))))
; union-set

(union-set '(1 2 3) '(2 3 4))
; (1 2 3 4)
(union-set '(2 3 4) '(2 3 4))
; (2 3 4)
{% endhighlight %}

### Exercise 2.60<a name="Exercise2_60">&nbsp;</a>

In this exercise, we are asked to create a set representation that allows us to have duplicate elements in a set. It can be done using the folowing code:-

{% highlight scheme %}
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
; element-of-set?

(define (adjoin-set x set)
  (cons x set))
; adjoin-set

(define (union-set set1 set2)
  (append set1 set2))
; union-set

(define (intersection-set set1 set2)
  (define (remove-element x set)
    (if (equal? x (car set))
        (cdr set)
		(cons (car set) (remove-element x (cdr set)))))
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 (remove-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))
; intersection-set
{% endhighlight %}

Let us test this,

{% highlight scheme %}
(element-of-set? 'c '(a b c))
; #t
(element-of-set? 'd '(a b c c))
; #f
(adjoin-set 'c '(c d e))
; (c c d e)
(union-set '(a b c) '(c d e))
; (a b c c d e)
(intersection-set '(a b b c) '(a a b b))
; (a b b)
{% endhighlight %}

As can be seen, we get the expected results.

This kind of representation of sets is used for different use cases compared to the previous representation without any duplicates. One example is if we want to keep track how many times each element occurs instead of just the types of elements.

Compared to the previous case, efficiency is as follows:-

|                  | No Duplicates | With Duplicates |
|------------------|---------------|-----------------|
| element-of-set?  | $$O(n)$$      | $$O(n)$$        |
| adjoin-set       | $$O(n)$$      | $$O(1)$$        |
| union-set        | $$O(n^2)$$    | $$O(n)$$        |
| intersection-set | $$O(n^2)$$    | $$O(n^2)$$      |

We can see that having duplicates simplifies adjoin and union operations. This is because we do not perform `element-of-set?` anymore for these operations. However, if there are a lot of duplicate elements, the time complexity would go up for the `union-set` and `intersection-set` procedures because *n* would tend to be a high value for the lists with duplicate elements.
