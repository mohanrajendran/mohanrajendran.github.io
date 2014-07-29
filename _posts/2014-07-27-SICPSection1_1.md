---
layout: post
category: SICP
title: SICP Section 1.1
---

# Elements of Programming

This section gives a basic understanding of [Scheme](http://en.wikipedia.org/wiki/Scheme_%28programming_language%29), the language used in this book. It also goes through the three essential components of any programming language:-

- **primitive expressions** used to represent the simplest entities in a language (*arithmetic, conditional evaluation, etc*)
- **combinations** arising from combining the primitive expressions (*mathematical formulas, etc*)
- **abstractions** which are combinations manipulated as units (*functions, subroutines, etc*)

<!--excerpt-->

#### Expressions

In Scheme, basic expressions can be created simply by using primitive functions. For example,

{% highlight scheme %}
(+ 137 349)
; 486
(* 5 99)
; 495
(< 5 4)
; #f
{% endhighlight %}

#### Combinations

Expressions can be combined together to obtain combinations. For example,

{% highlight scheme %}
(if (= 3 (/ 6 2))
    7
    10)
; 7
(+ (* 5 4) (10 / 2))
; 25
{% endhighlight %}

#### Abstractions

At this stage, abstractions can be thought of simply as named functions and values. For example,

{% highlight scheme %}
(define pi 3.14159)
; pi
pi
; 3.14159
(define (square x) (* x x))
; square
(square 6)
; 36
{% endhighlight %}

Abstractions can be used to build more complex programs easily within human comprehension. Further, defining absractions help build the [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) enviroment in Scheme.

#### Evaluation order

Evaluation order is an essential lesson to be learnt from this section. Simply speaking, this deals with how compound expressions are evaluated in Scheme. Abstractions are substituted with their primitive-level definitions and evaluated. There are two types of evaluation order.

- **Applicative-order evaluation** is the method used by Scheme. Primitive expressions are evaluated as soon as they appear in a substituted expression. An advantage is that less operations can be done in cases where subsequent expansions would cause duplication in an expression.
- **Normal-order evaluation** is slightly different in the sense evaluation is deferred until the expression is fully expanded. This has the advantage of lazy evaluation in some cases of conditional statements where we can skip evaluation of segments that are not required.

Both evaluation orders are studied in the exercises of this section.

#### Imperative knowledge

This section also emphasizes on the difference between *imperative* and *declarative* knowledge. Declarative knowledge is usually what math concerns with - the description of properties. Whereas, computer programs deal with imperative knowledge - procedure to obtain something. To demonstrate this, we write code to compute square root of a number as opposed to mathematical description which states $$\sqrt{x} = y$$ such that $$y >= 0$$ and $$y^2=x$$.

### Exercise 1.1

This exercise deals with evaluating some Scheme functions and reporting their output. This acts as an introduction to Scheme syntax.

{% highlight scheme %}
10
; 10
(+ 5 3 4)
; 12
(- 9 1)
; 8
(/ 6 2)
; 3
(+ (* 2 4) (- 4 6))
; 6
(define a 3)
; a
(define b (+ a 1))
; b
(+ a b (* a b))
; 19
(= a b)
; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16
(+ 2 (if (> b a)
     b
     a))
; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16
{% endhighlight %}

### Exercise 1.2

This exercise deals with the translation of an equation into the prefix form. The equation and expected answer are as follows:-

$$\frac{5+4+(2-(3-(6+\frac{4}{5})))}{3(6-2)(2-7)}=-\frac{37}{150}$$

The prefix form and evaluation in Scheme results in:-

{% highlight scheme %}
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
; -37/150
{% endhighlight %}

### Exercise 1.3

We are tasked with defining a procedure which takes in 3 arguments and returns the square of the 2 largest arguments. We solve this by building the solution step by step

{% highlight scheme %}
(define (sq x) (* x x))
; sq
 
(define (sumsq x y) (+ (sq x) (sq y)))
; sumsq
 
(define (largest2sq a b c)
(cond ((and (<= a b) (<= a c)) (sumsq b c))
((and (<= b a) (<= b c)) (sumsq a c))
(else (sumsq a b))))
; largest2sq
 
; Test cases:-
(largest2sq 1 2 3)
; 13
(largest2sq 1 3 2)
; 13
(largest2sq 1 1 2)
; 5
{% endhighlight %}

### Exercise 1.4

This exercise tasks us with explaining the behavior of the following procedure.

{% highlight scheme %}
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
{% endhighlight %}

When the if function is evaluated, based on the condition, the operator + or - is applied on the operands a and b. If b > 0, + operator is used, evaluating to $$a+b$$. Otherwise, - operator is issued which yields $$a-b$$. Ultimately, we evaluate to $$a+\|b\|$$.

### Exercise 1.5

This exercise deals with examining how Lisp would behave in appilcative-order evaluation and normal order evaluation.

{% highlight scheme %}
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
{% endhighlight %}

When we evaluate the above expression, one of the following occurs based on the evaluation order.

##### Applicative-order evaluation

{% highlight scheme %}
(test 0 (p))

(test 0 (p))

(test 0 (p))
...
{% endhighlight %}

In *applicative-order evaluation*, the expression fails to terminate. This is because each formal parameter is replaced by its corresponding argument first(operator and operands are both expanded). Thus, the interpreter tries to evaluate (p) first which itself evaluates to (p) and so on. Thus, the evaulation does not terminate.

##### Normal-order evaluation

{% highlight scheme %}
(test 0 (p))

(if (= 0 0 ) 0 (p))

(if #t 0 (p))

0
{% endhighlight %}

In *normal-order evaluation*, the operand is not evaluated until it is required to do so. Thus, the interpreted evaluates the operator *test* first. Since the condition evaluates to #t for the resulting if operation, only the first argument is evaluated which is 0.

Overall, MIT Scheme exhibits **applicative-order evaluation**. Thus, the evaluation never halts.
