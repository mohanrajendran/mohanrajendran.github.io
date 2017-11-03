---
layout: spiedpage
order: 28
title: Section 4.1 solutions
exercises: '4.1 - 4.2'
submenu:
  - { hook: "Exercise4_1", title: "Exercise 4.1" }
  - { hook: "Exercise4_2", title: "Exercise 4.2" }
---

### Exercise 4.1<a id="Exercise4_1">&nbsp;</a>

In this exercise, we are given the following function:-

{% highlight scheme %}
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))
{% endhighlight %}

It is noted that the order in which the `exps` are evaluated is the same as the order of the underlying language because we use `cons` to evaluate the list. We can use the `let` construct to evaluate the operands in the order w choose.

To evaluate the `list-of-values` from left to right regardless of the underlying implementation, we use the following code:-

{% highlight scheme %}
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-operands exps) env)))
          (cons first rest)))))
{% endhighlight %}

Likewise, we can evaluate from right to left using:-

{% highlight scheme %}
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))
{% endhighlight %}

### Exercise 4.2<a id="Exercise4_2">&nbsp;</a>

In this exercise, Louis Reasoner whanters to reorder the `cond` clauses in `eval` code so that procedure applications clause appears before assignments'. This is a bad idea mainly because the code as it is right now would causeall calls to go into application. Let us take the example code of `'(define x 3)`.

The given code is an assignment. However, by reordering, we first check if its an application by calling `(define (application? exp) (pair? exp))`. This would return true for the definition statement and execute an application which is false.

Now, Louis proposes another syntax for evaluation known as `call`. For example, we perform addition by using `(call + 1 2)`. This can be handled using the new syntax:-

{% highlight scheme %}
(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
{% endhighlight %}
