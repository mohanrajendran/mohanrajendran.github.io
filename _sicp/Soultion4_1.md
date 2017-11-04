---
layout: spiedpage
order: 28
title: Section 4.1 solutions
exercises: '4.1 - 4.3'
submenu:
  - { hook: "Exercise4_1", title: "Exercise 4.1" }
  - { hook: "Exercise4_2", title: "Exercise 4.2" }
  - { hook: "Exercise4_3", title: "Exercise 4.3" }
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

### Exercise 4.3<a id="Exercise4_3">&nbsp;</a>

In this exercise, we are tasked with rewriting `eval` so that the dispatch is done in a data-directed style. Based on the code from section 2, we set up an initial dispatch table as follows along with functions for tagged expressions.

{% highlight scheme %}
(define dispatch-table (make-hash-table equal?))
(define (put-op type proc)
  (hash-table/put! dispatch-table type proc))
(define (get-op type)
  (hash-table/get dispatch-table type #f))

(put-op 'quote (lambda (exp env) (text-of-quotation exp)))
(put-op 'set! eval-assignment)
(put-op 'define eval-definition)
(put-op 'if eval-if)
(put-op 'lambda (lambda (exp env)
                  (make-procedure
                   (lambda-parameters exp)
                   (lambda-body exp)
                   env)))
(put-op 'begin (lambda (exp env)
                 (eval-sequence
                  (begin-actions exp)
                  env)))
(put-op 'cond (lambda (exp env)
                (analyze (cond->if exp))))
{% endhighlight %}

Based on the above set-up, let us rewrite the `eval` function by replacing the tagged expressions with the data-direction:-

{% highlight scheme %}
((define (eval exp env)
   (cond ((self-evaluating? exp)
          exp)
         ((variable? exp)
          (lookup-variable-value exp env))
         ((get-op (car exp))
          ((get-op (car exp)) exp env))
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values
                  (operands exp)
                  env)))
         (else
          (error "Unknown expression
                 type: EVAL" exp)))))
{% endhighlight %}
