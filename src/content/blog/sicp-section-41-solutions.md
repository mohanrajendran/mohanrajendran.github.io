---
title: "SICP Section 4.1 solutions"
description: "SICP exercises 4.1 - 4.7 - Section 4.1 solutions"
pubDate: 2017-11-06
tags: ["sicp", "computer-science", "scheme", "programming", "functional-programming"]
---

### Exercise 4.1

In this exercise, we are given the following function:-

```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))
```

It is noted that the order in which the `exps` are evaluated is the same as the order of the underlying language because we use `cons` to evaluate the list. We can use the `let` construct to evaluate the operands in the order w choose.

To evaluate the `list-of-values` from left to right regardless of the underlying implementation, we use the following code:-

```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-operands exps) env)))
          (cons first rest)))))
```

Likewise, we can evaluate from right to left using:-

```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))
```

### Exercise 4.2

In this exercise, Louis Reasoner whanters to reorder the `cond` clauses in `eval` code so that procedure applications clause appears before assignments'. This is a bad idea mainly because the code as it is right now would causeall calls to go into application. Let us take the example code of `'(define x 3)`.

The given code is an assignment. However, by reordering, we first check if its an application by calling `(define (application? exp) (pair? exp))`. This would return true for the definition statement and execute an application which is false.

Now, Louis proposes another syntax for evaluation known as `call`. For example, we perform addition by using `(call + 1 2)`. This can be handled using the new syntax:-

```scheme
(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
```

### Exercise 4.3

In this exercise, we are tasked with rewriting `eval` so that the dispatch is done in a data-directed style. Based on the code from section 2, we set up an initial dispatch table as follows along with functions for tagged expressions.

```scheme
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
                (eval (cond->if exp))))
```
Based on the above set-up, let us rewrite the `eval` function by replacing the tagged expressions with the data-direction:-

```scheme
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
```

### Exercise 4.4

In this exercise, we are tasked with implementing the functionality to evaluate `and` and `or` functions and install them to the `eval` function. The code can be written using `make-if` and following the same pattern as `cond`:-

```scheme
(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses clauses)
               'false)))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-and-clauses clauses))))

(define (eval exp env)
  (cond (...)
        ((and? exp) (eval (and->if exp)))
        ((or? exp) (eval (or->if exp)))
        (...)))
```

### Exercise 4.5

In this exercise, we need to add a way to evaulate an additional syntax for `cond` of type `(test => recipient)`. To do that, we define the following tester functions:-

```scheme
(define (cond-apply-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-apply-test clause)
  (car clause))
(define (cond-apply-recipient clause)
  (caddr clause))
```

Next, we modify the `cond` clause generator to include the syntax:-

```scheme
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't
                        last: COND->IF"
                       clauses))
            (if (cond-apply-clause? first)
                (list (make-lambda '(_cond-test)
                                   (make-if (cond-apply-test first)
                                            (list _cond-test (cond-apply-test first))
                                            (expand-clauses rest)))
                      (cond-apply-test first))
                (make-if (cond-predicate first)
                         (sequence->exp
                          (cond-actions first))
                         (expand-clauses
                          rest)))))))
```

We use the lambda expression to make sure that the `test` is only run once in case it has side-effects.

### Exercise 4.6

In this exercise, we need to rewrite `let` expressions as derived expressions composed of `lambda` expressions. This can be done using the following code:-

```scheme
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-bindings exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))
(define (binding-names bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings)
            (binding-names bindings))))
(define (binding-values bindings)
  (if (null? bindings)
      '()
      (cons (cadr bindings)
            (binding-names bindings))))
(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (list (make-lambda (binding-names bindings)
                       (let-body exp))
          (binding-values bindings))))
```

Now we can handle `let` by modifying `eval` as follows:-

```scheme
(define (eval exp env)
  (cond (...)
        ((let? exp) (eval (let->combination exp)))
        (...)))
```

### Exercise 4.7

In this exercise, we need to write a converter for `let*` expression which is like `let` but allows for binding the variables one by one such that later bindings have access to the earlier bindings. Thus, `let*` can be expressed in terms of recursive `let`. The code to do that is as follows:-

```scheme
(define (let*? exp)
  (tagged-list? exp 'let?))
(define (let*->nested-lets exp)
  (expand-let* (let-bindings exp) (let-body exp)))
(define (expand-let* bindings body)
  (if (null? bindings)
      body
      (list
       'let
       (list car-bindings)
       (expand-let (cdr bindings) body))))
```

It is sufficient to add a clause `(eval (let*->nested-lets exp) env)` to `eval`. The `eval` function can recursively transform the expression until the primitive value.
