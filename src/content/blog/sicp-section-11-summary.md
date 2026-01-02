---
title: "SICP Section 1.1 Summary"
description: "SICP exercises  - Section 1.1 Summary"
pubDate: 2017-03-05
tags: ["sicp", "computer-science", "scheme", "programming", "functional-programming"]
---

# Elements of Programming

This section gives a basic understanding of [Scheme](http://en.wikipedia.org/wiki/Scheme_%28programming_language%29), the language used in this book. It also goes through the three essential components of any programming language:-

- **primitive expressions** used to represent the simplest entities in a language (*arithmetic, conditional evaluation, etc*)
- **combinations** arising from combining the primitive expressions (*mathematical formulas, etc*)
- **abstractions** which are combinations manipulated as units (*functions, subroutines, etc*)

#### Expressions

In Scheme, basic expressions can be created simply by using primitive functions. For example,

```scheme
(+ 137 349)
; 486
(* 5 99)
; 495
(< 5 4)
; #f
```

#### Combinations

Expressions can be combined together to obtain combinations. For example,

```scheme
(if (= 3 (/ 6 2))
    7
    10)
; 7
(+ (* 5 4) (10 / 2))
; 25
```

#### Abstractions

At this stage, abstractions can be thought of simply as named functions and values. For example,

```scheme
(define pi 3.14159)
; pi
pi
; 3.14159
(define (square x) (* x x))
; square
(square 6)
; 36
```

Abstractions can be used to build more complex programs easily within human comprehension. Further, defining absractions help build the [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) enviroment in Scheme.

#### Evaluation order

Evaluation order is an essential lesson to be learnt from this section. Simply speaking, this deals with how compound expressions are evaluated in Scheme. Abstractions are substituted with their primitive-level definitions and evaluated. There are two types of evaluation order.

- **Applicative-order evaluation** is the method used by Scheme. Primitive expressions are evaluated as soon as they appear in a substituted expression. An advantage is that less operations can be done in cases where subsequent expansions would cause duplication in an expression.
- **Normal-order evaluation** is slightly different in the sense evaluation is deferred until the expression is fully expanded. This has the advantage of lazy evaluation in some cases of conditional statements where we can skip evaluation of segments that are not required.

Both evaluation orders are studied in the exercises of this section.

#### Imperative knowledge

This section also emphasizes on the difference between *imperative* and *declarative* knowledge. Declarative knowledge is usually what math concerns with - the description of properties. Whereas, computer programs deal with imperative knowledge - procedure to obtain something. To demonstrate this, we write LISP code to compute square root of a number as opposed to mathematical description which simply states $$\sqrt{x} = y$$ such that $$y >= 0$$ and $$y^2=x$$ but does not actually say anything about obtaining the square root.
