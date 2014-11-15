---
layout: post
category: [SICP, Solutions]
post_no: 17
title: "SICP Section 2.2 Exercise Solutions - Part 3"
submenu:
   - { hook: "Exercise2_30", title: "Exercise 2.30" }
   - { hook: "Exercise2_31", title: "Exercise 2.31" }
   - { hook: "Exercise2_32", title: "Exercise 2.32" }
   - { hook: "Exercise2_33", title: "Exercise 2.33" }
   - { hook: "Exercise2_34", title: "Exercise 2.34" }
   - { hook: "Exercise2_35", title: "Exercise 2.35" }
   - { hook: "Exercise2_36", title: "Exercise 2.36" }
   - { hook: "Exercise2_37", title: "Exercise 2.37" }
   - { hook: "Exercise2_38", title: "Exercise 2.38" }
   - { hook: "Exercise2_39", title: "Exercise 2.39" }
---

### Exercise 2.30<a name="Exercise2_30">&nbsp;</a>

In this exercise, we are tasked with writing `square-tree` which is analogous to `square-list` which can be used to square all the elements in a list and maintain the overall structure of the list. Let us look at how to do this directly.

{% highlight scheme %}
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; square-tree

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
{% endhighlight %}
<!--excerpt-->

Now, this function could also be rewritten using the `map` function and recursion. Let us see how this is done:-

{% highlight scheme %}
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
			 (square-tree sub-tree)
			 (square sub-tree)))
       tree))
; square-tree

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
{% endhighlight %}

### Exercise 2.31<a name="Exercise2_31">&nbsp;</a>

In this exercise, we are tasked with creating a procedure `tree-map` which serves the same purpose for as the `map` procedure does for lists. Let us define it using the code from the previous exercise as a template.

{% highlight scheme %}
(define (tree-map proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
; tree-map

(define (square-tree tree) 
  (tree-map square tree))
; square-tree

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
{% endhighlight %}

### Exercise 2.32<a name="Exercise2_32">&nbsp;</a>

The `subsets` function can be defined as follows by filling in the blanks.

{% highlight scheme %}
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (r)
                       (cons (car s) r))
                     rest)))))
; subsets

(subsets (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
{% endhighlight %}

The definition of powerset [here](http://en.wikipedia.org/wiki/Power_set) gives the exact algorithm used by us in our `subsets` procedure. To prove that this works, let us use the method of induction.

To prove that, let us take two axioms:-
1. `(subset (cons x y))` contains all the members of `(subset y)`
2. In `(subset (cons x y))`, exactly half the members have `x`, since given any set, there is a 50/50 chance of `x` being there.

Now, onto the proof:-
* The subset of an empty list is a list of empty list. ie `(subset ())` is `(())`.

* We assume for a list `y`, `(subset y)` gives the correct set of subsets. We add one more element to the set to obtain `(cons x y)`.  By axiom 1, `(subset y)` gives all the subsets of `(subset (cons x y))` which does not contain the element `x`. By axiom 2, `(subset (cons x y))` has exactly twice as many members as `(subset y)`. Thus, we get the new set by duplicating the members of `(subset y)` with `x` thrown in.

By following this principle, we get the correct `subsets` function.

### Exercise 2.33<a name="Exercise2_33">&nbsp;</a>

In this exercise, we are tasked with rewriting `map`, `append` and `length` functions using `accumulate`.

##### `map`

The `map` function can be written as follows:-

{% highlight scheme %}
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))
; map

(map square (list 1 2 3 4 5))
; (1 4 9 16 25)
{% endhighlight %}

##### `append`

The `append` function can be written as folows:-

{% highlight scheme %}
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
; append

(append (list 1 2 3) (list 4 5 6))
; (1 2 3 4 5 6)
(append () (list 1 2 3))
; (1 2 3)
{% endhighlight %}

##### `length`

The `length` function can be written as follows:-

{% highlight scheme %}
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
; length

(length (list 1 2 3 4))
; 4
{% endhighlight %}

### Exercise 2.34<a name="Exercise2_34">&nbsp;</a>

In this exercise, we are tasked with evaluating a polynomial $$a_{n}x^{n}+a_{n-1}x^{n-1}+...+a_{1}x+a_{0}$$ at the given value of *x* using [Horner's Rule](http://en.wikipedia.org/wiki/Horner%27s_method) which is a recursive procedure which evaluates the polynomial by evaluating it as $$(...(a_{n}x+a_{n-1})x+...+a_1)x+a_{0}$$.

The procedure is defined as follows:-

{% highlight scheme %}
(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))
; horner-eval

(horner-eval 2 (list 1 3 0 5 0 1))
; 79
{% endhighlight %}

### Exercise 2.35<a name="Exercise2_35">&nbsp;</a>

In this exercise, we are tasked with redefining the `count-leaves` procedure defined earlier as follows:-

{% highlight scheme %}
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
{% endhighlight %}

We are told to use the `accumulate` and `map` function in the redefinition. For that purpose, let us reuse the `enumerate-tree` given in the book:-

{% highlight scheme %}
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))
; enumerate-tree

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))
; count-leaves

(define x (cons (list 1 2) (list 3 4)))
; x
(count-leaves (list x x))
; 8
{% endhighlight %}

### Exercise 2.36<a name="Exercise2_36">&nbsp;</a>

In this exercise, we need to implement a procedure `accumulate-n` which takes a list of lists and returns an element-wise accumulation in the form of another list. We assume each sub-list in the argument and the resulting list has the same number of elements each.

{% highlight scheme %}
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; accumulate-n

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
; s
s
; ((1 2 3) (4 5 6) (7 8 9) (10 11 12))
(accumulate-n + 0 s)
; (22 26 30)
{% endhighlight %}

### Exercise 2.37<a name="Exercise2_37">&nbsp;</a>

In this exercise, we are tasked with implementing matrix operations. The initial dot product procedure is defined as follows:-

{% highlight scheme %}
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; dot-product

(dot-product (list 1 2 3 4) (list 5 6 7 8))
; 70
{% endhighlight %}

Note that it is mentioned in the book that the `map` procedure used is the original once defined in Scheme which can take an arbitrary number of lists as arguments.

##### Matrix-vector multiplication

The matrix-vector multiplication can be defined as follows:-

{% highlight scheme %}
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
; matrix-*-vector

(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
; m
(define v (list 1 2 3))
; v

(matrix-*-vector m v)
; (14 32 50)
{% endhighlight %}

##### Transpose function

The transpose function can be defined as follows:-

{% highlight scheme %}
(define (transpose mat)
  (accumulate-n cons nil mat))
; transpose

m
; ((1 2 3) (4 5 6) (7 8 9))
(transpose m)
; ((1 4 7) (2 5 8) (3 6 9))
{% endhighlight %}

##### Matrix-matrix multiplication

The matrix-matrix multiplication can be defined as follows:-

{% highlight scheme %}
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))
; matrix-*-matrix

(define n (list (list 9 8 7) (list 6 5 4) (list 3 2 1)))
; n

(matrix-*-matrix m n)
; ((30 24 18) (84 69 54) (138 114 90))
{% endhighlight %}

All the computations were verified using [WolframAlpha](www.wolframalpha.com)

### Exercise 2.38<a name="Exercise2_38">&nbsp;</a>

In this exercise we examine `fold-left` and `fold-right` procedures.

{% highlight scheme %}
(define (fold-right op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))
{% endhighlight %}

Other than the fact that `fold-left` is iterative whereas `fold-right` is recursive, we can test the difference in results by looking at the examples provided.

{% highlight scheme %}
(fold-right / 1 (list 1 2 3))
; 3/2
(fold-left / 1 (list 1 2 3))
; 1/6

(fold-right list nil (list 1 2 3))
; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
; (((() 1) 2) 3)
{% endhighlight %}

As can be seen, we get totally different results for both cases. Let us look at the expansion of each expression,

{% highlight scheme %}
(fold-right op initial (list a b c))

(op a (fold-right op initial (list b c)))

(op a (op b (fold-right op initial (list c))))

(op a (op b (op c (fold-right op initial ()))))

(op a (op b (op c initial)))
{% endhighlight %}

{% highlight scheme %}
(fold-left op initial (list a b c))

(iter initial (list a b c))

(iter (op intial a) (list b c))

(iter (op (op intial a) b) (list c))

(iter (op (op (op intial a) b ) c) ())

(op (op (op inital a) b ) c)
{% endhighlight %}

Now, to get similar operations for both procedures, we need two properties:-

1. `(op initial a)` must be the same as `(op a initial)`. This is evident when we try this operation on a list with one element. This property is called [commutativity](http://en.wikipedia.org/wiki/Commutative_property).

2. `(op (op a b) c)` must be the same as `(op a (op b c))`. This is evident when we look at the final form of the expansions. This property is called [associativity](http://en.wikipedia.org/wiki/Associative_property).

In the given example, the division function and list functions fulfilled neither properties. Let us try with a function that fulfils both cases:-

{% highlight scheme %}
(fold-left * 1 (list 1 2 3))
; 6
(fold-right * 1 (list 1 2 3))
; 6
{% endhighlight %}

### Exercise 2.39<a name="Exercise2_39">&nbsp;</a>

In this exercise, we are tasked with defining the `reverse` procedure using the `fold-right` and `fold-left` from before.

{% highlight scheme %}
(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))
; reverse

(reverse (list 1 2 3))
; (3 2 1)

(define (reverse sequence)
  (fold-left 
   (lambda (x y) (append (list y) x)) nil sequence))
; reverse

(reverse (list 1 2 3))
; (3 2 1)
{% endhighlight %}

