---
layout: spiedpage
order: 12
title: Section 2.3 solutions
exercises: '2.53 - 2.72'
submenu:
  - { hook: "Exercise2_53", title: "Exercise 2.53" }
  - { hook: "Exercise2_54", title: "Exercise 2.54" }
  - { hook: "Exercise2_55", title: "Exercise 2.55" }
  - { hook: "Exercise2_56", title: "Exercise 2.56" }
  - { hook: "Exercise2_57", title: "Exercise 2.57" }
  - { hook: "Exercise2_58", title: "Exercise 2.58" }
  - { hook: "Exercise2_59", title: "Exercise 2.59" }
  - { hook: "Exercise2_60", title: "Exercise 2.60" }
  - { hook: "Exercise2_61", title: "Exercise 2.61" }
  - { hook: "Exercise2_62", title: "Exercise 2.62" }
  - { hook: "Exercise2_63", title: "Exercise 2.63" }
  - { hook: "Exercise2_64", title: "Exercise 2.64" }
  - { hook: "Exercise2_65", title: "Exercise 2.65" }
  - { hook: "Exercise2_66", title: "Exercise 2.66" }
  - { hook: "Exercise2_67", title: "Exercise 2.67" }
  - { hook: "Exercise2_68", title: "Exercise 2.68" }
  - { hook: "Exercise2_69", title: "Exercise 2.69" }
  - { hook: "Exercise2_70", title: "Exercise 2.70" }
  - { hook: "Exercise2_71", title: "Exercise 2.71" }
  - { hook: "Exercise2_72", title: "Exercise 2.72" }
---

### Exercise 2.53<a id="Exercise2_53">&nbsp;</a>

In this exercise, we are simply asked to determine the responses from the interpreter when given expressions are evauated. This is to understand the quotation mechanic.

1. When we evaluate `(list 'a 'b 'c)`, `a`, `b` and `c` are quoted directly, thus, we get `(a b c)` directly.

2. When we evaluate `(list (list 'george))`, `george` is quoted. Including nested lists, we expect `((george))`.

3. When we evaluate `(cdr '((x1 x2) (y1 y2)))`, we get the list with the first element removed. We expect `((y1 y2))`.

4. When we evaluate `(cadr '((x1 x2) (y1 y2)))`, we get the `car` of the previous result which is simple `(y1 y2)`.

5. When we evaluate `(pair? (car '(a short list)))`, we check if `a` is a pair which should result in `#f`.

6. When we evaluate `(memq 'red '((red shoes) (blue socks)))`, we should get `#f` since there is no member in the main list directly equal to `red`

7. When we evaluate `(memq 'red '(red shoes blue socks))`, we should get the whole list in return because the first element is the element we are looking for. We expect `(red shoes blue socks)`

Now, let us actually run it in the REPL:-

{% highlight scheme %}
(list 'a 'b 'c)
; (a b c)

(list (list 'george))
; ((george))

(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)
{% endhighlight %}

### Exercise 2.54<a id="Exercise2_54">&nbsp;</a>

In this exercise, we are tasked with writing a procedure called `equal?` which can be used to recursively determine the equality of two lists of symbols. We are given `eq?` which evaluates to true when the two arguments are the same symbols.

{% highlight scheme %}
(define (equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
	          (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))
; equal?

(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f

(equal? '(this (is a) list) '(this (is a) list))
; #t
{% endhighlight %}

### Exercise 2.55<a id="Exercise2_55">&nbsp;</a>

When Eva Lu Ator typed `(car ''abracadabra)`, the REPL printed back `quote`. This exercise ask us to explain how.

When we type the original quoted expression in the REPL, we get the following:-

{% highlight scheme %}
''abracadabra
; (quote abracadabra)
{% endhighlight %}

Thus, by using two quotes, we also quote in the inner quote. The inner quote thus does not get evaluated and shows up as is. When we take the `car` of this list, we simply get the `quote` back.

### Exercise 2.56<a id="Exercise2_56">&nbsp;</a>

In this exercise, we are tasked with extending the `deriv` program given in the book to handle exponentiations of the form:-

$$\frac{d(u^n)}{dx} = nu^{n-1}\frac{du}{dx}$$

We can do that by first implementing the data structure for exponentiation.

{% highlight scheme %}
(define (exponent? exp)
  (and (pair? exp) (eq? (car exp) '**)))
; exponent?

(define (base exp)
  (cadr exp))
; base

(define (exponent exp)
  (caddr exp))
; exponent

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        ((number? exponent) (list '** base exponent))
		(else (error "cannot exponentiate by an expression"))))
; make-exponentiation
{% endhighlight %}

Let us now include this code in the `deriv` expression.

{% highlight scheme %}
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponent? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))
{% endhighlight %}

Let us test this code:-

{% highlight scheme %}
(deriv '(** x 5) 'x)
; (* 5 (** x 4))

(deriv '(+ (** x 3) (** x 5)) 'x)
; (+ (* 3 (** x 2)) (* 5 (** x 4)))

(deriv '(* a (** x 5)) 'x)
; (* a (* 5 (** x 4)))
{% endhighlight %}

### Exercise 2.57<a id="Exercise2_57">&nbsp;</a>

In this exercise, we are tasked with extending the differentiation program to handle the sums and products of arbitrary number of terms.

We can do this by simply modifying the selector functions to return another sum/product expression if there are more than two terms.

{% highlight scheme %}
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
	  (cons '+ (cddr s))))
; augend

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
	  (cons '* (cddr p))))
; multiplicand
{% endhighlight %}

Let us test this:-

{% highlight scheme %}
(augend '(+ a b c))
; (+ b c)
(augend '(+ a b))
; b

(deriv '(* x y (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))
{% endhighlight %}

### Exercise 2.58<a id="Exercise2_58">&nbsp;</a>

In this exercise, we are required to modify the `deriv` program to work on a more natural form of expressing equations. 

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

### Exercise 2.59<a id="Exercise2_59">&nbsp;</a>

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

### Exercise 2.60<a id="Exercise2_60">&nbsp;</a>

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

|                  | No Duplicates      | With Duplicates      |
|------------------|--------------------|----------------------|
| element-of-set?  | $$\Theta(n)$$      | $$\Theta(n)$$        |
| adjoin-set       | $$\Theta(n)$$      | $$\Theta(1)$$        |
| union-set        | $$\Theta(n^2)$$    | $$\Theta(n)$$        |
| intersection-set | $$\Theta(n^2)$$    | $$\Theta(n^2)$$      |

We can see that having duplicates simplifies adjoin and union operations. This is because we do not perform `element-of-set?` anymore for these operations. However, if there are a lot of duplicate elements, the time complexity would go up for the `union-set` and `intersection-set` procedures because *n* would tend to be a high value for the lists with duplicate elements.

### Exercise 2.61<a id="Exercise2_61">&nbsp;</a>

In this exercise, we look at creating an implementation of `adjoin-set` for sets represented using ordered lists.

This function is similar to the `element-of-set?` function given in the book.

{% highlight scheme %}
(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
		(else (cons (car set) (adjoin-set x (cdr set))))))
; adjoin-set

(adjoin-set 1 '())
; (1)

(adjoin-set 1 '(1 2 3))
; (1 2 3)

(adjoin-set 4 '(1 2 3))
; (1 2 3 4)

(adjoin-set 2.5 '(1 2 3))
; (1 2 2.5 3)
{% endhighlight %}

Similar to the `element-of-set?` function from before, this function will take half as many steps in average compared to the un-ordered version.

### Exercise 2.62<a id="Exercise2_62">&nbsp;</a>

In this exercise, we need to implement an $$\Theta(n)$$ time implementation of `union-set` for ordered lists.

{% highlight scheme %}
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2))
                    (y1 (cdr set1))
                    (y2 (cdr set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set y1 y2)))
                      ((< x1 x2)
                       (cons x1 (union-set y1 set2)))
                      (else
                       (cons x2 (union-set set1 y2))))))))
; union-set

(union-set '() '(1))
; (1)

(union-set '(1) '(1 2))
; (1 2)

(union-set '(1 3 5) '(2 4 6))
; (1 2 3 4 5 6)

(union-set '(2 3 5 7) '(2 4 6))
; (2 3 4 5 6 7)
{% endhighlight %}

This is a single sweep algorithm similar to the merging step in [Merge sort](http://en.wikipedia.org/wiki/Merge_sort). It has $$\Theta(n)$$ time complexity.

### Exercise 2.63<a id="Exercise2_63">&nbsp;</a>

In this exercise, we are given two recursive procedures for converting trees to lists. We are asked to compare both of them. Here is the given code:-

{% highlight scheme %}
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))
; tree->list-1

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))
; tree->list-2
{% endhighlight %}

Let us test them with some example trees:-

{% highlight scheme %}
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; tree1
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
; tree2
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
; tree3

(tree->list-1 tree1)
; (1 3 5 7 9 11)
(tree->list-2 tree1)
; (1 3 5 7 9 11)

(tree->list-1 tree2)
; (1 3 5 7 9 11)
(tree->list-2 tree2)
; (1 3 5 7 9 11)

(tree->list-1 tree3)
; (1 3 5 7 9 11)
(tree->list-2 tree3)
; (1 3 5 7 9 11)
{% endhighlight %}

We get the same results for both cases. Now let us look deeper into each version.

Given a tree node, `tree->list-1` recursively accumulates the elements on the left branch and appends to the list created by `cons` of the entry and the accumulation of the right branch. We have already seen that `append` operation takes $$O(n)$$ time. For a balanced tree containing *n* elements, $$T(n)=2T(n/2)+O(n)$$. Using the [Master theorem](http://en.wikipedia.org/wiki/Master_theorem), we can see that the time complexity is $$\Theta(n log{n})$$.

As for `tree->list-2`, the process keeps a running list of all the accumulated values so far and returns that once all nodes are visited. At a tree node, it copies all the nodes in the right edge using `cons` to the given list, add its own entry and then feeds it to the left child elements. By using `cons`, it follows the more efficient method of collecting larger elements on the deeper end of the list first. For each node, we only call a `cons` procedure which takes $$O(1)$$ time. In total for *n* nodes, this procedure takes $$\Theta(n)$$ time.

### Exercise 2.64<a id="Exercise2_64">&nbsp;</a>

In this exercise, we are given the code that generates a balanced binary tree given a list of elements in ascending order.

{% highlight scheme %}
(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))
; list->tree

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))
; partial-tree

(list->tree '(1 3 5 7 9 11))
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
{% endhighlight %}

The diagram of the tree that was produced is given below:-

<center><img src="/images/Ex2_64_Tree.svg" alt="Balanced binary tree" height="200"/></center>

`list->tree` works by delegating its job to `partial-tree` which takes in two arguments, a list `elts` and a number `n`. With these, it returns a tuple. The first element contains the balanced tree constructed using the first *n* members of *elts* and the remaining members in a list. The initial recursion is initiated by requesting a tree with all the elements in the list.

Now, for each iteration in `partial-tree`, we first compute the number of elements needed in the left branch. It is simply the first $$(n-1)/2$$ elements in the list. Thus, by calling `(partial-tree elts left-size)`, we get a balanced tree for the left branch and the remaining elements not used to construct the branch. Next, we take the first element of the remaining elements to the the node entry, then construct the right branch by calling `(partial-tree (cdr non-left-elts) right-size))`. Finally obtaining all three, we construct a tree using `(make-tree this-entry left-tree right-tree)`, then `cons` it with the elements remaining after making the right tree and return this tuple.

At every element, we call `cons`, `car`, `cdr` and `make-tree` a fixed number of times, each operation takes $$O(1)$$ time. Thus, the total time complexity for *n* elements is $$\Theta(n)$$.

### Exercise 2.65<a id="Exercise2_65">&nbsp;</a>

This exercise tasks us with implementing a $$\Theta(n)$$ implementation for `union-set` and `intersection-set` for sets implemented as balanced binary trees. This can be done very simply. First we convert a balanced binary tree to a list in ascending order. Next, we perform union and intersection on the ordered lists. Finally, we convert the ordered list back to a balanced tree. Each of the steps can be completed in $$O(n)$$ time using the procedures we have developed earlier. The first one can be done using `tree->list-2` and the last one can be done using `list->tree`.

The code is as follows:-

{% highlight scheme %}
(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (let ((x1 (car list1))
                      (x2 (car list2))
                      (y1 (cdr list1))
                      (y2 (cdr list2)))
                  (cond ((= x1 x2)
                         (cons x1 (union-list y1 y2)))
                        ((< x1 x2)
                         (cons x1 (union-list y1 list2)))
                        (else
                         (cons x2 (union-list list1 y2))))))))
  (list->tree (union-list (tree->list-2 set1)
                          (tree->list-2 set2))))
; union-set

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1)) (x2 (car list2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-list 
                           (cdr list1)
                           (cdr list2))))
                ((< x1 x2) (intersection-list 
                            (cdr list1) 
                            list2))
                ((< x2 x1) (intersection-list 
                            list1 
                            (cdr list2)))))))
  (list->tree (intersection-list (tree->list-2 set1)
                                 (tree->list-2 set2))))
; intersection-set
{% endhighlight %}

### Exercise 2.66<a id="Exercise2_66">&nbsp;</a>

In this exercise, we are told to implement a `lookup` procedure that essentially finds a record from a set of records structured as a binary tree ordered by the numerical value of the keys.

The following code can be used for that purpose:-

{% highlight scheme %}
(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((= given-key (key (entry tree-of-records)))
         (entry tree-of-records))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        (else
         (lookup given-key (right-branch tree-of-records)))))
{% endhighlight %}

This procedure can retreive a record in $$\Theta(log{n})$$ time complexity.

### Exercise 2.67<a id="Exercise2_67">&nbsp;</a>

This exercise puts the given Huffman decoding procedure through its paces. Thus, its a simple evaluation in the REPL.

{% highlight scheme %}
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
; sample-tree

sample-tree
; ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; sample-message

(decode sample-message sample-tree)
; (a d a b b c a)
{% endhighlight %}

Thus the given binary code is decoded to `(a d a b b c a)`. Looking at the given Huffman tree below, this makes sense.

<center><img src="/images/Ex2_67_Tree.svg" alt="Balanced binary tree" height="200"/></center>

### Exercise 2.68<a id="Exercise2_68">&nbsp;</a>

This exercise deal with the `encode` procedure which takes a message and a Huffman tree and evaluates to the list of bits that gives the encoded message. We are given the initial scaffolding:-

{% highlight scheme %}
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))
{% endhighlight %}

We are told to give the `encode-symbol` that takes one symbol and returns the bit representation of the symbol.

{% highlight scheme %}
(define (encode-symbol symbol tree)
  (define (contains-symbol? s symbols)
    (if (null? symbols)
        false
        (if (equal? s (car symbols))
            true
            (contains-symbol? s (cdr symbols)))))
  (cond ((leaf? tree) '())
        ((contains-symbol? symbol
                           (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains-symbol? symbol
                           (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol cannot be found" symbol))))
; encode-symbol

sample-message
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

(decode sample-message sample-tree)
; (a d a b b c a)

(encode '(a d a b b c a) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
{% endhighlight %}

As can be seen, the encode function works perfectly in re-encoding the decoded message.

### Exercise 2.69<a id="Exercise2_69">&nbsp;</a>

In this exercise, we are tasked with generating Huffman encoding tree, given a list of symbol-frequency pairs. We are given the following starting code:-

{% highlight scheme %}
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))
{% endhighlight %}

We initially create a set of leaves in ascending order of frequency using `make-leaf-set`. Then, we call `successive-merge` which we need to write.

{% highlight scheme %}
(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (let ((merged (make-code-tree (car pairs) (cadr pairs)))
            (remain (cddr pairs)))
        (successive-merge (adjoin-set merged remain)))))
; successive-merge

(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
; pairs

(generate-huffman-tree pairs)
; ((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)
{% endhighlight %}

### Exercise 2.70<a id="Exercise2_70">&nbsp;</a>

In this exercise, we exercise our new-found compression skills by encoding a song lyrics using Huffman encoding. We are given the frequencies of the most commonly used symbols in 1950s rock songs.

{% highlight scheme %}
(define song-words '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
; song-words

(define word-tree (generate-huffman-tree song-words))
; word-tree
word-tree
; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)

(define song '(Get a job
               Sha na na na na na na na na
               Get a job
               Sha na na na na na na na na
               Wah yip yip yip yip 
               yip yip yip yip yip
               Sha boom))
; song

(define compressed-song (encode song word-tree))
; compressed-song
compressed-song
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
{% endhighlight %}

We have our final encoded song. If encoded with a fixed-length code, we would require 3-bit to store all $$2^3=8$$ possible words.

{% highlight scheme %}
(length compressed-song)
; 84
(* 3 (length song))
; 108
{% endhighlight %}

As can be seen, with Huffman encoding, we have compressed the song by 24 bits (22% compression).

### Exercise 2.71<a id="Exercise2_71">&nbsp;</a>

In this exercise, we are told to examine the Huffman tree for an alphabet of *n*  symbols with frequencies of $$1,2,4,...,2^{n-1}$$.

{% highlight scheme %}
(define alphabet1 '((a 1) (b 2) (c 4) (d 8) (e 16)))
; alphabet1

(define tree1 (generate-huffman-tree alphabet1))
; tree1
tree1
; (((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31)

(encode-symbol 'a tree1)
; (0 0 0 0)
(encode-symbol 'e tree1)
; (1)

(define alphabet2 '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))
; alphabet2

(define tree2 (generate-huffman-tree alphabet2))
; tree2
tree2
; ((((((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31) (leaf f 32) (a b c d e f) 63) (leaf g 64) (a b c d e f g) 127) (leaf h 128) (a b c d e f g h) 255) (leaf i 256) (a b c d e f g h i) 511) (leaf j 512) (a b c d e f g h i j) 1023)

(encode-symbol 'a tree2)
; (0 0 0 0 0 0 0 0 0)
(encode-symbol 'j tree2)
; (1)
{% endhighlight %}

The following are the Huffman trees that were generated:-

<center>
<img src="/images/Ex2_71_Tree1.svg" alt="Tree of depth 5" height="200"/>
<img src="/images/Ex2_71_Tree2.svg" alt="Tree of depth 10" height="300"/>
</center>

We can see for both cases it takes 1 bit for the most frequent symbol and *n-1* bits to encode the least frequent symbol. Also, the Huffman tree looks similar and has the deepest configuration possible. This is because for any symbol with frequency *x*, the sum of frequencies for all the symbols with lower frequencies is *x-1*. This is due to the rule of taking frequencies as powers of two. 

### Exercise 2.72<a id="Exercise2_72">&nbsp;</a>

In this exercise we are asked to evaluate the order of growth in the number of steps required to evaluate a symbol. Since this is a difficult question to answer for a general Huffman tree, we are asked to give the order of growth for special cases where the Huffman trees look like the ones created in the previous exercise.

Here is the code we use for finding a symbol in a tree:-

{% highlight scheme %}
(define (encode-symbol symbol tree)
  (define (contains-symbol? s symbols)
    (if (null? symbols)
        false
        (if (equal? s (car symbols))
            true
            (contains-symbol? s (cdr symbols)))))
  (cond ((leaf? tree) '())
        ((contains-symbol? symbol
                           (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains-symbol? symbol
                           (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol cannot be found" symbol))))
{% endhighlight %}

In this code, `contains-symbol?` is similar in complexity to `memq`. For a list of *n* elements, it takes $$\Theta(n)$$ time complexity. Moving down to the next branch is a trivial operation that takes a constant time.

Now, when we try to encode the most frequent symbol in the Huffman tree, we simply run `contains-symbol` on two lists totalling *n* in length. The right branch is immediately identified and it being a leaf immediately returns the encoding. Thus, the encoding of the most frequent symbol has a $$\Theta(n)$$ growth.

Finally, considering the symbol with the least frequency, we first run `contains-symbol?` on the first node on a list with *n* entries. Unsuccessful, we go down the left branch and run the process again on a list with *n-1* entries. This goes on until the bottom where we run scans on *2* entries. $$n+(n-1)+...+2=(n+2)(n-1)/2\approx O(n^2)$$. Thus, encoding the letter of lowest frequency has a growth of $$\Theta(n^2)$$.
