---
layout: post
category: [SICP, Solutions]
post_no: 24
title: "SICP Section 2.3 Exercise Solutions - Part 3"
submenu:
   - { hook: "Exercise2_61", title: "Exercise 2.61" }
   - { hook: "Exercise2_62", title: "Exercise 2.62" }
   - { hook: "Exercise2_63", title: "Exercise 2.63" }
   - { hook: "Exercise2_64", title: "Exercise 2.64" }
   - { hook: "Exercise2_65", title: "Exercise 2.65" }
---
### Exercise 2.61<a name="Exercise2_61">&nbsp;</a>

In this exercise, we look at creating an implementation of `adjoin-set` for sets represented using ordered lists.

<!--excerpt-->

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

### Exercise 2.62<a name="Exercise2_62">&nbsp;</a>

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

### Exercise 2.63<a name="Exercise2_63">&nbsp;</a>

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

### Exercise 2.64<a name="Exercise2_64">&nbsp;</a>

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

### Exercise 2.65<a name="Exercise2_65">&nbsp;</a>

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
