---
layout: post
category: [SICP, Solutions]
post_no: 18
title: "SICP Section 2.2 Exercise Solutions - Part 4"
submenu:
   - { hook: "Exercise2_40", title: "Exercise 2.40" }
   - { hook: "Exercise2_41", title: "Exercise 2.41" }
   - { hook: "Exercise2_42", title: "Exercise 2.42" }
   - { hook: "Exercise2_43", title: "Exercise 2.43" }
---

### Exercise 2.40<a name="Exercise2_40">&nbsp;</a>

In this exercise, we are told to define a procedure `unique-pairs` which takes an integer *n* and generates a sequence of pairs *(i,j)* where $$1\leq j<i\leq n$$. Basically, we are told to encapsulate the part that generates the pairs in the given `prime-sum-pairs` procedure in the book.

<!--excerpt-->

{% highlight scheme %}
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
; flatmap

(define (enumerate-interval i j)
  (if (> i j)
      nil
      (cons i (enumerate-interval (+ i 1) j))))
; enumerate-interval

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))
             ))
            (enumerate-interval 1 n)))
; unique-pairs

(unique-pairs 5)
; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))
{% endhighlight %}

We can thus simplify the `prime-sum-pairs` procedure as follows:-

{% highlight scheme %}
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
{% endhighlight %}

### Exercise 2.41<a name="Exercise2_41">&nbsp;</a>

In this exercise, we need to write a procedure that generates triples consisting of threee integers *i*, *j* and *k* less than or equal to a given number *n* that sums to a given integer *s*.

Drawing from the lessons learnt in this section, we start with smaller components and build the solution up. First, we build a `unique-triples` function that builds triples while using `unique-pairs` function.

{% highlight scheme %}
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))
; unique-triplets

(unique-triplets 5)
; ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))
{% endhighlight %}

Next, we do our normal filtering using a function.

{% highlight scheme %}
(define (makes-sum? triple s)
  (= (accumulate + 0 triple) s))
; makes-sum?

(define (triples-sum n s)
  (filter (lambda (triple) (makes-sum? triple s))
          (unique-triples n)))
; triples-sum

(triples-sum 20 10)
; ((5 3 2) (5 4 1) (6 3 1) (7 2 1))
{% endhighlight %}

### Exercise 2.42<a name="Exercise2_42">&nbsp;</a>

In this interesting exercise, we need to solve the *"eight-queens puzzle"*, where we try to place 8 queen pieces on a chess board in an arrangement that no queen is checking any other queen. A resursive solution is proposed which can generate all the solutions to the puzzle.

We are given the main code for solving this problem:-

{% highlight scheme %}
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
{% endhighlight %}

We are told to finish implementing the required data representations and sub-procedures for this problem.

##### Representation of positions

The first issue to be dealt with is creating a way to represent the position of a piece on the board. We can use our old knowledge to store the location as a *(row, col)* pair.

{% highlight scheme %}
(define (make-position col row)
  (cons col row))

(define (position-col position)
  (car position))

(define (position-row position)
  (cdr position))

(define (print-position position)
  (define colchar
    (integer->char
      (+ (char->integer #\`) (position-col position))))
  (display colchar)
  (display (position-row position)))
{% endhighlight %}

Further, we can represent the state of the board using a list of positions. Empty board can be represented as an empty list. Adding a new piece to the board is as simple as appending it.

{% highlight scheme %}
(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-position k new-row))))

(define (print-board board)
  (if (null? board)
      (display "EMPTY")
      (begin
        (display "(")
	    (print-position (car board))
	    (for-each (lambda (pos)
                    (begin
                      (display " ")
					  (print-position pos)))
                  (cdr board))
        (display ")")
		(newline))))
{% endhighlight %}

##### Safe positions

Every time we add a new queen piece to the board, we must check if the position is valid, ie, there is no queen checking the new piece. Checking only happens in two cases,

1. Both pieces are in the same row
2. Both pieces can be checked diagonally

If both pieces have the same column, we have no trouble.

We can create micro-procedures which check for this.

{% highlight scheme %}
(define (same-row pos1 pos2)
  (= (position-row pos1) (position-row pos2)))

(define (diag-check pos1 pos2)
  (= (abs (- (position-row pos1) (position-row pos2)))
     (abs (- (position-col pos1) (position-col pos2)))))

(define (same-piece pos1 pos2)
  (= (position-col pos1) (position-col pos2)))

(define (unsafe-pos? pos1 pos2)
  (if (same-piece pos1 pos2) #f
      (or (same-row pos1 pos2)
          (diag-check pos1 pos2))))
{% endhighlight %}

With that defined, let us look at defining the `safe?` function.

{% highlight scheme %}
(define (get-position k positions)
  (car (filter (lambda (pos) (= (position-col pos) k))
               positions)))

(define (safe? k positions)
  (define pos-k (get-position k positions))
  (define (safe-iter p)
    (if (null? p)
        #t
        (if (unsafe-pos? (car p) pos-k)
            #f
			(safe-iter (cdr p)))))
  (safe-iter positions))
{% endhighlight %}

##### Putting it all together

Now that all the components are set-up, we can run the code to see the solutions.

{% highlight scheme %}
(define (print-queens n)
  (for-each print-board (queens n)))
; print-queens

(print-queens 1)
; (a1)

(print-queens 4)
; (a2 b4 c1 d3)
; (a3 b1 c4 d2)

(print-queens 5)
; (a1 b3 c5 d2 e4)
; (a1 b4 c2 d5 e3)
; (a2 b4 c1 d3 e5)
; (a2 b5 c3 d1 e4)
; (a3 b1 c4 d2 e5)
; (a3 b5 c2 d4 e1)
; (a4 b1 c3 d5 e2)
; (a4 b2 c5 d3 e1)
; (a5 b2 c4 d1 e3)
; (a5 b3 c1 d4 e2)
{% endhighlight %}

As the board size increases, the number of solutions also increase. The exact length is given at OEIS [A000170](http://oeis.org/A000170). As can be seen, the first ten solutions areas follows:-

{% highlight scheme %}
(map (lambda (n)
       (cons n (length (queens n))))
     (enumerate-interval 1 10))

; ((1 . 1) (2 . 0) (3 . 0) (4 . 2) (5 . 10) (6 . 4) (7 . 40) (8 . 92) (9 . 352) (10 . 724))
{% endhighlight %}

This matches the number in the webpage.

### Exercise 2.43<a name="Exercise2_43">&nbsp;</a>

In this exercise, we are told to quantify how slowly Louis Reasoner's solution witll run compared to the original solution. The only difference is that the order of `(queen-cols (- k 1))` and `(enumerate-interval 1 board-size)` has been reversed. This means that each time the `flat-map` is called, `queen-cols` would be called `board-size` times instead of only once before.

In the original case, for `(queen-cols k)`, `(queen-cols (- k 1))` is only called once and work proportional to `board-size` is done on it. Thus, the overall time complexity is $$boardsize^2$$.

However in Louis' case, for `(queen-cols k)`, `(queen-cols (- k 1))` is called `board-size` times followed by work proportional to `board-size` done on it. Thus, the overall time complexity is $$boardsize^{boardsize}$$.

Taking the original time taken as *T*, the new time taken would be $$T^{\frac{boardsize}{2}}$$.
