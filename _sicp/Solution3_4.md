---
layout: spiedpage
order: 24
title: Section 3.4 solutions
exercises: '3.38 - 3.42'
submenu:
  - { hook: "Exercise3_38", title: "Exercise 3.38" }
  - { hook: "Exercise3_39", title: "Exercise 3.39" }
  - { hook: "Exercise3_40", title: "Exercise 3.40" }
  - { hook: "Exercise3_41", title: "Exercise 3.41" }
  - { hook: "Exercise3_42", title: "Exercise 3.42" }
  - { hook: "Exercise3_43", title: "Exercise 3.43" }
---

### Exercise 3.38<a id="Exercise3_38">&nbsp;</a>

In this exercise, we are given three events acting on an initial `balance` of 100:-

1. Peter: `(set! balance (+ balance 10))`
2. Paul: `(set! balance (- balance 20))`
3. Mary: `(set! balance (- balance (/ balance 2)))`

When these events might be run sequntally in an arbitrary order, the following values of `balance` might be produced:-

- Peter, Paul, Mary :- 100 -> 110 -> 90 -> 45
- Peter, Mary, Paul :- 100 -> 110 -> 55 -> 35
- Paul, Peter, Mary :- 100 -> 80 -> 90 -> 45
- Paul, Mary, Peter :- 100 -> 80 -> 40 -> 50
- Mary, Peter, Paul :- 100 -> 50 -> 60 -> 40
- Mary, Paul, Pater :- 100 -> 50 -> 30 -> 40

If the individual processes can be interleaved, many different values can be produced.

![Possibility 1](/images/Ex3_38_Part1.svg)

The above ordering can produce a final value of 80.

![Possibility 2](/images/Ex3_38_Part2.svg)
-
The above ordering can produce a final value of 110. These examples show us the importance of concurrency management.

### Exercise 3.39<a id="Exercise3_39">&nbsp;</a>

In this exercise, we are given the following serialized code:-

{% highlight scheme %}
(define x 10)
(define s (make-serializer))
(parallel-execute 
  (lambda () 
    (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
{% endhighlight %}

We are asked what the possible values of `x` would be once the execution is complete. We can see there are three operations:-

1. `((s (lambda () (* x x))))`
2. `(set! x ...)`
3. `(s (lambda () (set! x (+ x 1))))`

Since step **2** is dependant of **1** being complete, and **2** can interleave with **3**, we get the following permutations and results:-

* Steps 1, 2, 3 in order :- 10 -> 100 -> 101
* Steps 3, 1, 2 in order :- 10 -> 11 -> 121
* Steps 1, 3, 2 in order :- 10 -> 11 -> 100
* Step 1 first then Step 3 with Step 2 interleaved :- 10 -> 100 -> 11

### Exercise 3.40<a id="Exercise3_40">&nbsp;</a>

In this exercise, we are asked for possible values when the following code is evaluated:-

{% highlight scheme %}
(define x 10)
(parallel-execute 
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))
{% endhighlight %}

* 100 :- $$P_1$$ calculates 100, $$P_2$$ calculates 1000 and stores, $$P_1$$ stores 100
* 1000 :- $$P_2$$ calculates 1000, $$P_1$$ calculates 100 and stores, $$P_2$$ stores 1000
* 10000 :- $$P_1$$ reads first x as 10, $$P_2$$ calculates 1000 and stores, $$P_1$$ reads second x as 1000 and stores 10000
* 100000 :- $$P_2$$ reads first x as 10, $$P_1$$ calculates 100 and stores, $$P_2$$ reads second and third x as 100 and stores 100000
* 1000000 :- $$P_1$$ calculates 100 and stores, $$P_2$$ calculates 1000000 and stores

However, if we uses serialized procedures as follows:-

{% highlight scheme %}
(define x 10)
(define s (make-serializer))
(parallel-execute 
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (* x x x)))))
{% endhighlight %}

We can have $$P_1$$ and $$P_2$$ getting run atomically. Since the operation is commutative, we get 1000000 in both cases.

### Exercise 3.41<a id="Exercise3_41">&nbsp;</a>

In this exercise, Ben Bitdiddle replaces access to balance from `balance` to `(protected (lambda () balance))`. I think that this replacement is not required. This is mainly because accessing `balance` is a single operation. Thus we will not get any other operation interleaving with this access. Furthermore, mutations to `balance` is done using serialized methods and that ensures safe and consistent operations. Only place where interference might occur is when one tries to access `balance` in the middle of `withdraw` and `deposit` operations. This technically is not a faulty operation since we only consider the data to be mutated once these operations are over.

### Exercise 3.42<a id="Exercise3_42">&nbsp;</a>

In this exercise, we have another change by Ben Bitdiddle, he changes `make-account` so that `dispatch` returns pre-protected functions instead of protecting function each time its accessed. This is a valid change since protection only comes into play when executing. When the protection is applied is immaterial.

### Exercise 3.43<a id="Exercise3_43">&nbsp;</a>

In this exercise, we talk about exchanging balances between three accounts. Let us use the following code as an example:-

{% highlight scheme %}
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define a (make-account 10))
(define b (make-account 20))
(define c (make-account 30))

(parallel-execute
  (exchange a b)
  (exchange b c))
{% endhighlight %}

If we use the given code as follows and replace `exchange` above with `serialized-exchange` below:-

{% highlight scheme %}
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
{% endhighlight %}

we preserve the right balances because all involved accounts are serialized for the duration of exchange. No other process involving either account can be run while `serialized-exchange` being run. However, if we use the original `exchange` function, we obviously will get errors because execution can be interleaved between when a balance is computed and when balance is swapped. Let us demonstrate this by using the following diagram:-

![Exchange race condition](/images/Ex3_43.svg)

Though, we should end with 20/30/10, we end up with 20/20/20. This is because by the time actual swap is conducted, the amount to swap would not be valid anymore. However, since the amount withdrawn and deposited in any step is equal and each sub-operation is serialized, the total amount of balance would still be maintained.