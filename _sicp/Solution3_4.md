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
  - { hook: "Exercise3_44", title: "Exercise 3.44" }
  - { hook: "Exercise3_45", title: "Exercise 3.45" }
  - { hook: "Exercise3_46", title: "Exercise 3.46" }
  - { hook: "Exercise3_47", title: "Exercise 3.47" }
  - { hook: "Exercise3_48", title: "Exercise 3.48" }
  - { hook: "Exercise3_49", title: "Exercise 3.49" }
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

Though, we should end with 20/30/10, we end up with 20/20/20. This is because by the time actual swap is conducted, the `difference` amount to swap would not be valid anymore. However, since the amount withdrawn and deposited in any step is equal and each sub-operation is serialized, the total amount of balance would still be maintained.

### Exercise 3.44<a id="Exercise3_44">&nbsp;</a>

In this exercise, we are given the following code to transfer amounts between accounts:-

{% highlight scheme %}
(define 
  (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
{% endhighlight %}

Ben Bitdiddle calins that the procedure works when many people transfer money concurrently and Louis Reasoner claims otherwise. I would agree with Ben. Since `withdraw` and `deposit` are serialized, there should not be any inconsistencies. Furthermore, the actual `amount` to transfer is held the same across withdrawal and deposit. This means that the intended effect is obtained.

A key difference between this and the previous exercise is that the value of `difference` migh be invalid before the transfer ever takes place. In this case, we are explicitly given the transfer amount. Thus no concurrency issues would arise.

### Exercise 3.45<a id="Exercise3_45">&nbsp;</a>

In this exercise, we are given a new version of `make-account-and-serializer` from Louis that serializes `withdraw` and `deposit` internally as well as exposes a serializer for use in transactions involving multiple accounts.

{% highlight scheme %}
(define 
  (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) 
             (balance-serializer withdraw))
            ((eq? m 'deposit) 
             (balance-serializer deposit))
            ((eq? m 'balance) 
             balance)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))
{% endhighlight %}

The issue with this code is that when a combined transaction needs to perform `deposit` or `withdraw`, it would be locked out since the overall transaction would already have acquired the locks. For example, let us look at the `serialized-exhange` code again:-

{% highlight scheme %}
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
{% endhighlight %}

In this case, since the overall call to `exchange` is serialized with `serializer1` and `serializer2`, the internal calls would not be able to call `withdraw` and `deposit` since they require the serializers again. Thus, the call would have a [deadlock](https://en.wikipedia.org/wiki/Deadlock).

### Exercise 3.46<a id="Exercise3_46">&nbsp;</a>

In this exercise, we are asked what happens if the given `test-and-set!` process is not run atomically.

{% highlight scheme %}
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
{% endhighlight %}

The main risk in the above code is when two processes concurrently try to obtain a lock and there is interleaving between test and set. Let us see how this happens. Assume processes $$P_1$$ and $$P_2$$ try to obtain a lock at the same time.

- $$P_1$$ calls `(car cell)` and recieves `#f`
- $$P_2$$ also calls `(car cell)` and recieves `#f`
- $$P_1$$ sets `cell` to `#t` and returns `#f`
- $$P_2$$ also sets `cell` to `#t` and returns `#f`

Since there is a break between when the cell is tested and when a lock is actually set, both processes acquire a lock. Thus, it is esssential that `test-and-set!` be atomic.

### Exercise 3.47<a id="Exercise3_47">&nbsp;</a>

In this exercise, we are tasked with implementing [semaphores](https://en.wikipedia.org/wiki/Semaphore_(programming)) of size *n*.

#### In terms of mutexes

To implement semaphores in terms of mutexes, we simply create a structure that maintains the number of processes that can still acquire the semaphore currently. Then, we can use a built-in `mutex` to test and set the count accordingly. If the count is 0, then the semaphore needs to re-acquire just like how we retry for mutex. The following code demonstrates this.

{% highlight scheme %}
(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (count n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> count 0)
                 (begin (set! count (- count 1))
                        (mutex 'release))
                 (begin (mutex 'release)
                        (the-semaphore 'acquire)))) ; retry
            ((eq? m 'release)
             (mutex 'acquire)
             (if (= count n)
                 (mutex 'release)
                 (begin (set! count (+ count 1))
                        (mutex 'release))))))
    the-semaphore))
{% endhighlight %}

#### In terms of `test-and-set!` opeerations

To implement semaphores in terms of `test-and-set!` operations, we can simply replace `mutex` above with `test-and-set!`. In this case, the retry part of mutex would also need to be implemented. The following code is the implementation.

{% highlight scheme %}
(define (make-semaphore n)
  (let ((cell (list false))
        (count n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire) ; retry
                 (if (> count 0)
                     (begin (set! count (- count 1))
                            (clear! cell))
                     (begin (clear! cell)
                            (the-semaphore 'acquire))))) ; retry
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release) ; retry
                 (if (= count n)
                     (clear! cell)
                     (begin (set! count (+ count 1))
                            (clear! cell)))))))
    the-semaphore))
{% endhighlight %}

### Exercise 3.48<a id="Exercise3_48">&nbsp;</a>

In this exercise, we are presented with a method for deadlock avoidance. The accounts are numbered and the locks are acquired in order of the account number. This avoids deadlock because all processes acquire locks in the same order. This prevents the case where two concurrent processes are waiting for the other process to release a lock to proceed. The process to obtain the first lock is able to acquire all other locks. The `serialized-exchange` incorporating this idea is as follows:-

{% highlight scheme %}
(define next-id 0)

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define id next-id)
  (set! next-id (+ next-id 1))
  (let ((balance-serializer 
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2)))
{% endhighlight %}

This problem is famously called the [Dining Philosophers problem](https://en.wikipedia.org/wiki/Dining_philosophers_problem) and this solution was proposed by Edgar Djikstra.

### Exercise 3.49<a id="Exercise3_49">&nbsp;</a>

One place where the solution presented would fail is when the resources required are not known in advance. An example would be in case of database mutations where we can have a complex query which needs to find out the row to mutate by reading another row first. In this case, the database cannot know in advance which locks to obtain. Thus, there might be a chance for deadlock to occur.