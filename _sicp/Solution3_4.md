---
layout: spiedpage
order: 24
title: Section 3.4 solutions
exercises: '3.38'
submenu:
  - { hook: "Exercise3_38", title: "Exercise 3.38" }
---

### Exercise 3.38<a id="Exercise3_38">&nbsp;</a>

In this exercise, we are given three events acting on an initial `balance` of 100:-

1) Peter: `(set! balance (+ balance 10))`
2) Paul: `(set! balance (- balance 20))`
3) Mary: `(set! balance (- balance (/ balance 2)))`

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

The above ordering can produce a final value of 110. These examples show us the importance of concurrency management.