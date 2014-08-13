---
layout: post
category: [SICP, Solutions]
post_no: 5
title: "SICP Section 1.2 Exercise Solutions - Part 2"
submenu:
  - { hook: "Exercise1_20", title: "Exercise 1.20" }
---

### Exercise 1.20<a name="Exercise1_20">&nbsp;</a>

In this exercise, we revisit the applicative and normal-order evaluation strategies presented in [Section 1.1]({% post_url 2014-07-27-SICPSection1_1 %}) to trace the given function and determine how many times `remainder` is called.

{% highlight scheme %}
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
{% endhighlight %}

#### Normal-order evaluation

In normal-order evaluation, we fully expand first before evaluating.

{% highlight scheme %}
(gcd 206 40)

(if (= 40 0)
    206
	(gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

; 1 evaluation (remainder 206 40) => 6
(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
	
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))))

; 2 evaluations (remainder 40 (remainder 206 40))
;            => (remainder 40 6) => 4
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))

(if (= (remainder (remainder 206 40)
                  (remainder 40 (remainder 206 40))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

; 4 evaluations (remainder (remainder 206 40)
;                          (remainder 40 (remainder 206 40)))
;            => (remainder 6 4) => 2
(if (= 2 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

; 7 evaluations (remainder (remainder 40 (remainder 206 40))
;                          (remainder (remainder 206 40)
;                                     (remainder 40 (remainder 206 40))))
;            => (remainder 4 2) => 0
(if (= 0 0) 
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206
                                                                40)))))))

(remainder (remainder 206 40)
           (remainder 40 (remainder 206 40)))

; 4 evaluations
2

{% endhighlight %}

As can be seen, the overly long evaluation chain evaluates `remainder` 18 times.

#### Applicative-order evaluation

In applicative order evaluation, we evaluate available sub-expressions before expanding.

{% highlight scheme %}
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

; 1 evaluation
(gcd 40 6)

(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6))

; 1 evaluation
(gcd 6 4)

(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4))

; 1 evaluation
(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

(gcd 2 (remainder 4 2))

; 1 evaluation
(gcd 2 0)

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

2
{% endhighlight %}

As can be seen, `remainder` is only called 4 times in the applicative order.
