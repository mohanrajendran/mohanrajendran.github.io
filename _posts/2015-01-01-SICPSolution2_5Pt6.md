---
layout: post
category: [SICP, Solutions]
post_no: 35
title: "SICP Section 2.5 Exercise Solutions - Part 6"
submenu:
   - { hook: "Exercise2_91", title: "Exercise 2.91" }
---
### Exercise 2.91<a name="Exercise2_91">&nbsp;</a>

In this exercise, we are tasked with implemeting polynomial division. For that purpose, we are given a skeleton code that we should complete.

<!--excerpt-->

{% highlight scheme %}
(define (install-core-polynomial-package-division)
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1) (car result))
                (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var: DIV-POLY"
               (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list L1 L1)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (make-dense-term-list ()) L1)
              (let ((new-c (div (coeff t1) 
                                (coeff t2)))
                    (new-o (- (order t1) 
                              (order t2))))
                (let ((rest-of-result
                        (div-terms
                          (add-terms L1
                                     (negate
                                       (mul-term-by-all-terms
                                         (make-term new-o new-c)
                                         L2)))
                           L2)))
                  (list (add-terms (make-dense-term-list
                                     (list (make-term new-o new-c)))
                                   (car rest-of-result))
                        (cadr rest-of-result))))))))

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  'done)
{% endhighlight %}

By default, we have set things up to use the dense representation.

{% highlight scheme %}
(define p1
  (make-polynomial 'x
                   (make-dense-term-list (list (make-term 5 1)
                                               (make-term 0 (- 1))))))
; p1
(define p2
  (make-polynomial 'x
                   (make-dense-term-list (list (make-term 2 1)
                                               (make-term 0 (- 1))))))
; p2

(div p1 p2)
; ((polynomial x dense (3 . 1) (1 . 1)) (polynomial x dense (1 . 1) (0 . -1)))
{% endhighlight %}

We get the right result given in the book.
