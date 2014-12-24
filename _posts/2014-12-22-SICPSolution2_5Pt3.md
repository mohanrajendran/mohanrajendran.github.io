---
layout: post
category: [SICP, Solutions]
post_no: 32
title: "SICP Section 2.5 Exercise Solutions - Part 3"
submenu:
   - { hook: "Exercise2_84", title: "Exercise 2.84" }
   - { hook: "Exercise2_85", title: "Exercise 2.85" }
   - { hook: "Exercise2_86", title: "Exercise 2.86" }
---
### Exercise 2.84<a name="Exercise2_84">&nbsp;</a>

This exercise tasks us with using the `raise` operation defined in the previous exercise to modify the `apply-generic` procedure to work on arguments of multiple type. We can do that by testing which of the provided types is higher up in the tower of type hierarchy and raising the other to that type.

<!--excerpt-->

Further, we are required to set-up code such that future modifications are easy. We assume that the types follow a tower hierarchy and not a more complex relationship like the polygons. Let us first define how we can define the type hierarchy

{% highlight scheme %}
(define (put-level type level)
  (hash-table/put! dispatch-table (list 'level type) level))

(define (get-level type)
  (hash-table/get dispatch-table (list 'level type) (error "Type tag not found" type)))

(define (install-arithmetic-package-hierarchy)
  ;; Tower definition and extraction
  (define tower '(integer rational scheme-number complex))
  (define (tower-level type)
    (define (get-level-iter types level)
      (if (null? types)
          (error "Type not found" type)
          (if (eq? type (car types))
              level
              (get-level-iter (cdr types) (+ level 1)))))
    (get-level-iter tower 0))

  ;; Level definition
  (put-level 'integer (tower-level 'integer))
  (put-level 'rational (tower-level 'rational))
  (put-level 'scheme-number (tower-level 'scheme-number))
  (put-level 'complex (tower-level 'complex))
  'done)
; install-arithmetic-package-hierarchy

(install-arithmetic-package-hierarchy)
; done

(get-level 'integer)
; 0
(get-level 'rational)
; 1
(get-level 'scheme-number)
; 2
(get-level 'complex)
; 3
{% endhighlight %}

Thus, when we add new types, we only need to modify the tower and add the `'level` definition to the dispatch table. Now, we can modify the `apply-generic` function.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
; apply-generic

(add (make-rational 1 3)
     (make-scheme-number 1))
; (scheme-number . 1.3333333333333333)
(add (make-rational 1 2)
     (make-complex-from-real-imag 5 6))
; (complex rectangular 5.5 . 6)
{% endhighlight %}

### Exercise 2.85<a name="Exercise2_85">&nbsp;</a>

In this exercise, analogous to the `raise` operation that we have worked with, we are tasked with implementing a `drop` operation that can be used to push the data to the simplest level in the hierarchy. For that purpose, we create a function called `project` that pushes the data down by throwing away data. When we raise it again, if it retains the same value before projecting, we can safely use the lower level for storage.

Let use first define the `project` procedure:-
{% highlight scheme %}
(define (install-arithmetic-package-project)
  (put 'project '(complex)
       (lambda (z) (make-scheme-number (real-part z))))
  (put 'project '(scheme-number)
       (lambda (n)
         (let ((r (rationalize (inexact->exact n) 1/10000)))
           (make-rational (numerator r) (denominator r)))))
  (put 'project '(rational)
       (lambda (r) (make-integer (/ (numer r) (denom r)))))
  'done)
; install-arithmetic-package-project
(define (project x)
  (apply-generic 'project x))
; project

(install-arithmetic-package-project)
; done

(project (make-complex-from-real-imag 5.5 6))
; (scheme-number . 5.5)
(project (make-scheme-number 5.5))
; (rational 11 . 2)
(project (make-rational 11 2))
; (integer . 6)
{% endhighlight %}

Now that we have successfully implemented project, let us implement the `drop` procedure.

{% highlight scheme %}
(define (drop x)
  (if (= (get-level (type-tag x)) 0)
      x
      (if (equ? (raise (project x)) x)
          (drop (project x))
          x)))
; drop

(drop (make-complex-from-real-imag 2 3))
; (complex rectangular 2 . 3)
(drop (make-complex-from-real-imag 1.5 0))
; (rational 3 . 2)
(drop (make-complex-from-real-imag 1 0))
; (integer . 1)
{% endhighlight %}

As can be seen, drop works perfectly. Now, let us integrate it into the `apply-generic` procedure.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (define (drop-type ans)
    (if (or (eq? op 'add)
            (eq? op 'sub)
            (eq? op 'mul)
            (eq? op 'div))
        (drop ans)
        ans))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-type (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
; apply-generic

(add (make-complex-from-real-imag 1.5 0)
     (make-rational 3 2))
; (integer . 3)
(add (make-complex-from-real-imag 1.5 2.3)
     (make-rational 3 2))
; (complex rectangular 3. . 2.3)
(add (make-complex-from-real-imag 1.5 2)
     (make-complex-from-real-imag 3 (- 2)))
;Value 90: (rational 9 . 2)
{% endhighlight %}

### Exercise 2.86<a name="Exercise2_86">&nbsp;</a>

In this exercise, we are told to modify complex numbers so that their components can be of any numerical type that we desire in the system. Looking at the code for computing these values, we have are required to implement these new functions for the components:- `sin`, `cos`, `atan`, `square` and `sqrt`. We implement them for the rational and real numbers and modify the packages to use generic functions.

{% highlight scheme %}
(define (install-arithmetic-package-sine)
  (put 'sine '(scheme-number)
    (lambda (s) (make-scheme-number (sin s))))
  (put 'sine '(rational)
    (lambda (r) (sine (raise (attach-tag 'rational r)))))
  'done)
(define (sine x)
  (apply-generic 'sine x))

(define (install-arithmetic-package-cosine)
  (put 'cosine '(scheme-number)
    (lambda (s) (make-scheme-number (cos s))))
  (put 'cosine '(rational)
    (lambda (r) (cosine (raise (attach-tag 'rational r)))))
  'done)
(define (cosine x)
  (apply-generic 'cosine x))

(define (install-arithmetic-package-arctan)
  (put 'arctan '(scheme-number scheme-number)
    (lambda (s1 s2) (make-scheme-number (atan s1 s2))))
  (put 'arctan '(rational)
    (lambda (r1 r2) (arctan (raise (attach-tag 'rational r1))
                            (raise (attach-tag 'rational r2)))))
  'done)
(define (arctan x y)
  (apply-generic 'arctan x y))

(define (install-arithmetic-package-squared)
  (put 'squared '(scheme-number)
    (lambda (s) (make-scheme-number (square s))))
  (put 'squared '(rational)
    (lambda (r) (squared (raise (attach-tag 'rational r)))))
  'done)
(define (squared x)
  (apply-generic 'squared x))

(define (install-arithmetic-package-squareroot)
  (put 'squareroot '(scheme-number)
    (lambda (s) (make-scheme-number (sqrt s))))
  (put 'squareroot '(rational)
    (lambda (r) (squareroot (raise (attach-tag 'rational r)))))
  'done)
(define (squareroot x)
  (apply-generic 'squareroot x))

(install-arithmetic-package-sine)
(install-arithmetic-package-cosine)
(install-arithmetic-package-arctan)
(install-arithmetic-package-squared)
(install-arithmetic-package-squareroot)
{% endhighlight %}

Once we have defined these, we can rewrite the complex number packages. Note we have used different names so as not to overwrite default functions.

{% highlight scheme %}
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (squareroot (add (squared (real-part z))
                     (squared (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (squareroot (add (squared x) (squared y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)
{% endhighlight %}

We also modify `apply-generic` to drop type when these functions are called.

{% highlight scheme %}
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (if (> (get-level type1) (get-level type2))
        type1
        type2))
  (define (raise-to arg t-type)
    (if (eq? (type-tag arg) t-type)
        arg
        (raise-to (raise arg) t-type)))
  (define (drop-type ans)
    (if (or (eq? op 'add)
            (eq? op 'sub)
            (eq? op 'mul)
            (eq? op 'div)
            (eq? op 'sine)
            (eq? op 'cosine)
            (eq? op 'arctan)
            (eq? op 'squared)
            (eq? op 'squareroot))
        (drop ans)
        ans))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-type (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((target-type (higher-type type1 type2)))
                      (apply-generic op
                                     (raise-to a1 target-type)
                                     (raise-to a2 target-type)))))
              (error 
               "No method for these types"
               (list op type-tags)))))))
{% endhighlight %}

Let us test the code:-

{% highlight scheme %}
(define y (make-complex-from-mag-ang (make-scheme-number 5.6) (make-rational 3 4)))
; y
y
; (complex polar (scheme-number . 5.6) rational 3 . 4)
(real-part y)
; (scheme-number . 4.097457665693397)
(imag-part y)
; (scheme-number . 3.817177056130671)

(define z (make-complex-from-real-imag (make-scheme-number 5.6) (make-rational 3 4)))
; z
z
; (complex rectangular (scheme-number . 5.6) rational 3 . 4)
(magnitude z)
; (scheme-number . 5.6499999999999995)
(angle z)
; (scheme-number . .13313632755164762)
{% endhighlight %}

We now get answers of the right type.
