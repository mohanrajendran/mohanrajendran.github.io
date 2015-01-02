---
layout: post
category: [SICP, Solutions]
post_no: 34
title: "SICP Section 2.5 Exercise Solutions - Part 5"
submenu:
   - { hook: "Exercise2_90", title: "Exercise 2.90" }
---
### Exercise 2.90<a name="Exercise2_90">&nbsp;</a>

In this exercise, we are tasked with implementing a polynomial system that can work with both dense and sparse representation simultaneously. We can coerce between each representation as needed. The coefficients can either be a number or a polynomial.

Note:- Since our old number tower is beyond the scope of upcoming exercises, we stick with the simple default numbers provided in MIT-Scheme so as to keep things simple. There will be no `drop` or `raise` being done. Only direct coercion. Full capability can be achieved but required too much testing corner cases to be use for this exercise.

<!--excerpt-->

{% highlight scheme %}
;; Term representation. Common for dense and sparse
(define (install-terms-package)
  ;; Internal procedures
  (define (term-tag x)
    (attach-tag 'term x))
  (define (make-term order coeff) 
    (cons order coeff))
  (define (order term) (car term))
  (define (coeff term) (cdr term))
  ;; External interface
  (put 'order '(term)
       (lambda (t)
	     (order t)))
  (put 'coeff '(term)
       (lambda (t)
	     (coeff t)))
  (put 'make 'term
       (lambda (order coeff)
	     (term-tag (make-term order coeff))))
  'done)
(install-terms-package)
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))

;; Generic function definitions
(define (adjoin-term x y) (apply-generic 'adjoin-term x y))
(define (first-term x) (apply-generic 'first-term x))
(define (rest-terms x) (apply-generic 'rest-terms x))
(define (empty-termlist? x) (apply-generic 'empty-termlist? x))

(define (install-polynomial-dense-package)
  ;; Internal procedures
  (define (dense-tag x)
    (attach-tag 'dense x))
  (define (term-tag x)
    (attach-tag 'term x))
  (define (tcoeff term)
    (coeff (term-tag term)))
  (define (torder term)
    (order (term-tag term)))

  (define (dense-adjoin-term term term-list)
    (if (=zero? (tcoeff term))
        term-list
        (cons term term-list)))
  (define (dense-first-term term-list) (car term-list))
  (define (dense-rest-terms term-list) (cdr term-list))
  (define (dense-empty-termlist? term-list)
    (null? term-list))
	
  (define dense-empty-term-list (dense-tag '()))
  (define (dense-make-term-list list-of-terms)
	(if (null? list-of-terms)
	    dense-empty-term-list
		(adjoin-term (car list-of-terms)
		             (dense-make-term-list (cdr list-of-terms)))))

  (define (dense-term-list-negate term-list)
    (if (null? term-list)
	    ()
		(let ((ft (car term-list)))
		      (rt (cdr term-list)))
		  (cons
		    (contents (make-term (torder ft) (negate (tcoeff ft))))
		    (dense-term-list-negate rt)))))

  ;; External interface
  (put 'adjoin-term '(term dense)
       (lambda (t tl)
	     (dense-tag (dense-adjoin-term t tl))))
  (put 'first-term '(dense)
       (lambda (tl)
	     (term-tag (dense-first-term tl))))
  (put 'rest-terms '(dense)
       (lambda (tl)
	     (dense-tag (dense-rest-terms tl))))
  (put 'empty-termlist? '(dense)
       dense-empty-termlist?)
  (put '=zero? '(dense)
       dense-empty-termlist?)
  (put 'negate '(dense)
       (lambda (t1)
	     (dense-tag (dense-term-list-negate t1))))
  (put 'make 'dense
       (lambda (terms)
	      (dense-make-term-list terms)))
  'done)
(install-polynomial-dense-package)
(define (make-dense-term-list terms-list)
  ((get 'make 'dense) terms-list))

(define (install-polynomial-sparse-package)
  ;; Internal procedures
  (define (sparse-tag x)
    (attach-tag 'sparse x))
  (define (term-tag x)
    (attach-tag 'term x))
  (define (tcoeff term)
    (coeff (term-tag term)))
  (define (torder term)
    (order (term-tag term)))

  ;; Internal representation
  (define (make-term-list starting-order coefficients)
    (cons starting-order coefficients))
  (define (starting-order term-list)
    (car term-list))
  (define (coefficients term-list)
    (cdr term-list))

  (define (sparse-empty-termlist? term-list)
    (null? (coefficients term-list)))

  (define (strip-leading-zeros term-list)
    (if (or (sparse-empty-termlist? term-list)
            (not (=zero? (car (coefficients term-list)))))
  	    term-list
  	    (strip-leading-zeros
	      (make-term-list (- (starting-order term-list) 1)
		                  (cdr (coefficients term-list))))))
						  
  (define (sparse-adjoin-term term term-list)
  (if (=zero? (tcoeff term))
      term-list
	  (if (sparse-empty-termlist? term-list)
	      (make-term-list (torder term)
		                  (list (tcoeff term)))
	      (if (= (- (torder term) 1)
	             (starting-order term-list))
		      (make-term-list
			    (torder term)
		        (cons (tcoeff term)
				      (coefficients term-list)))
	          (adjoin-term
			    term
		        (make-term-list (+ (starting-order term-list) 1)
				                   (cons 0 (coefficients term-list))))))))

  (define sparse-empty-term-list
    (sparse-tag (make-term-list (- 1) ())))
  (define (sparse-make-term-list list-of-terms)
	(if (null? list-of-terms)
	    sparse-empty-term-list
		(adjoin-term (car list-of-terms)
		             (sparse-make-term-list (cdr list-of-terms)))))


  (define (sparse-first-term term-list)
    (make-term (starting-order term-list)
               (car (coefficients term-list))))
  (define (sparse-rest-terms term-list)
    (strip-leading-zeros
      (make-term-list (- (starting-order term-list) 1)
	                  (cdr (coefficients term-list)))))

  (define (sparse-term-list-negate t)
  (if (sparse-empty-termlist? t)
      t
      (make-term-list (starting-order t)
	                  (map negate (coefficients t)))))

  ;; External interface
  (put 'adjoin-term '(term sparse)
       (lambda (t tl)
	     (sparse-tag (sparse-adjoin-term t tl))))
  (put 'first-term '(sparse)
       sparse-first-term)
  (put 'rest-terms '(sparse)
       (lambda (tl)
	     (sparse-tag (sparse-rest-terms tl))))
  (put 'empty-termlist? '(sparse)
       sparse-empty-termlist?)
  (put '=zero? '(sparse)
       sparse-empty-termlist?)
  (put 'negate '(sparse)
       (lambda (t1)
	     (sparse-tag (sparse-term-list-negate t1))))
  (put 'make 'sparse
       (lambda (terms)
	      (sparse-make-term-list terms)))
  'done)
(install-polynomial-sparse-package)
(define (make-sparse-term-list terms-list)
  ((get 'make 'sparse) terms-list))

(define (install-polynomial-core-package)
  ;; Internal functions
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
          (make-poly 
           (variable p1)
           (mul-terms (term-list p1)
                      (term-list p2)))
          (error "Polys not in same var: MUL-POLY"
                 (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) 
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))

  (define (sub-poly p1 p2)
    (add (tag p1) (negate (tag p2))))

  ;; External interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       sub-poly)
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero? (term-list p))))
  (put 'negate '(polynomial)
       (lambda (p) (make-polynomial
	                 (variable p)
                     (negate (term-list p)))))
  'done)
(install-polynomial-core-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
{% endhighlight %}

Let us test it.

{% highlight scheme %}
(define p1 (make-polynomial 'x (make-dense-term-list (list (make-term 2 5) (make-term 1 4) (make-term 0 3)))))
; p1
(define p2 (make-polynomial 'x (make-sparse-term-list (list (make-term 2 5) (make-term 1 4) (make-term 0 3)))))
; p2

p1
; (polynomial x dense (2 . 5) (1 . 4) (0 . 3))
p2
; (polynomial x sparse 2 5 4 3)

(add p1 p2)
; (polynomial x sparse 2 10 8 6)
(add p2 p1)
; (polynomial x dense (2 . 10) (1 . 8) (0 . 6))

(mul p1 p2)
; (polynomial x sparse 4 25 40 46 24 9)
(mul p2 p1)
; (polynomial x dense (4 . 25) (3 . 40) (2 . 46) (1 . 24) (0 . 9))

(sub p1 p2)
; (polynomial x sparse -1)
(sub p2 p1)
; (polynomial x dense)
{% endhighlight %}

As can be seen, we can use both representations interchangably because of the similar interface they expose. Taking terms from them emits the same data structure, so coercion is not even needed. The result of mixing is of the type of the first argument.

