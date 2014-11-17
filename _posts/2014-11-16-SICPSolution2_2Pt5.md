---
layout: post
category: [SICP, Solutions]
post_no: 19
title: "SICP Section 2.2 Exercise Solutions - Part 5"
submenu:
   - { hook: "Exercise2_44", title: "Exercise 2.44"}
   - { hook: "Exercise2_45", title: "Exercise 2.45" }
   - { hook: "Exercise2_46", title: "Exercise 2.46" }
   - { hook: "Exercise2_47", title: "Exercise 2.47" }
   - { hook: "Exercise2_48", title: "Exercise 2.48" }
   - { hook: "Exercise2_49", title: "Exercise 2.49" }
---

For this set of exercises, MIT-Scheme was proven insufficient due to the inability to draw pictures. Instead, [Racket](http://racket-lang.org/) was used along with the [SICP Picture Language](http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html) package.

This package can be invoked by typing `(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))` in the Racket REPL.

<!--excerpt-->

### Exercise 2.44<a name="Exercise2_44">&nbsp;</a>

In this exercise, we are tasked with implementing `up-split`, a function analogous to the `right-split` given in the book. It makes the painter split and branch upwards as follows when `(up-split painter n)` is called.

<center><img src="/images/Ex2_44_Diag.svg" alt="up-split n" /></center>

The code can be written as follows:-

{% highlight scheme %}
(define (up-split painter n)
  (if (= n 0)
      painter
	  (let ((smaller (up-split painter (- n 1))))
        (below painter
               (beside smaller smaller)))))
{% endhighlight %}

`(paint einstein)`
![](/images/Ex2_44P1.png)

`(paint (up-split einstein 1))`
![](/images/Ex2_44P2.png)

`(paint (up-split einstein 2))`
![](/images/Ex2_44P3.png)

### Exercise 2.45<a name="Exercise2_45">&nbsp;</a>

In this exercise, we are told to express both `right-split` and `up-split` as instances of a general splitting operation. It can be done as follows:-

{% highlight scheme %}
(define (split op1 op2)
  (define (splitter painter n)
    (if (= n 0)
        painter
	    (let ((smaller (splitter painter (- n 1))))
          (op1 painter
               (op2 smaller smaller)))))
  splitter)

(define right-split (split beside below))

(define up-split (split below beside))
{% endhighlight %}

`(paint (up-split einstein 2))`
![](/images/Ex2_45P1.png)

`(paint (right-split einstein 2))`
![](/images/Ex2_45P2.png)

### Exercise 2.46<a name="Exercise2_46">&nbsp;</a>

In this exercise, we are told to simply create a data structure holding a 2D vector. We follow our templates as usual.

{% highlight scheme %}
(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))
{% endhighlight %}

Now, to implement the other procedures:-

{% highlight scheme %}
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
{% endhighlight %}

Let us test them to be sure.

{% highlight scheme %}
(define u (make-vect 1 5))
(define v (make-vect 2 7))

u
; (1 . 5)
v
; (2 . 7)
(add-vect u v)
; (3 . 12)
(sub-vect u v)
; (-1 . -2)
(scale-vect 5 u)
; (5 . 25)
{% endhighlight %}

### Exercise 2.47<a name="Exercise2_47">&nbsp;</a>

In this exercise, we are given two constructors for frames. With the two constructors, we are needed to provide appropriate selectors for each representation.

{% highlight scheme %}
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define f (make-frame (make-vect 1 1) (make-vect 2 1) (make-vect 1 2)))

f
; ((1 . 1) (2 . 1) (1 . 2))
(origin-frame f)
; (1 . 1)
(edge1-frame f)
; (2 . 1)
(edge2-frame f)
; (1 . 2)
{% endhighlight %}

Let's look at the next case.

{% highlight scheme %}
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define f (make-frame (make-vect 1 1) (make-vect 2 1) (make-vect 1 2)))

f
; ((1 . 1) (2 . 1) 1 . 2)
(origin-frame f)
; (1 . 1)
(edge1-frame f)
; (2 . 1)
(edge2-frame f)
; (1 . 2)
{% endhighlight %}

### Exercise 2.48<a name="Exercise2_48">&nbsp;</a>

In this exercise, we are tasked with creating a data structure for representing a segment.

{% highlight scheme %}
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
{% endhighlight %}

### Exercise 2.49<a name="Exercise2_49">&nbsp;</a>

In this exercise, we are told to use the `segments->painter` function to create shapes.

##### Outline

The outline of the designated frame can be drawn using four segments. *(0,0) to (1,0)*, *(1,0) to (1,1)*, *(1,1) to (0,1)* and *(0,1) to (0,0)*.

{% highlight scheme %}
(define outline
   (segments->painter
    (list 
     (make-segment
      (make-vect 0 0)
      (make-vect 0.99 0))
     (make-segment
      (make-vect 0.99 0)
      (make-vect 0.99 0.99))
     (make-segment
      (make-vect 0.99 0.99)
      (make-vect 0 0.99))
     (make-segment
      (make-vect 0 0.99)
      (make-vect 0 0)))))
{% endhighlight %}

`(paint outline)`
![](/images/Ex2_49P1.png)

Note 0.99 was used instead of 1 so that lines show up in the image.

##### Cross

The cross can be drawn using just two segments. *(0,0) to (1,1)* and *(1,0) to (0,1)*.

{% highlight scheme %}
(define cross
   (segments->painter
    (list
     (make-segment
      (make-vect 0 0)
      (make-vect 1 1))
     (make-segment
      (make-vect 1 0)
      (make-vect 0 1)))))
{% endhighlight %}

`(paint cross)`
![](/images/Ex2_49P2.png)

##### Diamond

The diamond can be constructed as follows:-

{% highlight scheme %}
(define diamond
   (segments->painter
    (list
     (make-segment
      (make-vect 0 0.5)
      (make-vect 0.5 0))
     (make-segment
      (make-vect 0.5 0)
      (make-vect 1 0.5))
     (make-segment
      (make-vect 1 0.5)
      (make-vect 0.5 1))
     (make-segment
      (make-vect 0.5 1)
      (make-vect 0 0.5)))))
{% endhighlight %}

`(paint diamond)`
![](/images/Ex2_49P3.png)

##### Wave

Drawing the wave takes a bit longer. The coordinates are borrowed from [Bill the lizard](http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html)

{% highlight scheme %}
(define wave
   (segments->painter
    (list
     (make-segment
      (make-vect 0.006 0.840)
      (make-vect 0.155 0.591))
     (make-segment
      (make-vect 0.006 0.635)
      (make-vect 0.155 0.392))
     (make-segment
      (make-vect 0.304 0.646)
      (make-vect 0.155 0.591))
     (make-segment
      (make-vect 0.298 0.591)
      (make-vect 0.155 0.392))
     (make-segment
      (make-vect 0.304 0.646)
      (make-vect 0.403 0.646))
     (make-segment
      (make-vect 0.298 0.591)
      (make-vect 0.354 0.492))
     (make-segment
      (make-vect 0.403 0.646)
      (make-vect 0.348 0.845))
     (make-segment
      (make-vect 0.354 0.492)
      (make-vect 0.249 0.000))
     (make-segment
      (make-vect 0.403 0.000)
      (make-vect 0.502 0.293))
     (make-segment
      (make-vect 0.502 0.293)
      (make-vect 0.602 0.000))
     (make-segment
      (make-vect 0.348 0.845)
      (make-vect 0.403 0.999))
     (make-segment
      (make-vect 0.602 0.999)
      (make-vect 0.652 0.845))
     (make-segment
      (make-vect 0.652 0.845)
      (make-vect 0.602 0.646))
     (make-segment
      (make-vect 0.602 0.646)
      (make-vect 0.751 0.646))
     (make-segment
      (make-vect 0.751 0.646)
      (make-vect 0.999 0.343))
     (make-segment
      (make-vect 0.751 0.000)
      (make-vect 0.597 0.442))
     (make-segment
      (make-vect 0.597 0.442)
      (make-vect 0.999 0.144)))))
{% endhighlight %}

`(paint wave)`
![](/images/Ex2_49P4.png)
