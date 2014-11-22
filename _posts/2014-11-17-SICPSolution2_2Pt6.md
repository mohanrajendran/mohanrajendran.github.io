---
layout: post
category: [SICP, Solutions]
post_no: 20
title: "SICP Section 2.2 Exercise Solutions - Part 6"
submenu:
   - { hook: "Exercise2_50", title: "Exercise 2.50" }
   - { hook: "Exercise2_51", title: "Exercise 2.51" }
   - { hook: "Exercise2_52", title: "Exercise 2.52" }
---

For this set of exercises, MIT-Scheme was proven insufficient due to the inability to draw pictures. Instead, [Racket](http://racket-lang.org/) was used along with the [SICP Picture Language](http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html) package.

This package can be invoked by typing `(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))` in the Racket REPL.

<!--excerpt-->

### Exercise 2.50<a name="Exercise2_50">&nbsp;</a>

In this exercise, we are tasked with defining the transformations to flip the painters horizontally, rotate them counterclockwise by 180 degrees and 270 degrees.

Let us start with the original coordinate system:-

![](/images/Ex2_50_orig_coord.svg)

The dotted outine refers to the area occupied by the frame with a square of length 1.

`(paint einstein)`
![](/images/Ex2_44P1.png)

##### Horizontal flip

A horizontal flip can be done by using the following coordinate system for the frame:-

![](/images/Ex2_50_horiz_flip.svg)

It can be done by defining the following transformation.

{% highlight scheme %}
(define (flip-horizontal painter)
   ((transform-painter (make-vect 1.0 0.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0))
    painter))
{% endhighlight %}

`(paint (flip-horizontal einstein))`
![](/images/Ex2_50P1.png)

##### Rotate counterclockwise by 180 degrees

A rotation by 180 degrees can be done using the following coordinate system:-

![](/images/Ex2_50_180_rotate.svg)

It can be done by defining the following transformation.

{% highlight scheme %}
(define (rotate180 painter)
   ((transform-painter (make-vect 1.0 1.0)
        		       (make-vect 0.0 1.0)
		               (make-vect 1.0 0.0))
    painter))
{% endhighlight %}

`(paint (rotate180 einstein))`
![](/images/Ex2_50P2.png)

##### Rotate counterclockwise by 270 degrees

A rotation by 270 degrees can be done using the following coordinate system:-

![](/images/Ex2_50_270_rotate.svg)

It can be done by defining the following transformation.

{% highlight scheme %}
(define (rotate270 painter)
   ((transform-painter (make-vect 0.0 1.0)
        		       (make-vect 0.0 0.0)
		               (make-vect 1.0 1.0))
    painter))
{% endhighlight %}

`(paint (rotate270 einstein))`
![](/images/Ex2_50P3.png)

### Exercise 2.51<a name="Exercise2_51">&nbsp;</a>

In this exercise, we are tasked with writing an operation called `below` analogous to the `beside` operation given in the book. This procedure takes in two painters and places them one on top of another.

##### Direct method

The operation can be performed directly using the following code.

{% highlight scheme %}
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	        ((transform-painter (make-vect 0.0 0.0)
					            (make-vect 1.0 0.0)
						        split-point) painter1))
	      (paint-top
		    ((transform-painter split-point
						        (make-vect 1.0 0.5)
						        (make-vect 0.0 1.0)) painter2)))
	     (lambda (frame)
	       (paint-bottom frame)
	       (paint-top frame)))))
{% endhighlight %}

`(paint (below einstein einstein))`
![](/images/Ex2_51P1.png)

##### Using `beside` and rotation

We can however, also utilise the given `beside` procedure coupled with rotation to achieve the same effect.

{% highlight scheme %}
(define (below picture1 picture2)
   (rotate90 
    (beside (rotate270 picture1)
     	    (rotate270 picture2))))
{% endhighlight %}

`(paint (below einstein einstein))`
![](/images/Ex2_51P1.png)

### Exercise 2.52<a name="Exercise2_52">&nbsp;</a>

Through this exercise, we look at changing code at each abstraction level.

##### Adding smile

To add smile to the `wave` painter, I took the coordinates again from [Bill the lizard](http://www.billthelizard.com/2012/02/sicp-252-levels-of-language-for-robust.html).

{% highlight scheme %}
(define wave-smile
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
     (make-vect 0.999 0.144))
    (make-segment
     (make-vect 0.395 0.916)
     (make-vect 0.410 0.916))
    (make-segment
     (make-vect 0.376 0.746)
     (make-vect 0.460 0.790)))))
{% endhighlight %}

`(paint wave-smile)`
![](/images/Ex2_52P1.png)

##### Change the `corner-split` pattern

We are tasked with modifying `corner-split` so that we only use `up-split` and `right-split` once. Let us try the original code:-

`(paint (corner-split wave 2))`
![](/images/Ex2_52P2.png)

Now the modified code:-

{% highlight scheme %}
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
		(beside (below painter up)
                (below right corner)))))
{% endhighlight %}

`(paint (corner-split wave 2))`
![](/images/Ex2_52P3.png)

##### Change of `square-limit`

We are told to change the `square-limit` function so that the larger segments are outside. We simply re-order things.

First let us look at the original diagram:-
![](/images/Ex2_52P4.png)

`(paint (square-limit wave 2))`

{% highlight scheme %}
(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-vert
                         rotate180
                         identity 
                         flip-horiz)))
    (combine4 (corner-split painter n))))
{% endhighlight %}

`(paint (square-limit wave 2))`
![](/images/Ex2_52P5.png)
