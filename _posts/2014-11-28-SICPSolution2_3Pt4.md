---
layout: post
category: [SICP, Solutions]
post_no: 25
title: "SICP Section 2.3 Exercise Solutions - Part 4"
submenu:
   - { hook: "Exercise2_66", title: "Exercise 2.66" }
   - { hook: "Exercise2_67", title: "Exercise 2.67" }
   - { hook: "Exercise2_68", title: "Exercise 2.68" }
   - { hook: "Exercise2_69", title: "Exercise 2.69" }
   - { hook: "Exercise2_70", title: "Exercise 2.70" }
   - { hook: "Exercise2_71", title: "Exercise 2.71" }
   - { hook: "Exercise2_72", title: "Exercise 2.72" }
---
### Exercise 2.66<a name="Exercise2_66">&nbsp;</a>

In this exercise, we are told to implement a `lookup` procedure that essentially finds a record from a set of records structured as a binary tree ordered by the numerical value of the keys.

<!--excerpt-->

The following code can be used for that purpose:-

{% highlight scheme %}
(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((= given-key (key (entry tree-of-records)))
         (entry tree-of-records))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        (else
         (lookup given-key (right-branch tree-of-records)))))
{% endhighlight %}

This procedure can retreive a record in $$\Theta(log{n})$$ time complexity.

### Exercise 2.67<a name="Exercise2_67">&nbsp;</a>

This exercise puts the given Huffman decoding procedure through its paces. Thus, its a simple evaluation in the REPL.

{% highlight scheme %}
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
; sample-tree

sample-tree
; ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; sample-message

(decode sample-message sample-tree)
; (a d a b b c a)
{% endhighlight %}

Thus the given binary code is decoded to `(a d a b b c a)`. Looking at the given Huffman tree below, this makes sense.

<center><img src="/images/Ex2_67_Tree.svg" alt="Balanced binary tree" height="200"/></center>

### Exercise 2.68<a name="Exercise2_68">&nbsp;</a>

This exercise deal with the `encode` procedure which takes a message and a Huffman tree and evaluates to the list of bits that gives the encoded message. We are given the initial scaffolding:-

{% highlight scheme %}
(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))
{% endhighlight %}

We are told to give the `encode-symbol` that takes one symbol and returns the bit representation of the symbol.

{% highlight scheme %}
(define (encode-symbol symbol tree)
  (define (contains-symbol? s symbols)
    (if (null? symbols)
        false
        (if (equal? s (car symbols))
            true
            (contains-symbol? s (cdr symbols)))))
  (cond ((leaf? tree) '())
        ((contains-symbol? symbol
                           (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains-symbol? symbol
                           (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol cannot be found" symbol))))
; encode-symbol

sample-message
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

(decode sample-message sample-tree)
; (a d a b b c a)

(encode '(a d a b b c a) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
{% endhighlight %}

As can be seen, the encode function works perfectly in re-encoding the decoded message.

### Exercise 2.69<a name="Exercise2_69">&nbsp;</a>

In this exercise, we are tasked with generating Huffman encoding tree, given a list of symbol-frequency pairs. We are given the following starting code:-

{% highlight scheme %}
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))
{% endhighlight %}

We initially create a set of leaves in ascending order of frequency using `make-leaf-set`. Then, we call `successive-merge` which we need to write.

{% highlight scheme %}
(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (let ((merged (make-code-tree (car pairs) (cadr pairs)))
            (remain (cddr pairs)))
        (successive-merge (adjoin-set merged remain)))))
; successive-merge

(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
; pairs

(generate-huffman-tree pairs)
; ((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)
{% endhighlight %}

### Exercise 2.70<a name="Exercise2_70">&nbsp;</a>

In this exercise, we exercise our new-found compression skills by encoding a song lyrics using Huffman encoding. We are given the frequencies of the most commonly used symbols in 1950s rock songs.

{% highlight scheme %}
(define song-words '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
; song-words

(define word-tree (generate-huffman-tree song-words))
; word-tree
word-tree
; ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)

(define song '(Get a job
               Sha na na na na na na na na
               Get a job
               Sha na na na na na na na na
               Wah yip yip yip yip 
               yip yip yip yip yip
               Sha boom))
; song

(define compressed-song (encode song word-tree))
; compressed-song
compressed-song
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
{% endhighlight %}

We have our final encoded song. If encoded with a fixed-length code, we would require 3-bit to store all $$2^3=8$$ possible words.

{% highlight scheme %}
(length compressed-song)
; 84
(* 3 (length song))
; 108
{% endhighlight %}

As can be seen, with Huffman encoding, we have compressed the song by 24 bits (22% compression).

### Exercise 2.71<a name="Exercise2_71">&nbsp;</a>

In this exercise, we are told to examine the Huffman tree for an alphabet of *n*  symbols with frequencies of $$1,2,4,...,2^{n-1}$$.

{% highlight scheme %}
(define alphabet1 '((a 1) (b 2) (c 4) (d 8) (e 16)))
; alphabet1

(define tree1 (generate-huffman-tree alphabet1))
; tree1
tree1
; (((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31)

(encode-symbol 'a tree1)
; (0 0 0 0)
(encode-symbol 'e tree1)
; (1)

(define alphabet2 '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))
; alphabet2

(define tree2 (generate-huffman-tree alphabet2))
; tree2
tree2
; ((((((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31) (leaf f 32) (a b c d e f) 63) (leaf g 64) (a b c d e f g) 127) (leaf h 128) (a b c d e f g h) 255) (leaf i 256) (a b c d e f g h i) 511) (leaf j 512) (a b c d e f g h i j) 1023)

(encode-symbol 'a tree2)
; (0 0 0 0 0 0 0 0 0)
(encode-symbol 'j tree2)
; (1)
{% endhighlight %}

The following are the Huffman trees that were generated:-

<center>
<img src="/images/Ex2_71_Tree1.svg" alt="Tree of depth 5" height="200"/>
<img src="/images/Ex2_71_Tree2.svg" alt="Tree of depth 10" height="300"/>
</center>

We can see for both cases it takes 1 bit for the most frequent symbol and *n-1* bits to encode the least frequent symbol. Also, the Huffman tree looks similar and has the deepest configuration possible. This is because for any symbol with frequency *x*, the sum of frequencies for all the symbols with lower frequencies is *x-1*. This is due to the rule of taking frequencies as powers of two. 

### Exercise 2.72<a name="Exercise2_72">&nbsp;</a>

In this exercise we are asked to evaluate the order of growth in the number of steps required to evaluate a symbol. Since this is a difficult question to answer for a general Huffman tree, we are asked to give the order of growth for special cases where the Huffman trees look like the ones created in the previous exercise.

Here is the code we use for finding a symbol in a tree:-

{% highlight scheme %}
(define (encode-symbol symbol tree)
  (define (contains-symbol? s symbols)
    (if (null? symbols)
        false
        (if (equal? s (car symbols))
            true
            (contains-symbol? s (cdr symbols)))))
  (cond ((leaf? tree) '())
        ((contains-symbol? symbol
                           (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains-symbol? symbol
                           (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol cannot be found" symbol))))
{% endhighlight %}

In this code, `contains-symbol?` is similar in complexity to `memq`. For a list of *n* elements, it takes $$\Theta(n)$$ time complexity. Moving down to the next branch is a trivial operation that takes a constant time.

Now, when we try to encode the most frequent symbol in the Huffman tree, we simply run `contains-symbol` on two lists totalling *n* in length. The right branch is immediately identified and it being a leaf immediately returns the encoding. Thus, the encoding of the most frequent symbol has a $$\Theta(n)$$ growth.

Finally, considering the symbol with the least frequency, we first run `contains-symbol?` on the first node on a list with *n* entries. Unsuccessful, we go down the left branch and run the process again on a list with *n-1* entries. This goes on until the bottom where we run scans on *2* entries. $$n+(n-1)+...+2=(n+2)(n-1)/2\approx O(n^2)$$. Thus, encoding the letter of lowest frequency has a growth of $$\Theta(n^2)$$.
