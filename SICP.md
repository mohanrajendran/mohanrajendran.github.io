---
layout: page
title: SICP
---

## Structure And Interpretation of Computer Programs

<center><a href="https://mitpress.mit.edu/sicp/full-text/book/book.html"><img src="/images/SICP_cover.jpg" alt="SICP cover" height="400" /></a></center>

Also known as [SICP](http://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs). It is one of the most famous books in computer science and very essential for any software engineer to go through. In a series of blog posts, I shall be posting my comments and the solutions to the exercises as I work through them. This page curates the links to all the solutions. Online version of this book can be found at <https://mitpress.mit.edu/sicp/full-text/book/book.html>

#### Contents

<ul>
{% for post in site.posts %}
	{% if post.category == "SICP" %}
		<li>
			<a href="{{ post.url }}">{{ post.title }}</a>
		</li>
	{% endif %}
{% endfor %}
