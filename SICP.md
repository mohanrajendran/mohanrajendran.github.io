---
layout: page
title: SICP
---

## Structure And Interpretation of Computer Programs

<center><a href="https://mitpress.mit.edu/sicp/full-text/book/book.html"><img src="/images/SICP_cover.jpg" alt="SICP cover" height="400" /></a></center>

Also known as [SICP](http://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs). It is one of the most famous books in computer science and very essential for any software engineer to go through. In a series of blog posts, I shall be posting my comments and the solutions to the exercises as I work through them. This page curates the links to all the chapter summary and solutions. Online version of this book can be found at <https://mitpress.mit.edu/sicp/full-text/book/book.html>

To supplement the book, I will also be following the original online lectures by Abelson and Sussman found [here](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/)

#### Contents


<ul>
	{% assign sicp_posts = site.categories.SICP | sort:"post_no" %}
	{% for post in sicp_posts %}
		<li>
			<a href="{{ post.url }}">{{ post.title }}</a>
			{% if post.submenu %}
			<ul>
			{% for item in post.submenu %}
				<li> <a href="{{ post.url }}#{{ item.hook }}">{{ item.title }} </a> </li>
			{% endfor %}
			</ul>
			{% endif %}
		</li>
	{% endfor %}
</ul>
