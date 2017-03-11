---
layout: page
title: Structure and Interpretation of Computer Programs
---

<center><a href="https://mitpress.mit.edu/sicp/full-text/book/book.html"><img src="/images/SICP_cover.jpg" alt="SICP cover" height="400" /></a></center>

Also known as [SICP](http://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs). It is one of the most famous books in computer science and very essential for any software engineer to go through. This page contains links to chapter summary and solutions. Online version of this book can be found at <https://mitpress.mit.edu/sicp/full-text/book/book.html>

To supplement the book, I will also be following the original online lectures by Abelson and Sussman found [here](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/)

#### Contents


<ul>
	{% assign sicp_pages = site.sicp | sort:"order" %}
	{% for page in sicp_pages %}
		<li>
			<a href="{{ page.url }}">{{ page.title }}{% if page.exercises %} [{{ page.exercises }}]{% endif %}</a>
		</li>
	{% endfor %}
</ul>
