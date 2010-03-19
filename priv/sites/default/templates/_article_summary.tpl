<section class="post">
	<h1><a href="{{m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h1>
	{% include "_article_meta.tpl" id=id %}
	<p class="summary">
		{{ m.rsc[id].summary }} <a href="{{ m.rsc[id].page_url }}">Read&nbsp;more&nbsp;&raquo;</a>
	</p>
</section>