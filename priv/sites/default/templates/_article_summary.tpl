<section class="post clearfix">
	{% image m.rsc[id].media[1] height=108 width=120 crop %}
	<h1><a href="{{m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h1>
	{% include "_article_meta.tpl" id=id %}
	<p class="summary">
		{{ m.rsc[id].summary | truncate:100 }} <a href="{{ m.rsc[id].page_url }}">Read&nbsp;more&nbsp;&raquo;</a>
	</p>
</section>