{% if big %}

	<section class="post clearfix">

		<h1><a href="{{m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></h1>
		{% include "_article_meta.tpl" id=id %}

		{% ifequal m.rsc[id].media[1].mime "text/html-video-embed" %}
			<section class="video-wrapper clearfix">
				{% media m.rsc[id].media[1] %}
			</section>
		{% else %}
			<figure class="image-wrapper block-level-image clearfix">
				<a href="{{m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
					{% media m.rsc[id].media[1] width=445 height=180 crop alt=m.rsc[id].title %}
				</a>
			</figure>
		{% endifequal %}

		<p class="important">
			{{ id|summary:300 }} <a href="{{ m.rsc[id].page_url }}">{_ Read more _}&nbsp;&raquo;</a>
		</p>


	</section>

{% else %}

	<section class="post clearfix">
		<a href="{{m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
			{% image m.rsc[id].media[1] height=108 width=120 crop %}
		</a>
		<h1><a href="{{m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h1>
		{% include "_article_meta.tpl" id=id %}
		<p class="summary">
			{{ m.rsc[id].summary|default:m.rsc[id].body|striptags|truncate:100 }} <a href="{{ m.rsc[id].page_url }}">{_ Read more _}&nbsp;&raquo;</a>
		</p>
	</section>

{% endif %}
