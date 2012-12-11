  {{ [[1990,10,10],[10,11,12]]|date:"j F Y - H:i:s" }}
  {{ [1990,10,10]|date:"j F Y" }}
  
{% cache 3600 cat='article' vary=id %}

<div class="prevnext">

	{% for id in m.search[{next cat='article' id=id pagelen=1}] %}
		<div class="next">
			<a class="btn btn-mini" href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{_ Next post _} &raquo;</a>
		</div>
	{% endfor %}

	{% for id in m.search[{previous cat='article' id=id pagelen=1}] %}
		<div class="prev">
			<a class="btn btn-mini" href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">&laquo; {_ Previous post _}</a>
		</div>
	{% endfor %}

</div>
{% endcache %}
