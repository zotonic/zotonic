{% with m.rsc[id].o.subject as keywords %}
	{% if keywords %}
		<p class="keywords">
		    Keywords: 
		    {% for id in keywords %}
			    <a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %},{% endif %}
		    {% endfor %}
		</p>
	{% endif %}
{% endwith %}
