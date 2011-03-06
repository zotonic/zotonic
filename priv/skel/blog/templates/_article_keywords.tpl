{% with m.rsc[id].o.subject as tags %}
	{% if tags %}
		<p class="keywords">
		    {_ Keywords _}: 
		    {% for id in tags %}
			    <a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %},{% endif %}
		    {% endfor %}
		</p>
	{% endif %}
{% endwith %}
