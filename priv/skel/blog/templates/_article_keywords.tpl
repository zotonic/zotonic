{% with id.o.subject as tags %}
{% if tags %}
<p class="keywords">
	{_ Keywords _}:
	{% for id in tags %}
	<a class="label label-info" href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>
	{% endfor %}
</p>
{% endif %}
{% endwith %}
