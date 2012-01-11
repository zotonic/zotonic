{% include "_keywords.tpl" %}

{% with m.rsc[id].s.haspart as collections %}
{% if collections %}
{% for c_id in collections %}
<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>

<ul class="item-list">
    {% for p_id in m.rsc[c_id].o.haspart %}
    <li class="list-item">
	{% ifnotequal p_id id %}
	<h3><a href="{{ m.rsc[p_id].page_url }}">{{ m.rsc[p_id].title }}</a></h3>
	{% else %}
	<h3>{{ m.rsc[p_id].title }}</h3>
	{% endifnotequal %}
	{% if m.rsc[p_id].summary %}
	<p class="summary">{{ m.rsc[p_id].summary }}</p>
	{% endif %}
    </li>
    {% endfor %}
</ul>
{% endfor %}
{% else %}
{% endif %}
{% endwith %}

{% include "_documents.tpl" %}
