{% with m.config.seo.keywords.value as keywords %}
	{% with m.config.seo.description.value as description %}
		{% if id %}
			<link rel="shorturl" href="{% url id id=id %}" />
			<link rel="canonical" href="{{ m.rsc[id].page_url }}" />
			{% if m.rsc[id].seo_noindex %}
				<meta name="robots" content="noindex" />
			{% else %}
				{% with m.rsc[id].seo_keywords as seo_keywords %}
					{% if seo_keywords %}
						<meta name="keywords" content="{{ seo_keywords }}, {{ keywords }}" />
					{% else %}
						<meta name="keywords" content="{% for predicate in m.rsc[id].op %}{% ifnotequal predicate "depiction" %}{% for oid in m.rsc[id].o[predicate] %}{{ m.rsc[oid].title }}, {% endfor %}{% endifnotequal %}{% endfor %}{{ keywords }}" />
					{% endif %}
					<meta name="description" content="{{ m.rsc[id].seo_desc|default:m.rsc[id].summary }} {{ description }}" />
				{% endwith %}
			{% endif %}
		{% else %}
			{% if keywords or description %}
				<meta name="keywords" content="{{ keywords }}" />
				<meta name="description" content="{{ description }}" />
			{% endif %}
		{% endif %}
	{% endwith %}
{% endwith %}
